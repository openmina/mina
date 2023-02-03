open Core_kernel
open Async_kernel

let coordinator_url =
  Sys.getenv_opt "MINA_SNARK_COORDINATOR_URL"
  |> Option.value ~default:"http://localhost:8080"

let query_coordinator identifier =
  let url =
    Uri.of_string
    @@ String.concat ~sep:"/" [ coordinator_url; "lock-job"; identifier ]
  in
  let%map.Deferred response, _body = Cohttp_async.Client.put url in
  let status_code = Cohttp.Response.status response in
  (* status equals 200 when job already exists, 201 when it was created and locked *)
  201 = Cohttp.Code.code_of_status status_code

module Make
    (Inputs : Intf.Inputs_intf)
    (Lib : Intf.Lib_intf with module Inputs := Inputs) =
struct
  let spec_hashes spec =
    let statement = Snark_work_lib.Work.Single.Spec.statement spec in
    let source =
      Mina_base.Frozen_ledger_hash.to_base58_check
        statement.source.first_pass_ledger
    in
    let target =
      Mina_base.Frozen_ledger_hash.to_base58_check
        statement.target.second_pass_ledger
    in
    source ^ ":" ^ target

  let work_identifier work =
    match One_or_two.to_list work with
    | [ single ] ->
        spec_hashes single
    | [ first; second ] ->
        spec_hashes first ^ "::" ^ spec_hashes second
    | _ ->
        assert false

  let rec get_next_coordinated_job = function
    | [] ->
        return (None, [])
    | work :: rest ->
        let%bind locked = query_coordinator (work_identifier work) in
        if locked then return (Some work, rest)
        else get_next_coordinated_job rest

  let work_uncached ~snark_pool ~fee ~logger (state : Lib.State.t) =
    Lib.State.remove_old_assignments state ~logger ;
    let unseen_jobs = Lib.State.all_unseen_works state in
    match%bind.Deferred Lib.get_expensive_work ~snark_pool ~fee unseen_jobs with
    | [] ->
        return (None, [])
    | expensive_work ->
        let%map result, remaining = get_next_coordinated_job expensive_work in
        result
        |> Option.map ~f:(fun x -> Lib.State.set state x ; (Some x, remaining))
        |> Option.value ~default:(None, remaining)

  let work_cached ~expensive_work ~logger (state : Lib.State.t) =
    Lib.State.remove_old_assignments state ~logger ;
    match expensive_work with
    | [] ->
        return (None, [])
    | expensive_work ->
        let%map result, remaining = get_next_coordinated_job expensive_work in
        result
        |> Option.map ~f:(fun x -> Lib.State.set state x ; (Some x, remaining))
        |> Option.value ~default:(None, remaining)

  let work =
    let result_cache = ref [] in
    let cached_at = ref 0.0 in
    fun ~snark_pool ~fee ~logger (state : Lib.State.t) ->
      let now = Core.Unix.gettimeofday () in
      (* only recompute list every few seconds to avoid stalling the scheduler *)
      if Float.(now - !cached_at > 6.0) then (
        cached_at := now ;
        let%map work_opt, remaining_expensive_work =
          work_uncached ~snark_pool ~fee ~logger state
        in
        result_cache := remaining_expensive_work ;
        work_opt )
      else
        let expensive_work = !result_cache in
        let%map work_opt, remaining_expensive_work =
          work_cached ~expensive_work ~logger state
        in
        result_cache := remaining_expensive_work ;
        work_opt

  let remove = Lib.State.remove

  let pending_work_statements = Lib.pending_work_statements
end
