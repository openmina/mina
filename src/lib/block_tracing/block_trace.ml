open Core
module Checkpoint = Block_checkpoint

module Entry = struct
  type t =
    { checkpoint : Checkpoint.t
    ; started_at : float
    ; duration : float
    ; metadata : string
    }
  [@@deriving to_yojson]

  let make ?(metadata = "") checkpoint =
    let started_at = Unix.gettimeofday () in
    (* Duration will be adjusted during post-processing *)
    let duration = 0.0 in
    { checkpoint; started_at; duration; metadata }
end

type block_source =
  [ `External | `Internal | `Catchup | `Reconstruct | `Unknown ]
[@@deriving to_yojson]

type status = [ `Pending | `Failure | `Success ] [@@deriving to_yojson]

let block_source_to_yojson = Util.flatten_yojson_variant block_source_to_yojson

let status_to_yojson = Util.flatten_yojson_variant status_to_yojson

(* TODOX: add general metadata *)
type t =
  { source : block_source
  ; blockchain_length : Mina_numbers.Length.t
  ; checkpoints : Entry.t list
  ; status : status
  ; total_time : float
  }
[@@deriving to_yojson]

let empty ?(blockchain_length = Mina_numbers.Length.zero) source =
  { source
  ; blockchain_length
  ; checkpoints = []
  ; status = `Pending
  ; total_time = 0.0
  }

let to_yojson t = to_yojson { t with checkpoints = List.rev t.checkpoints }

let push ~status ~source ?blockchain_length entry trace =
  match trace with
  | None ->
      let trace = empty ?blockchain_length source in
      { trace with checkpoints = [ entry ]; status }
  | Some ({ checkpoints = []; _ } as trace) ->
      { trace with checkpoints = [ entry ]; status }
  | Some ({ checkpoints = previous :: rest; _ } as trace) ->
      let previous =
        { previous with duration = entry.started_at -. previous.started_at }
      in
      (* TODOX: this will be incorrect if the block is processed twice
         because it gets received twice *)
      let total_time = trace.total_time +. previous.duration in
      { trace with checkpoints = entry :: previous :: rest; status; total_time }
