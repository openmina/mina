(** Pickles implementation *)

(** See documentation of the {!Mina_wire_types} library *)
module Wire_types = Mina_wire_types.Pickles

module Make_sig (A : Wire_types.Types.S) = struct
  module type S =
    Pickles_intf.S
      with type Side_loaded.Verification_key.Stable.V2.t =
        A.Side_loaded.Verification_key.V2.t
       and type ('a, 'b) Proof.t = ('a, 'b) A.Proof.t
end

module Make_str (_ : Wire_types.Concrete) = struct
  module Endo = Endo
  module P = Proof

  module type Statement_intf = Intf.Statement

  module type Statement_var_intf = Intf.Statement_var

  module type Statement_value_intf = Intf.Statement_value

  module Common = Common
  open Tuple_lib
  module Scalar_challenge = Scalar_challenge
  module SC = Scalar_challenge
  open Core_kernel
  open Async_kernel
  open Import
  open Pickles_types
  open Poly_types
  open Hlist
  open Common
  open Backend
  module Backend = Backend
  module Sponge_inputs = Sponge_inputs
  module Util = Util
  module Tick_field_sponge = Tick_field_sponge
  module Impls = Impls
  module Inductive_rule = Inductive_rule
  module Tag = Tag
  module Types_map = Types_map
  module Dirty = Dirty
  module Cache_handle = Cache_handle
  module Step_main_inputs = Step_main_inputs
  module Step_verifier = Step_verifier

  exception Return_digest of Md5.t

  let profile_constraints = false

  let verify_promise = Verify.verify

  let verify max_proofs_verified statement key proofs =
    verify_promise max_proofs_verified statement key proofs

  (* This file (as you can see from the mli) defines a compiler which turns an inductive
     definition of a set into an inductive SNARK system for proving using those rules.

     The two ingredients we use are two SNARKs.
     - A step based SNARK for a field Fp, using the group G1/Fq (whose scalar field is Fp)
     - A DLOG based SNARK for a field Fq, using the group G/Fp (whose scalar field is Fq)

     For convenience in this discussion, let's define
      (F_0, G_0) := (Fp, G1)
      (F_1, G_1) := (Fq, G)
     So ScalarField(G_i) = F_i and G_i / F_{1-i}.

     An inductive set A is defined by a sequence of inductive rules.
     An inductive rule is intuitively described by something of the form

     a1 ∈ A1, ..., an ∈ An
       f [ a0, ... a1 ] a
     ----------------------
             a ∈ A

     where f is a snarky function defined over an Impl with Field.t = Fp
     and each Ai is itself an inductive rule (possibly equal to A itself).

     We pursue the "step" then "wrap" approach for proof composition.

     The main source of complexity is that we must "wrap" proofs whose verifiers are
     slightly different.

     The main sources of complexity are twofold:
     1. Each SNARK verifier includes group operations and scalar field operations.
        This is problematic because the group operations use the base field, which is
        not equal to the scalar field.

        Schematically, from the circuit point-of-view, we can say a proof is
     - a sequence of F_0 elements xs_0
     - a sequence of F_1 elelements xs_1
        and a verifier is a pair of "snarky functions"
     - check_0 : F_0 list -> F_1 list -> unit which uses the Impl with Field.t = F_0
     - check_1 : F_0 list -> F_1 list -> unit which uses the Impl with Field.t = F_1
     - subset_00 : 'a list -> 'a list
     - subset_01 : 'a list -> 'a list
     - subset_10 : 'a list -> 'a list
     - subset_11 : 'a list -> 'a list
        and a proof verifies if
        ( check_0 (subset_00 xs_0) (subset_01 xs_1)  ;
          check_1 (subset_10 xs_0) (subset_11 xs_1) )

        When verifying a proof, we perform the parts of the verifier involving group operations
        and expose as public input the scalar-field elements we need to perform the final checks.

        In the F_0 circuit, we witness xs_0 and xs_1,
        execute `check_0 (subset_00 xs_0) (subset_01 xs_1)` and
        expose `subset_10 xs_0` and `subset_11 xs_1` as public inputs.

        So the "public inputs" contain within them an "unfinalized proof".

        Then, the next time we verify that proof within an F_1 circuit we "finalize" those
        unfinalized proofs by running `check_1 xs_0_subset xs_1_subset`.

        I didn't implement it exactly this way (although in retrospect probably I should have) but
        that's the basic idea.

        **The complexity this causes:**
        When you prove a rule that includes k recursive verifications, you expose k unfinalized
        proofs. So, the shape of a statement depends on how many "predecessor statements" it has
        or in other words, how many verifications were performed within it.

        Say we have an inductive set given by inductive rules R_1, ... R_n such that
        each rule R_i has k_i predecessor statements.

        In the "wrap" circuit, we must be able to verify a proof coming from any of the R_i.
        So, we must pad the statement for the proof we're wrapping to have `max_i k_i`
        unfinalized proof components.

     2. The verifier for each R_i looks a little different depending on the complexity of the "step"
        circuit corresponding to R_i has. Namely, it is dependent on the "domains" H and K for this
        circuit.

        So, when the "wrap" circuit proves the statement,
        "there exists some index i in 1,...,n and a proof P such that verifies(P)"
        "verifies(P)" must also take the index "i", compute the correct domain sizes correspond to rule "i"
        and use *that* in the "verifies" computation.
  *)

  let pad_local_max_proofs_verifieds
      (type prev_varss prev_valuess env max_proofs_verified branches)
      (max_proofs_verified : max_proofs_verified Nat.t)
      (length : (prev_varss, branches) Hlist.Length.t)
      (local_max_proofs_verifieds :
        (prev_varss, prev_valuess, env) H2_1.T(H2_1.T(E03(Int))).t ) :
      ((int, max_proofs_verified) Vector.t, branches) Vector.t =
    let module Vec = struct
      type t = (int, max_proofs_verified) Vector.t
    end in
    let module M =
      H2_1.Map
        (H2_1.T
           (E03 (Int))) (E03 (Vec))
           (struct
             module HI = H2_1.T (E03 (Int))

             let f : type a b e. (a, b, e) H2_1.T(E03(Int)).t -> Vec.t =
              fun xs ->
               let (T (_proofs_verified, pi)) = HI.length xs in
               let module V = H2_1.To_vector (Int) in
               let v = V.f pi xs in
               Vector.extend_exn v max_proofs_verified 0
           end)
    in
    let module V = H2_1.To_vector (Vec) in
    V.f length (M.f local_max_proofs_verifieds)

  open Kimchi_backend

  module Messages_for_next_proof_over_same_field = struct
    module Wrap = Types.Wrap.Proof_state.Messages_for_next_wrap_proof
    module Step = Types.Step.Proof_state.Messages_for_next_step_proof
  end

  module Proof_ = P.Base
  module Proof = P

  module Statement_with_proof = struct
    type ('s, 'max_width, _) t =
      (* TODO: use Max local max proofs verified instead of max_width *)
      ('max_width, 'max_width) Proof.t
  end

  let pad_messages_for_next_wrap_proof
      (type local_max_proofs_verifieds max_local_max_proofs_verifieds
      max_proofs_verified )
      (module M : Hlist.Maxes.S
        with type ns = max_local_max_proofs_verifieds
         and type length = max_proofs_verified )
      (messages_for_next_wrap_proofs :
        local_max_proofs_verifieds
        H1.T(Proof_.Messages_for_next_proof_over_same_field.Wrap).t ) =
    let dummy_chals = Dummy.Ipa.Wrap.challenges in
    let rec go :
        type len ms ns.
           ms H1.T(Nat).t
        -> ns H1.T(Proof_.Messages_for_next_proof_over_same_field.Wrap).t
        -> ms H1.T(Proof_.Messages_for_next_proof_over_same_field.Wrap).t =
     fun maxes messages_for_next_wrap_proofs ->
      match (maxes, messages_for_next_wrap_proofs) with
      | [], _ :: _ ->
          assert false
      | [], [] ->
          []
      | m :: maxes, [] ->
          { challenge_polynomial_commitment = Lazy.force Dummy.Ipa.Step.sg
          ; old_bulletproof_challenges = Vector.init m ~f:(fun _ -> dummy_chals)
          }
          :: go maxes []
      | ( m :: maxes
        , messages_for_next_wrap_proof :: messages_for_next_wrap_proofs ) ->
          let messages_for_next_wrap_proof =
            { messages_for_next_wrap_proof with
              old_bulletproof_challenges =
                Vector.extend_exn
                  messages_for_next_wrap_proof.old_bulletproof_challenges m
                  dummy_chals
            }
          in
          messages_for_next_wrap_proof :: go maxes messages_for_next_wrap_proofs
    in
    go M.maxes messages_for_next_wrap_proofs

  module Verification_key = struct
    include Verification_key

    module Id = struct
      include Cache.Wrap.Key.Verification

      let dummy_id = Type_equal.Id.(uid (create ~name:"dummy" sexp_of_opaque))

      let dummy : unit -> t =
        let header =
          { Snark_keys_header.header_version = Snark_keys_header.header_version
          ; kind = { type_ = "verification key"; identifier = "dummy" }
          ; constraint_constants =
              { sub_windows_per_window = 0
              ; ledger_depth = 0
              ; work_delay = 0
              ; block_window_duration_ms = 0
              ; transaction_capacity = Log_2 0
              ; pending_coinbase_depth = 0
              ; coinbase_amount = Unsigned.UInt64.of_int 0
              ; supercharged_coinbase_factor = 0
              ; account_creation_fee = Unsigned.UInt64.of_int 0
              ; fork = None
              }
          ; commits = { mina = ""; marlin = "" }
          ; length = 0
          ; commit_date = ""
          ; constraint_system_hash = ""
          ; identifying_hash = ""
          }
        in
        let t = lazy (dummy_id, header, Md5.digest_string "") in
        fun () -> Lazy.force t
    end

    (* TODO: Make async *)
    let load ~cache id =
      Key_cache.Sync.read cache
        (Key_cache.Sync.Disk_storable.of_binable Id.to_string
           (module Verification_key.Stable.Latest) )
        id
      |> Deferred.return
  end

  module type Proof_intf = sig
    type statement

    type t

    val verification_key : Verification_key.t Lazy.t

    val id : Verification_key.Id.t Lazy.t

    val verify : (statement * t) list -> bool

    val verify_promise : (statement * t) list -> bool
  end

  module Prover = struct
    type ('prev_values, 'local_widths, 'local_heights, 'a_value, 'proof) t =
         ?handler:
           (   Snarky_backendless.Request.request
            -> Snarky_backendless.Request.response )
      -> 'a_value
      -> 'proof
  end

  module Make
      (Arg_var : Statement_var_intf)
      (Arg_value : Statement_value_intf)
      (Ret_var : T0)
      (Ret_value : T0)
      (Auxiliary_var : T0)
      (Auxiliary_value : T0) =
  struct
    module IR =
      Inductive_rule.T (Arg_var) (Arg_value) (Ret_var) (Ret_value)
        (Auxiliary_var)
        (Auxiliary_value)
    module HIR = H4.T (IR)

    let max_local_max_proofs_verifieds ~self (type n)
        (module Max_proofs_verified : Nat.Intf with type n = n) branches choices
        =
      let module Local_max_proofs_verifieds = struct
        type t = (int, Max_proofs_verified.n) Vector.t
      end in
      let module M =
        H4.Map (IR) (E04 (Local_max_proofs_verifieds))
          (struct
            module V = H4.To_vector (Int)
            module HT = H4.T (Tag)

            module M =
              H4.Map (Tag) (E04 (Int))
                (struct
                  let f (type a b c d) (t : (a, b, c, d) Tag.t) : int =
                    if Type_equal.Id.same t.id self then
                      Nat.to_int Max_proofs_verified.n
                    else
                      let (module M) = Types_map.max_proofs_verified t in
                      Nat.to_int M.n
                end)

            let f :
                type a b c d. (a, b, c, d) IR.t -> Local_max_proofs_verifieds.t
                =
             fun rule ->
              let (T (_, l)) = HT.length rule.prevs in
              Vector.extend_exn (V.f l (M.f rule.prevs)) Max_proofs_verified.n 0
          end)
      in
      let module V = H4.To_vector (Local_max_proofs_verifieds) in
      let padded = V.f branches (M.f choices) |> Vector.transpose in
      (padded, Maxes.m padded)

    module Lazy_ (A : T0) = struct
      type t = A.t Lazy.t
    end

    module Lazy_keys = struct
      type t =
        (Impls.Step.Keypair.t * Dirty.t) Lazy.t
        * (Kimchi_bindings.Protocol.VerifierIndex.Fp.t * Dirty.t) Lazy.t

      (* TODO Think this is right.. *)
    end

    let log_step main typ name index =
      let module Constraints = Snarky_log.Constraints (Impls.Step.Internal_Basic) in
      let log =
        let weight =
          let sys = Backend.Tick.R1CS_constraint_system.create () in
          fun ({ annotation; basic } : Impls.Step.Constraint.t) ->
            let prev =
              Kimchi_pasta_constraint_system.Vesta_constraint_system.next_row
                sys
            in
            Backend.Tick.R1CS_constraint_system.add_constraint sys
              ?label:annotation basic ;
            let next =
              Kimchi_pasta_constraint_system.Vesta_constraint_system.next_row
                sys
            in
            next - prev
        in
        Constraints.log ~weight (fun () -> Impls.Step.make_checked main)
      in
      if profile_constraints then
        Snarky_log.to_file (sprintf "step-snark-%s-%d.json" name index) log

    let log_wrap main typ name id =
      let module Constraints = Snarky_log.Constraints (Impls.Wrap.Internal_Basic) in
      let log =
        let sys = Backend.Tock.R1CS_constraint_system.create () in
        let weight ({ annotation; basic } : Impls.Wrap.Constraint.t) =
          let prev =
            Kimchi_pasta_constraint_system.Pallas_constraint_system.next_row sys
          in
          Backend.Tock.R1CS_constraint_system.add_constraint sys
            ?label:annotation basic ;
          let next =
            Kimchi_pasta_constraint_system.Pallas_constraint_system.next_row sys
          in
          next - prev
        in
        let log =
          Constraints.log ~weight
            Impls.Wrap.(
              fun () ->
                make_checked (fun () : unit ->
                    let x = with_label __LOC__ (fun () -> exists typ) in
                    main x () ))
        in
        log
      in
      if profile_constraints then
        Snarky_log.to_file
          (sprintf
             !"wrap-%s-%{sexp:Type_equal.Id.Uid.t}.json"
             name (Type_equal.Id.uid id) )
          log

    let compile :
        type var value prev_varss prev_valuess widthss heightss max_proofs_verified branches.
           self:(var, value, max_proofs_verified, branches) Tag.t
        -> cache:Key_cache.Spec.t list
        -> ?disk_keys:
             (Cache.Step.Key.Verification.t, branches) Vector.t
             * Cache.Wrap.Key.Verification.t
        -> ?return_early_digest_exception:bool
        -> branches:(module Nat.Intf with type n = branches)
        -> max_proofs_verified:
             (module Nat.Add.Intf with type n = max_proofs_verified)
        -> name:string
        -> constraint_constants:Snark_keys_header.Constraint_constants.t
        -> public_input:
             ( var
             , value
             , Arg_var.t
             , Arg_value.t
             , Ret_var.t
             , Ret_value.t )
             Inductive_rule.public_input
        -> auxiliary_typ:(Auxiliary_var.t, Auxiliary_value.t) Impls.Step.Typ.t
        -> choices:
             (   self:(var, value, max_proofs_verified, branches) Tag.t
              -> (prev_varss, prev_valuess, widthss, heightss) H4.T(IR).t )
        -> unit
        -> ( prev_valuess
           , widthss
           , heightss
           , Arg_value.t
           , ( Ret_value.t
             * Auxiliary_value.t
             * (max_proofs_verified, max_proofs_verified) Proof.t )
             Promise.t )
           H3_2.T(Prover).t
           * _
           * _
           * _ =
     fun ~self ~cache ?disk_keys ?(return_early_digest_exception = false)
         ~branches:(module Branches) ~max_proofs_verified ~name
         ~constraint_constants ~public_input ~auxiliary_typ ~choices () ->
      let snark_keys_header kind constraint_system_hash =
        { Snark_keys_header.header_version = Snark_keys_header.header_version
        ; kind
        ; constraint_constants
        ; commits =
            { mina = Mina_version.commit_id
            ; marlin = Mina_version.marlin_commit_id
            }
        ; length = (* This is a dummy, it gets filled in on read/write. *) 0
        ; commit_date = Mina_version.commit_date
        ; constraint_system_hash
        ; identifying_hash =
            (* TODO: Proper identifying hash. *)
            constraint_system_hash
        }
      in
      Timer.start __LOC__ ;
      let module Max_proofs_verified = ( val max_proofs_verified : Nat.Add.Intf
                                           with type n = max_proofs_verified )
      in
      let T = Max_proofs_verified.eq in
      let choices = choices ~self in
      let (T (prev_varss_n, prev_varss_length)) = HIR.length choices in
      let T = Nat.eq_exn prev_varss_n Branches.n in
      let padded, (module Maxes) =
        max_local_max_proofs_verifieds
          ( module struct
            include Max_proofs_verified
          end )
          prev_varss_length choices ~self:self.id
      in
      let full_signature = { Full_signature.padded; maxes = (module Maxes) } in
      Timer.clock __LOC__ ;
      let wrap_domains =
        let module M =
          Wrap_domains.Make (Arg_var) (Arg_value) (Ret_var) (Ret_value)
            (Auxiliary_var)
            (Auxiliary_value)
        in
        let rec f :
            type a b c d. (a, b, c, d) H4.T(IR).t -> (a, b, c, d) H4.T(M.I).t =
          function
          | [] ->
              []
          | x :: xs ->
              x :: f xs
        in
        M.f full_signature prev_varss_n prev_varss_length ~self
          ~choices:(f choices) ~max_proofs_verified
      in
      Timer.clock __LOC__ ;
      let module Branch_data = struct
        type ('vars, 'vals, 'n, 'm) t =
          ( Arg_var.t
          , Arg_value.t
          , Ret_var.t
          , Ret_value.t
          , Auxiliary_var.t
          , Auxiliary_value.t
          , Max_proofs_verified.n
          , Branches.n
          , 'vars
          , 'vals
          , 'n
          , 'm )
          Step_branch_data.t
      end in
      let proofs_verifieds =
        let module M =
          H4.Map (IR) (E04 (Int))
            (struct
              module M = H4.T (Tag)

              let f : type a b c d. (a, b, c, d) IR.t -> int =
               fun r ->
                let (T (n, _)) = M.length r.prevs in
                Nat.to_int n
            end)
        in
        let module V = H4.To_vector (Int) in
        V.f prev_varss_length (M.f choices)
      in
      let step_uses_lookup =
        let rec go :
            type a b c d. (a, b, c, d) H4.T(IR).t -> Plonk_types.Opt.Flag.t =
         fun rules ->
          match rules with
          | [] ->
              No
          | r :: rules -> (
              let rest_usage = go rules in
              match (r.uses_lookup, rest_usage) with
              | true, Yes ->
                  Yes
              | false, No ->
                  No
              | _, Maybe | true, No | false, Yes ->
                  Maybe )
        in
        go choices
      in
      let step_data =
        let i = ref 0 in
        Timer.clock __LOC__ ;
        let rec f :
            type a b c d.
            (a, b, c, d) H4.T(IR).t -> (a, b, c, d) H4.T(Branch_data).t =
          function
          | [] ->
              []
          | rule :: rules ->
              let first =
                Timer.clock __LOC__ ;
                let res =
                  Common.time "make step data" (fun () ->
                      Step_branch_data.create ~index:!i ~step_uses_lookup
                        ~max_proofs_verified:Max_proofs_verified.n
                        ~branches:Branches.n ~self ~public_input ~auxiliary_typ
                        Arg_var.to_field_elements Arg_value.to_field_elements
                        rule ~wrap_domains ~proofs_verifieds )
                in
                Timer.clock __LOC__ ; incr i ; res
              in
              first :: f rules
        in
        f choices
      in
      Timer.clock __LOC__ ;
      let step_domains =
        let module M =
          H4.Map (Branch_data) (E04 (Domains))
            (struct
              let f (T b : _ Branch_data.t) = b.domains
            end)
        in
        let module V = H4.To_vector (Domains) in
        V.f prev_varss_length (M.f step_data)
      in
      let cache_handle = ref (Lazy.return `Cache_hit) in
      let accum_dirty t = cache_handle := Cache_handle.(!cache_handle + t) in
      Timer.clock __LOC__ ;
      let step_keypairs =
        let disk_keys =
          Option.map disk_keys ~f:(fun (xs, _) -> Vector.to_array xs)
        in
        let module M =
          H4.Map (Branch_data) (E04 (Lazy_keys))
            (struct
              let etyp =
                Impls.Step.input ~proofs_verified:Max_proofs_verified.n
                  ~wrap_rounds:Tock.Rounds.n ~uses_lookup:Maybe
              (* TODO *)

              let f (T b : _ Branch_data.t) =
                let (T (typ, _conv, conv_inv)) = etyp in
                let main () () =
                  let res = b.main ~step_domains () in
                  Impls.Step.with_label "conv_inv" (fun () -> conv_inv res)
                in
                let () = if true then log_step main typ name b.index in
                let open Impls.Step in
                (* HACK: TODO docs *)
                if return_early_digest_exception then
                  raise
                    (Return_digest
                       ( constraint_system ~input_typ:Typ.unit ~return_typ:typ
                           main
                       |> R1CS_constraint_system.digest ) ) ;

                let k_p =
                  lazy
                    (let cs =
                       constraint_system ~input_typ:Typ.unit ~return_typ:typ
                         main
                     in
                     let cs_hash =
                       Md5.to_hex (R1CS_constraint_system.digest cs)
                     in
                     ( Type_equal.Id.uid self.id
                     , snark_keys_header
                         { type_ = "step-proving-key"
                         ; identifier = name ^ "-" ^ b.rule.identifier
                         }
                         cs_hash
                     , b.index
                     , cs ) )
                in
                let k_v =
                  match disk_keys with
                  | Some ks ->
                      Lazy.return ks.(b.index)
                  | None ->
                      lazy
                        (let id, _header, index, cs = Lazy.force k_p in
                         let digest = R1CS_constraint_system.digest cs in
                         ( id
                         , snark_keys_header
                             { type_ = "step-verification-key"
                             ; identifier = name ^ "-" ^ b.rule.identifier
                             }
                             (Md5.to_hex digest)
                         , index
                         , digest ) )
                in
                let ((pk, vk) as res) =
                  Common.time "step read or generate" (fun () ->
                      Cache.Step.read_or_generate
                        ~prev_challenges:(Nat.to_int (fst b.proofs_verified))
                        cache k_p k_v
                        (Snarky_backendless.Typ.unit ())
                        typ main )
                in
                accum_dirty (Lazy.map pk ~f:snd) ;
                accum_dirty (Lazy.map vk ~f:snd) ;
                res
            end)
        in
        M.f step_data
      in
      Timer.clock __LOC__ ;
      let step_vks =
        let module V = H4.To_vector (Lazy_keys) in
        lazy
          (Vector.map (V.f prev_varss_length step_keypairs) ~f:(fun (_, vk) ->
               Tick.Keypair.vk_commitments (fst (Lazy.force vk)) ) )
      in
      Timer.clock __LOC__ ;
      let wrap_requests, wrap_main =
        Wrap_main.wrap_main full_signature prev_varss_length step_vks
          proofs_verifieds step_domains max_proofs_verified
      in
      Timer.clock __LOC__ ;
      let (wrap_pk, wrap_vk), disk_key =
        let open Impls.Wrap in
        let (T (typ, conv, _conv_inv)) = input () in
        Timer.clock __LOC__ ;
        let main x () : unit = wrap_main (conv x) in
        Timer.clock __LOC__ ;
        let () = if false then log_wrap main typ name self.id in
        Timer.clock __LOC__ ;
        let self_id = Type_equal.Id.uid self.id in
        let disk_key_prover =
          lazy
            (let cs =
               constraint_system ~input_typ:typ
                 ~return_typ:(Snarky_backendless.Typ.unit ())
                 main
             in
             let cs_hash = Md5.to_hex (R1CS_constraint_system.digest cs) in
             ( self_id
             , snark_keys_header
                 { type_ = "wrap-proving-key"; identifier = name }
                 cs_hash
             , cs ) )
        in
        Timer.clock __LOC__ ;
        let disk_key_verifier =
          match disk_keys with
          | None ->
              lazy
                (let id, _header, cs = Lazy.force disk_key_prover in
                 let digest = R1CS_constraint_system.digest cs in
                 ( id
                 , snark_keys_header
                     { type_ = "wrap-verification-key"; identifier = name }
                     (Md5.to_hex digest)
                 , digest ) )
          | Some (_, (_id, header, digest)) ->
              Lazy.return (self_id, header, digest)
        in
        Timer.clock __LOC__ ;
        let r =
          Common.time "wrap read or generate " (fun () ->
              Cache.Wrap.read_or_generate (* Due to Wrap_hack *)
                ~prev_challenges:2 cache disk_key_prover disk_key_verifier typ
                (Snarky_backendless.Typ.unit ())
                main )
        in
        (r, disk_key_verifier)
      in
      Timer.clock __LOC__ ;
      accum_dirty (Lazy.map wrap_pk ~f:snd) ;
      accum_dirty (Lazy.map wrap_vk ~f:snd) ;
      let wrap_vk = Lazy.map wrap_vk ~f:fst in
      let module S =
        Step.Make (Arg_var) (Arg_value)
          (struct
            include Max_proofs_verified
          end)
      in
      let (typ : (var, value) Impls.Step.Typ.t) =
        match public_input with
        | Input typ ->
            typ
        | Output typ ->
            typ
        | Input_and_output (input_typ, output_typ) ->
            Impls.Step.Typ.(input_typ * output_typ)
      in
      Timer.clock __LOC__ ;
      let provers =
        let module Z = H4.Zip (Branch_data) (E04 (Impls.Step.Keypair)) in
        let f :
            type prev_vars prev_values local_widths local_heights.
               ( prev_vars
               , prev_values
               , local_widths
               , local_heights )
               Branch_data.t
            -> Lazy_keys.t
            -> ?handler:
                 (   Snarky_backendless.Request.request
                  -> Snarky_backendless.Request.response )
            -> Arg_value.t
            -> ( Ret_value.t
               * Auxiliary_value.t
               * (Max_proofs_verified.n, Max_proofs_verified.n) Proof.t )
               Promise.t =
         fun (T b as branch_data) (step_pk, step_vk) ->
          let (module Requests) = b.requests in
          let _, prev_vars_length = b.proofs_verified in
          let step handler next_state =
            let wrap_vk = Lazy.force wrap_vk in
            S.f ?handler branch_data next_state ~prevs_length:prev_vars_length
              ~self ~step_domains ~self_dlog_plonk_index:wrap_vk.commitments
              ~public_input ~auxiliary_typ
              ~uses_lookup:(if b.rule.uses_lookup then Yes else No)
              (Impls.Step.Keypair.pk (fst (Lazy.force step_pk)))
              wrap_vk.index
          in
          let step_vk = fst (Lazy.force step_vk) in
          let wrap ?handler next_state =
            let wrap_vk = Lazy.force wrap_vk in
            let%bind.Promise ( proof
                             , return_value
                             , auxiliary_value
                             , actual_wrap_domains ) =
              step handler ~maxes:(module Maxes) next_state
            in
            let proof =
              { proof with
                statement =
                  { proof.statement with
                    messages_for_next_wrap_proof =
                      pad_messages_for_next_wrap_proof
                        (module Maxes)
                        proof.statement.messages_for_next_wrap_proof
                  }
              }
            in
            let%map.Promise proof =
              Wrap.wrap ~max_proofs_verified:Max_proofs_verified.n
                full_signature.maxes wrap_requests
                ~dlog_plonk_index:wrap_vk.commitments wrap_main ~typ ~step_vk
                ~step_plonk_indices:(Lazy.force step_vks) ~actual_wrap_domains
                (Impls.Wrap.Keypair.pk (fst (Lazy.force wrap_pk)))
                proof
            in
            ( return_value
            , auxiliary_value
            , Proof.T
                { proof with
                  statement =
                    { proof.statement with
                      messages_for_next_step_proof =
                        { proof.statement.messages_for_next_step_proof with
                          app_state = ()
                        }
                    }
                } )
          in
          wrap
        in
        let rec go :
            type xs1 xs2 xs3 xs4 xs5 xs6.
               (xs1, xs2, xs3, xs4) H4.T(Branch_data).t
            -> (xs1, xs2, xs3, xs4) H4.T(E04(Lazy_keys)).t
            -> ( xs2
               , xs3
               , xs4
               , Arg_value.t
               , ( Ret_value.t
                 * Auxiliary_value.t
                 * (max_proofs_verified, max_proofs_verified) Proof.t )
                 Promise.t )
               H3_2.T(Prover).t =
         fun bs ks ->
          match (bs, ks) with
          | [], [] ->
              []
          | b :: bs, k :: ks ->
              f b k :: go bs ks
        in
        go step_data step_keypairs
      in
      Timer.clock __LOC__ ;
      let data : _ Types_map.Compiled.t =
        { branches = Branches.n
        ; proofs_verifieds
        ; max_proofs_verified
        ; public_input = typ
        ; wrap_key = Lazy.map wrap_vk ~f:Verification_key.commitments
        ; wrap_vk = Lazy.map wrap_vk ~f:Verification_key.index
        ; wrap_domains
        ; step_domains
        ; step_uses_lookup
        }
      in
      Timer.clock __LOC__ ;
      Types_map.add_exn self data ;
      (provers, wrap_vk, disk_key, !cache_handle)
  end

  module Side_loaded = struct
    module V = Verification_key

    module Verification_key = struct
      include Side_loaded_verification_key

      let to_input (t : t) =
        to_input ~field_of_int:Impls.Step.Field.Constant.of_int t

      let of_compiled tag : t =
        let d = Types_map.lookup_compiled tag.Tag.id in
        { wrap_vk = Some (Lazy.force d.wrap_vk)
        ; wrap_index = Lazy.force d.wrap_key
        ; max_proofs_verified =
            Pickles_base.Proofs_verified.of_nat
              (Nat.Add.n d.max_proofs_verified)
        }

      module Max_width = Width.Max
    end

    let in_circuit tag vk =
      Types_map.set_ephemeral tag { index = `In_circuit vk }

    let in_prover tag vk = Types_map.set_ephemeral tag { index = `In_prover vk }

    let create ~name ~max_proofs_verified ~uses_lookup ~typ =
      Types_map.add_side_loaded ~name
        { max_proofs_verified
        ; public_input = typ
        ; branches = Verification_key.Max_branches.n
        ; step_uses_lookup = uses_lookup
        }

    module Proof = struct
      include Proof.Proofs_verified_max

      let of_proof : _ Proof.t -> t = Wrap_hack.pad_proof
    end

    let verify_promise (type t) ~(typ : (_, t) Impls.Step.Typ.t)
        (ts : (Verification_key.t * t * Proof.t) list) =
      let m =
        ( module struct
          type nonrec t = t

          let to_field_elements =
            let (Typ typ) = typ in
            fun x -> fst (typ.value_to_fields x)
        end : Intf.Statement_value
          with type t = t )
      in
      (* TODO: This should be the actual max width on a per proof basis *)
      let max_proofs_verified =
        (module Verification_key.Max_width : Nat.Intf
          with type n = Verification_key.Max_width.n )
      in
      with_return (fun { return } ->
          List.map ts ~f:(fun (vk, x, p) ->
              let vk : V.t =
                { commitments = vk.wrap_index
                ; index =
                    (match vk.wrap_vk with None -> return false | Some x -> x)
                ; data =
                    (* This isn't used in verify_heterogeneous, so we can leave this dummy *)
                    { constraints = 0 }
                }
              in
              Verify.Instance.T (max_proofs_verified, m, vk, x, p) )
          |> Verify.verify_heterogenous )

    let verify ~typ ts = verify_promise ~typ ts

    let srs_precomputation () : unit =
      let srs = Tock.Keypair.load_urs () in
      List.iter [ 0; 1; 2 ] ~f:(fun i ->
          Kimchi_bindings.Protocol.SRS.Fq.add_lagrange_basis srs
            (Domain.log2_size (Common.wrap_domains ~proofs_verified:i).h) )
  end

  let compile_promise :
      type var value a_var a_value ret_var ret_value auxiliary_var auxiliary_value prev_varss prev_valuess prev_ret_varss prev_ret_valuess widthss heightss max_proofs_verified branches.
         ?self:(var, value, max_proofs_verified, branches) Tag.t
      -> ?cache:Key_cache.Spec.t list
      -> ?disk_keys:
           (Cache.Step.Key.Verification.t, branches) Vector.t
           * Cache.Wrap.Key.Verification.t
      -> ?return_early_digest_exception:bool
      -> public_input:
           ( var
           , value
           , a_var
           , a_value
           , ret_var
           , ret_value )
           Inductive_rule.public_input
      -> auxiliary_typ:(auxiliary_var, auxiliary_value) Impls.Step.Typ.t
      -> branches:(module Nat.Intf with type n = branches)
      -> max_proofs_verified:
           (module Nat.Add.Intf with type n = max_proofs_verified)
      -> name:string
      -> constraint_constants:Snark_keys_header.Constraint_constants.t
      -> choices:
           (   self:(var, value, max_proofs_verified, branches) Tag.t
            -> ( prev_varss
               , prev_valuess
               , widthss
               , heightss
               , a_var
               , a_value
               , ret_var
               , ret_value
               , auxiliary_var
               , auxiliary_value )
               H4_6.T(Inductive_rule).t )
      -> unit
      -> (var, value, max_proofs_verified, branches) Tag.t
         * Cache_handle.t
         * (module Proof_intf
              with type t = (max_proofs_verified, max_proofs_verified) Proof.t
               and type statement = value )
         * ( prev_valuess
           , widthss
           , heightss
           , a_value
           , ( ret_value
             * auxiliary_value
             * (max_proofs_verified, max_proofs_verified) Proof.t )
             Promise.t )
           H3_2.T(Prover).t =
   (* This function is an adapter between the user-facing Pickles.compile API
      and the underlying Make(_).compile function which builds the circuits.
   *)
   fun ?self ?(cache = []) ?disk_keys ?(return_early_digest_exception = false)
       ~public_input ~auxiliary_typ ~branches ~max_proofs_verified ~name
       ~constraint_constants ~choices () ->
    let self =
      match self with
      | None ->
          { Tag.id = Type_equal.Id.create ~name sexp_of_opaque
          ; kind = Compiled
          }
      | Some self ->
          self
    in
    (* Extract to_fields methods from the public input declaration. *)
    let (a_var_to_fields : a_var -> _), (a_value_to_fields : a_value -> _) =
      match public_input with
      | Input (Typ typ) ->
          ( (fun x -> fst (typ.var_to_fields x))
          , fun x -> fst (typ.value_to_fields x) )
      | Output _ ->
          ((fun () -> [||]), fun () -> [||])
      | Input_and_output (Typ typ, _) ->
          ( (fun x -> fst (typ.var_to_fields x))
          , fun x -> fst (typ.value_to_fields x) )
    in
    let module A_var = struct
      type t = a_var

      let to_field_elements = a_var_to_fields
    end in
    let module A_value = struct
      type t = a_value

      let to_field_elements = a_value_to_fields
    end in
    let module Ret_var = struct
      type t = ret_var
    end in
    let module Ret_value = struct
      type t = ret_value
    end in
    let module Auxiliary_var = struct
      type t = auxiliary_var
    end in
    let module Auxiliary_value = struct
      type t = auxiliary_value
    end in
    let module M =
      Make (A_var) (A_value) (Ret_var) (Ret_value) (Auxiliary_var)
        (Auxiliary_value)
    in
    let rec conv_irs :
        type v1ss v2ss v3ss v4ss wss hss.
           ( v1ss
           , v2ss
           , wss
           , hss
           , a_var
           , a_value
           , ret_var
           , ret_value
           , auxiliary_var
           , auxiliary_value )
           H4_6.T(Inductive_rule).t
        -> (v1ss, v2ss, wss, hss) H4.T(M.IR).t = function
      | [] ->
          []
      | r :: rs ->
          r :: conv_irs rs
    in
    let provers, wrap_vk, wrap_disk_key, cache_handle =
      M.compile ~return_early_digest_exception ~self ~cache ?disk_keys ~branches
        ~max_proofs_verified ~name ~public_input ~auxiliary_typ
        ~constraint_constants
        ~choices:(fun ~self -> conv_irs (choices ~self))
        ()
    in
    let (module Max_proofs_verified) = max_proofs_verified in
    let T = Max_proofs_verified.eq in
    let module Value = struct
      type t = value

      let typ : (var, value) Impls.Step.Typ.t =
        match public_input with
        | Input typ ->
            typ
        | Output typ ->
            typ
        | Input_and_output (input_typ, output_typ) ->
            Impls.Step.Typ.(input_typ * output_typ)

      let to_field_elements =
        let (Typ typ) = typ in
        fun x -> fst (typ.value_to_fields x)
    end in
    let module P = struct
      type statement = value

      type return_type = ret_value

      module Max_local_max_proofs_verified = Max_proofs_verified

      module Max_proofs_verified_vec = Nvector (struct
        include Max_proofs_verified
      end)

      include
        Proof.Make
          (struct
            include Max_proofs_verified
          end)
          (struct
            include Max_local_max_proofs_verified
          end)

      let id = wrap_disk_key

      let verification_key = wrap_vk

      let verify_promise ts =
        verify_promise
          ( module struct
            include Max_proofs_verified
          end )
          (module Value)
          (Lazy.force verification_key)
          ts

      let verify ts = verify_promise ts

      let statement (T p : t) =
        p.statement.messages_for_next_step_proof.app_state
    end in
    (self, cache_handle, (module P), provers)

  let compile ?self ?cache ?disk_keys ~public_input ~auxiliary_typ ~branches
      ~max_proofs_verified ~name ~constraint_constants ~choices () =
    let self, cache_handle, proof_module, provers =
      compile_promise ?self ?cache ?disk_keys ~public_input ~auxiliary_typ
        ~branches ~max_proofs_verified ~name ~constraint_constants ~choices ()
    in
    let rec adjust_provers :
        type a1 a2 a3 a4 s1 s2_inner.
           (a1, a2, a3, s1, s2_inner Promise.t) H3_2.T(Prover).t
        -> (a1, a2, a3, s1, s2_inner Deferred.t) H3_2.T(Prover).t = function
      | [] ->
          []
      | prover :: tl ->
          (fun ?handler public_input ->
            Promise.to_deferred (prover ?handler public_input) )
          :: adjust_provers tl
    in
    (self, cache_handle, proof_module, adjust_provers provers)

  module Provers = H3_2.T (Prover)
  module Proof0 = Proof
end

include Wire_types.Make (Make_sig) (Make_str)
