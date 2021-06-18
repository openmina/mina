use crate::pasta_fp::Fp;
use mina_curves::pasta::{
    fp::Fp as mina_Fp,
    vesta::{Affine as GAffine, VestaParameters},
};

use oracle::{
    self,
    poseidon::PlonkSpongeConstants,
    sponge::{DefaultFqSponge, DefaultFrSponge},
    FqSponge,
};

use commitment_dlog::commitment::{shift_scalar, PolyComm};
use plonk_circuits::scalars::RandomOracles;
use plonk_protocol_dlog::{
    index::VerifierIndex as DlogVerifierIndex, prover::ProverProof as DlogProof,
};

use crate::pasta_fp_plonk_verifier_index::CamlPastaFpPlonkVerifierIndex;

/// The state of the verifier during verification
#[derive(ocaml::ToValue, ocaml::FromValue)]
pub struct CamlPastaFpPlonkOracles {
    pub o: RandomOracles<mina_Fp>, // all the challenges produced during the protocol
    pub p_eval: (Fp, Fp),          // two evaluation of some poly?
    pub opening_prechallenges: Vec<Fp>, // challenges before some opening?
    pub digest_before_evaluations: Fp, // digest of poseidon before evaluating?
}

/// Creates a [CamlPastaFpPlonkOracles] state which will initialize a verifier
#[ocaml::func]
pub fn caml_pasta_fp_plonk_oracles_create(
    lgr_comm: Vec<PolyComm<GAffine>>, // the bases to commit polynomials
    index: CamlPastaFpPlonkVerifierIndex, // parameters
    proof: DlogProof<GAffine>,        // the final proof (contains public elements at the beginning)
) -> CamlPastaFpPlonkOracles {
    let index: DlogVerifierIndex<'_, GAffine> = index.into();
    let proof: DlogProof<GAffine> = proof.into(); // isn't this useless?
    let lgr_comm: Vec<PolyComm<GAffine>> = lgr_comm.into_iter().map(From::from).collect(); // isn't this useless?

    // get commitments to the public elements
    let p_comm = PolyComm::<GAffine>::multi_scalar_mul(
        &lgr_comm
            .iter()
            .take(proof.public.len())
            .map(|x| x) // isn't this useless?
            .collect(),
        &proof.public.iter().map(|s| -*s).collect(),
    );

    // runs the entire protocol
    let (mut sponge, digest_before_evaluations, o, _, p_eval, _, _, _, combined_inner_product) =
        proof.oracles::<DefaultFqSponge<VestaParameters, PlonkSpongeConstants>, DefaultFrSponge<Fp, PlonkSpongeConstants>>(&index, &p_comm);

    // absorb the combined inner product into the sponge, as an Fr (why?)
    // shift_scalar = x - 2^(modulus_bits), why??
    sponge.absorb_fr(&[shift_scalar(combined_inner_product)]);

    // return the state at that point on.
    CamlPastaFpPlonkOracles {
        o: o,
        p_eval: (p_eval[0][0], p_eval[1][0]),
        opening_prechallenges: proof
            .proof
            .prechallenges(&mut sponge)
            .into_iter()
            .map(|x| x.0)
            .collect(),
        digest_before_evaluations: digest_before_evaluations,
    }
}

#[ocaml::func]
pub fn caml_pasta_fp_plonk_oracles_dummy() -> RandomOracles<Fp> {
    RandomOracles::zero().into()
}

#[ocaml::func]
pub fn caml_pasta_fp_plonk_oracles_deep_copy(x: RandomOracles<Fp>) -> RandomOracles<Fp> {
    x
}
