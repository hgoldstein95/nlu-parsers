Require Import QArith.
Require Import List.
Import ListNotations.

Inductive dist A :=
  | Dist : list (A * Q) -> dist A.

Definition run_dist {A} (d : dist A) : list (A * Q) :=
  match d with
  | Dist _ x => x
  end.

Definition sum xs := List.fold_left Qplus xs 0.

Definition mod_snd {A} {B} {C} (f : A -> B) (xs : list (C * A)) : list (C * B) :=
  List.map (fun p => (fst p, f (snd p))) xs.

Definition nonzero_bool q :=
  match Qeq_dec q 0 with
  | left _ => false
  | right _ => true
  end.

Definition mk_dist {A} (xs : list (A * Q)) : dist A :=
  let s := sum (List.map snd xs) in
  let xs' := List.filter (fun x => nonzero_bool (snd x)) xs in
  Dist _ (mod_snd (fun x => x / s) xs').

Definition sum_probs {A} (d : dist A) :=
  sum (List.map snd (run_dist d)).

Theorem mk_dist_correct : forall {A : Type} (xs : list (A * Q)),
    run_dist (mk_dist xs) = [] \/ sum_probs (mk_dist xs) == 1.
Proof.
  intros.
  induction xs ; simpl ; auto.
  destruct IHxs.