(* M: *)
(*
type morph = A | Lot | Of | Possible | Morphs
 *)
type morph = string

(* These data types are enumerations of possible feature values: *)
type 'a polarity = Minus | Plus of 'a
type augmented = Augmented of unit polarity
type minimal = Minimal of augmented polarity

(* The set F: *)
type feature =
        | Participant
        | Speaker of unit polarity
        | Group of minimal polarity

let hi = Participant

module FSet = Set.Make (struct
        type t = feature
        let compare = compare
end)

type monomial = FSet
(* We'll maybe use "feature list" or "feature set" of "feature hashtable". *)

(* A monomial is maximal iff it contains one feature for each feature
 * constructor.  (Easy function to write.)  *)

type lexicon = (morph * monomial) list
(* It'd be nice if I could encode a specific type for "maximal monomial" to
 * enforce that a lexicon's elements can only have maximal monomials.
 *
 * And again, we might want the lexicon to be a (morph * monomial) set or a
 * (morph * monomial, bool) hashtable. *)

(* I haven't done Lex, BR and T yet. *)
