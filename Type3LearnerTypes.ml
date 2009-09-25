
(* M: *)
(*
type morph = A | Lot | Of | Possible | Morphs
 *)
type morph = string

(* These data types are enumerations of possible feature values: *)
type tense = Past | Nonpast                     (* F_tense *)
type person = First | Second | Third            (* F_person *)
type quantity = Singular | Plural | Mass        (* F_quantity *)
type gender = Animate | Inanimate               (* F_gender *)
                                                (* F_telic is bool *)
(* The set F: *)
type feature =
        | Tense of tense
        | Person of person 
        | Quantity of quantity
        | Gender of gender
        | Telic of bool

(* Unfortunately, we type each feature's name three different times.  I'd like
 * a better way to define a feature and its allowed values in one go. *)

type monomial = feature list
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
