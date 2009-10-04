(* M: *)
type morph = string

type feature = string*string
(* Features are commonly written as "+group" or "-augmented".  I will represent
 * these as ("group", "+") or ("augmented", "-").
 * I don't have to represent the hierarchy of feature implications (for example,
 * feature -group might imply -augmented), because I can assume the input text
 * obeys it.  The Type3Learner algorithm currently does not take any knowledge
 * of the feature hierarchy into account.
 * As a consequence, I can represent monomials as unstructed sets of features. *)

module FSet = Set.Make (struct
        type t = feature
        let compare = compare
end)

type monomial = FSet.t

type lexeme = morph * monomial

module LSet = Set.Make(struct
        type t = lexeme*int
        let compare = compare
end)

type lexicon = LSet.t

type blocker = (morph*int) * (morph*int)
(* Might restructure the lexicon as a map so we can do something like
*       type blocker = lexKey * lexKey                          *)

module Table = Map.Make(struct
        type t = monomial
        let compare = compare
end)

type table = (morph*int*int list) Table.t
(* This is a map from (maximal) monomials to monomials. *)
