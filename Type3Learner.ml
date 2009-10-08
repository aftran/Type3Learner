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

(* A lexicon is a table that maps morphs to tables that map integers to
 * monomials.  In other words, it lets each morph have multiple meanings
 * (=monomials), which are indexed by integers. *)
type lexicon = (morph, monomial list) Hashtbl.t

type blocker = (morph*int) * (morph*int)
(* Might restructure the lexicon as a map so we can do something like
*       type blocker = lexKey * lexKey                          *)

type seenness = Seen | Predicted

type table = (monomial, (morph*int*seenness list)) Hashtbl.t
(* This is a map from (maximal) monomials to lists of lexicon keys
 * (=monomial*int) associated with a seenness value. *)

(* Impure function with the same signature as Hashtbl.add.
 * Adds (monomial, i) ~> meaning to the Hashtbl l and then minimizes l. *)
let update l (monomial, i) meaning =
        let minimize l (monomial, i) meaning = 
                () 
        in
        Hashtbl.remove l (monomial, i);
        Hashtbl.add l (monomial, i) meaning;
        minimize l (monomial, i) meaning

(* For a monomial list ms and environment (=maximal monomial) e, output an
 * element of ms that is the most similar to e. *)
let maxSimilarity ms e = List.hd ms (*STUB! TODO*) 

let similarity s t = FSet.cardinal (FSet.inter s t)
