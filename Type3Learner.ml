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

(* Symmetric difference of two sets. *)
let delta a b = let u = FSet.union a b in
                let i = FSet.inter a b in
                FSet.diff u i

let similar a b = let d = delta a b in
                  1 == FSet.cardinal d

(* The "minimized" version of h::hs, which is a list of monomials.  Minimizing
 * means combining (by intersection) any pair of monomials that differs by only
 * one element.  Non-idempotent.
 * TODO: This isn't quite right yet.  We need to make the list completely
 * minimized so that no two morphs are "similar" (as defined in the function
 * above).  One way to do this is by repeating the minimize process until we
 * reach a fixed point. *)
let rec minimize h hs = match hs with
        | [] -> h::hs
        | m::ms -> if similar h m then 
                let n = FSet.inter h m in
                minimizeBy n ms
        else
                m::minimizeBy h ms

(* Impure (imperative) function with the same signature as Hashtbl.add.
 * Adds mean (a monomial) to the list of homonyms associated with the morpheme
 * moph in lexicon l. *)
let update l morph mean =
        if Hashtbl.mem l morph then
                let means = Hashtbl.find l morph in
                Hashtbl.replace l morph (minimize mean means)
        else
                Hashtbl.add l morph [mean]
                (* no need to minimize if the lexicon was already minimized *)

(* The similarity of two monomials = the size of their intesection. *)
let similarity s t = FSet.cardinal (FSet.inter s t)

(* Compare function for sorting monomials by dissimilarity to e. *)
let compareDissimTo e s t = compare (similarity e s) (similarity e t)
 
(* For a monomial list ms and environment (=maximal monomial) e, output an
 * element of ms that is the most similar to e. 
 *
 * This is a wasteful implementation, because I'm sorting the whole list before
 * using only the first element.  But these lists will be short, and we're not
 * worried about minute performance gains. *)
let maxSimilarity ms e = List.hd (List.sort (compareDissimTo e) ms)
