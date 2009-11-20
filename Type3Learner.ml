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

module FSet = Set.Make(struct
        type t = feature
        let compare = compare
end)

type monomial = FSet.t

(* Construct a monomial that contains the values in list x. *)
let monomial x = List.fold_right FSet.add x FSet.empty

module IntMap = Map.Make(struct
        type t = int
        let compare = compare
end)

(* A lexeme is a table that maps integers to monomials. *)
type lexeme = monomial IntMap.t

module Lexicon = Map.Make(struct
        type t = morph
        let compare = compare
end)

(* A lexicon is a table that maps morphs to 'homophones' tables.  In other
 * words, it lets each morph have multiple meanings (=monomials), which are
 * indexed by integers. *)
type lexicon = lexeme Lexicon.t

module MSet = Set.Make(struct
        type t = morph*int
        let compare = compare
end)

module Table = Map.Make(struct
        type t = monomial
        let compare = compare
end)

(* A table is a map from (maximal) monomials to sets of morphs with integer
 * indexes. *)
type table = MSet.t Table.t

(* matches t e = the union of all values of t whose key is a subset of e. *)
let matches (t:table) (e:monomial) =
        let (<) x y = FSet.subset x y in
        let (+) x y = MSet.union x y in
        let f (k:monomial) (d:MSet.t) a =
                if k < e then a + d else a
        in
        Table.fold f t MSet.empty

(* Graph type for the blocking rules: *)

module IndexedMorph = struct
  type t = morph*int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end

module G = Graph.Persistent.Digraph.Concrete(IndexedMorph)

type graph = G.t

module DFS = Graph.Traverse.Dfs(G)

(* meanings m l = the lexeme associated with morph m in lexicon l, if it exists,
 * otherwise the empty lexeme. *)
let meanings (m:morph) (l:lexicon) =
        try Lexicon.find m l with Not_found -> IntMap.empty

(*  update l m i mn = the lexicon l with the added meaning mn associated
 *  with index i of morph m. *)
let update (l:lexicon) (m:morph) (i:int) (mn:monomial) =
        let t = meanings m l in
        let newVal = IntMap.add i mn t in
        Lexicon.add m newVal l

(* similarity s t = the cardinality of s intersected with t. *)
let similarity s t = FSet.cardinal (FSet.inter s t)

(* Compare function for sorting monomials by dissimilarity to e. *)
let compareDissimTo e s t = compare (similarity e t) (similarity e s)
 
(* sortDissimTo e ms = ms sorted so that the monomials are in decreasing order
 * of similarity to e.  The integers paired with the monomials are ignored but
 * kept with the same monomials they were originally paired with.
 *
 * For a monomial*int list ms and environment (=maximal monomial) e,
 * sortDissimTo e [(m1,1); (m2,2); (m3,3), ...] =
 * [(mN(1),N(1)); (mN(2),N(2)); (mN(3),N(3)); ...],
 * where mN(1), mN(2), ... are in decreasing order of similarity to e. *)
let sortDissimTo (e:monomial) (ms:(int*monomial) list) =
        let f x y = compareDissimTo e (snd x) (snd y) in
        List.sort f ms

(* With ms = [(4,m1); (9,m2); (1,m3); ...] (for example),
 * intersect e ms total = (a,b), where:
 *      a = e intersected with the monomial in the head of ms (if ms has a
 *      head) or just e.
 *      b = the integer in the head of ms (if ms has a head) or just total+1.
 *)
let intersect (e:monomial) (ms:(int*monomial) list) (total:int) =
        match ms with
        | (i,h)::_ -> i, (FSet.inter h e)
        | [] -> total+1, e

(* Return (a new table, a new blocking graph) as if we are adding
 * (m -> i -> mean) to the lexicon. *)
let synchronize
        (s:table) (p:table) (br:graph) (m:morph) (i:int) (mn:monomial) (e:monomial)
=
        s, p, br (* TODO: Stub. *)

(* overlap x is true iff x has a cycle. *)
let overlap = DFS.has_cycle

(* TODO: Document. *)
let rec getHypothesis
        (lex:lexicon) (br:graph) (s:table) (p:table) (m:morph)
        (e:monomial) (ms:(int*monomial) list) (total:int)
=
        let i, mean = intersect e ms total in
        let s2, p2, br2 = synchronize s p br m i mean e in
        if overlap br2 then
                (* Start over without the head of ms *)
                getHypothesis lex br s p m e (List.tl ms) total
                (* Question: what happens if everything in ms results in an
                 * overlap?
                 * Answer: No, once ms becomes empty, getHypothesis
                 * posits a new homophone of m, which will never overlap with
                 * anything.
                 * Upshot: this recursion will be finite.*)
        else
                mean, i, br2, s2, p2

(* lexeme2list l = the list of (key,value) pairs in l, in no particular
 * order. *)
let lexeme2list l =
        let f k d a = (k,d)::a in
        IntMap.fold f l []

(* Update the lexicon l, blocking rules b, and table t appropriately to account
 * for observing the morph m in environment (maximal monomial) e.  Returns a
 * triple: (the updated lexicon, the updated blocking rules, the updated table).
 *)
let learn (lex:lexicon) (br:graph) (s:table) (p:table) (m:morph) (e:monomial) =
        let ms = meanings m lex in
        (* ms = the list of meanings for the homophones of m *)
        let ims = lexeme2list ms in (* indexized ms *)
        let sms = sortDissimTo e ims in
        (* sms = the list of meanings, sorted by similarity to e, paired with
         * their homophone indexes in the lexicon. *)
        let mean, idx, br2, s2, p2 = getHypothesis lex br s p m e sms (List.length sms) in
        let lex2 = update lex m idx mean in
        lex2, br2, s2, p2

(* TESTS *)
(* TODO: Move the tests to a different file. *)

let m1 = monomial [("A","+"); ("B","-")]
let m2 = monomial [("B","-"); ("G","+"); ("A","-")]
let m3 = monomial [("C","+"); ("A","-")]
let m4 = monomial [("D","+"); ("I","-"); ("J","-"); ("K","-")]
let m5 = monomial [("E","+"); ("A","-")]
let m6 = monomial [("A","-"); ("E","+")]
let m7 = monomial [("A","-"); ("E","+")]

(* TODO: I'm mis-using 'assert', but it works for now.  Will fix later. *)
let t = assert (not (m1 = m2))
let t = assert (m1 = m1)
let t = assert (m6 = m7)

let e = monomial [("A","+"); ("B","-"); ("C","+"); ("D","-"); ("E","-"); ("A","-")]

let t = assert (compareDissimTo e m1 m3 = -(compare 2 2))
let t = assert (compareDissimTo e m2 (monomial []) = -(compare 2 0))

let ms = [(1,m1); (2,m2); (3,m3); (4,m5); (5,m4); (6,m6); (7,m7)]

let i = intersect e ms 0
let t = assert (   i = (1, (FSet.inter e m1))   )
let t = assert (   (1,e) = (intersect e [] 0)    )

let ms2 = [(1,m5); (2,m1); (3,m4);]
let t = assert ([(2,m1); (1,m5); (3,m4)] = sortDissimTo e ms2)
