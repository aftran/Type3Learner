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

(* A lexicon is a table that maps morphs to lexemes. *)
type lexicon = lexeme Lexicon.t

module MSet = Set.Make(struct
        type t = morph*int
        let compare = compare
end)

(* morphXint_set x = an MSet containing the elements of list x *)
let morphXint_set x = List.fold_right MSet.add x MSet.empty

module Table = Map.Make(struct
        type t = monomial
        let compare = compare
end)

(* A table is a map from monomials to sets of morphs with integer
 * indexes. *)
type table = MSet.t Table.t

(* table x = a new table based on the list x of type
 * (monomial*((morph*int) list)).  The list is of ordered pairs that map a
 * monomial to a list of morphs.  The new table maps those monomials to their
 * respective lists of morphs.. *)
let table (x:(monomial*((morph*int) list)) list) =
        let f (k,v) a = Table.add k(morphXint_set v) a in
        List.fold_right f x Table.empty

(* In terms of Type3Learner, matches e t = the morphs predicted to appear in
 * environment e, according to table t.
 * matches e t = the union of all values of t whose key is a subset of e. *)
let matches (e:monomial) (t:table) =
        let (<) x y = FSet.subset x y in
        let (+) x y = MSet.union x y in
        let f (k:monomial) (d:MSet.t) a =
                if k < e then a + d else a
        in
        Table.fold f t MSet.empty

module IndexedMorph = struct
  type t = morph*int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end

(* Blocking rules will be stored in a digraph: *)

module DG = Graph.Persistent.Digraph.Concrete(IndexedMorph)

type digraph = DG.t

module DFS = Graph.Traverse.Dfs(DG)

(* The free-variation pairs will be stored in an undirected graph: *)

module G = Graph.Persistent.Graph.Concrete(IndexedMorph)

type graph = G.t

(* Functions for updating a hypothesis: *)

(* meanings m l = the lexeme associated with morph m in lexicon l, if it exists,
 * otherwise the empty lexeme. *)
let meanings (m:morph) (l:lexicon) =
        try Lexicon.find m l with Not_found -> IntMap.empty

(*  update_lex l m i mn = the lexicon l with the added meaning mn associated
 *  with index i of morph m. *)
let update_lex (l:lexicon) (m:morph) (i:int) (mn:monomial) =
        let x = meanings m l in
        let newVal = IntMap.add i mn x in
        Lexicon.add m newVal l

(* In terms of Type3Learner, morphs e t = the morphs seen in environment e,
 * according to table t.
 * morphs e t = the MSet.t associated with monomial e in table t, if it exists,
 * otherwise the empty MSet.t. *)
let morphs (e:monomial) (t:table) =
        try Table.find e t with Not_found-> MSet.empty

(* update_table t e m i = the table t with the added pair (m,i) in the set
 * associated with e. *)
let update_table (t:table) (e:monomial) (mi:morph*int) =
        let x = morphs e t in
        let newVal = MSet.add mi x in
        Table.add e newVal t

(* similarity s t = the cardinality of s intersected with t. *)
let similarity s t = FSet.cardinal (FSet.inter s t)

(* Compare function for sorting monomials by dissimilarity to e. *)
let compare_dissim_to e s t = compare (similarity e t) (similarity e s)
 
(* sort_dissim_to e ms = ms sorted so that the monomials are in decreasing order
 * of similarity to e.  The integers paired with the monomials are ignored but
 * kept with the same monomials they were originally paired with.
 *
 * For a monomial*int list ms and environment (=maximal monomial) e,
 * sort_dissim_to e [(m1,1); (m2,2); (m3,3), ...] =
 * [(mN(1),N(1)); (mN(2),N(2)); (mN(3),N(3)); ...],
 * where N is a permutation that ensures that mN(1), mN(2), ... are in
* decreasing order of similarity to e. *)
let sort_dissim_to (e:monomial) (ms:(int*monomial) list) =
        let f x y = compare_dissim_to e (snd x) (snd y) in
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

(* update_blocking_row br seen predicted = br with a new edge added for each pair in
 * the cartesian product seen*(predicted-seen).  This means creating a new
 * blocking rule whenever a morph is predicted (but not seen) in the environment
 * where another morph has been seen. *)
let update_blocking_row (br:digraph) (seen:MSet.t) (predicted:MSet.t) =
        let pNotSeen = MSet.diff predicted seen in
        let f m a =
                let g n b = DG.add_edge b m n in (* m blocks n now *)
                MSet.fold g pNotSeen a
        in
        MSet.fold f seen br

(* Given a table of seen and predicted morphs (s and p, respectively),
 * compute_blocking s p = the blocking-rule digraph generated by this rule:
 * Whenever a morph, a, is seen where another, b, is predicted-but-not-seen, add
 * a blocking rule from a to b. *)
let compute_blocking (s:table) (p:table) =
        let f (e:monomial) (seen:MSet.t) a =
                let predicted = matches e p in
                update_blocking_row a seen predicted
        in
        Table.fold f s DG.empty

(* update_free_variation v seen = v with a new edge added for each pair in the cartesian
 * product seen*seen. *)
let update_free_variation (v:graph) (seen:MSet.t) =
        let f m a =
                let g n b = G.add_edge b m n in (* m,n are in free variation now *)
                MSet.fold g seen a
        in
        MSet.fold f seen v
(* Stupidly, this adds each edge twice, but that won't affect the end result. *)

(* Return the tuple:
 * (a new 'seen' table, a new 'predicted' table, a new free-variation graph, a
 * new blocking digraph) as if we are adding (m -> i -> mean) to the lexicon. *)
let synchronize
        (s:table) (p:table) (v:graph) (br:digraph) (m:morph) (i:int) (mn:monomial) (e:monomial)
=
        let s2 = update_table s e  (m,i) in
        let p2 = update_table p mn (m,i) in
        let seen = morphs  e s in
        let br2 = compute_blocking s2 p2 in
        let v2 = update_free_variation v seen in
        s2, p2, v2, br2

(* cycle x is true iff x has a cycle. *)
let cycle = DFS.has_cycle

(* Return a meaning, morph index, free-variation graph, blocking-rule digraph,
 * seen table, and predicted table in response to the given hypothesis
 * (lex, v, br) and the witnessing of morph m in environment e.  The input must
 * also provide an indexed list of monomials attached to morph m in the lexicon,
 * and the size of this list.
 * TODO: Make an internal recursion function so the caller doesn't have to
 * provide redundant data. *)
let rec get_hypothesis
        (lex:lexicon) (v:graph) (br:digraph) (s:table) (p:table) (m:morph)
        (e:monomial) (ms:(int*monomial) list) (total:int)
=
        let i, mean = intersect e ms total in
        let s2, p2, v2, br2 = synchronize s p v br m i mean e in
        if cycle br2 then
                (* Start over without the head of ms *)
                get_hypothesis lex v br s p m e (List.tl ms) total
                (* Question: what happens if everything in ms results in an
                 * overlap?
                 * Answer: No, once ms becomes empty, get_hypothesis
                 * posits a new homophone of m, which will never overlap with
                 * anything.
                 * Upshot: this recursion will be finite.*)
        else
                mean, i, v2, br2, s2, p2

(* lexeme2list l = the list of (key,value) pairs in l, in no particular
 * order. *)
let lexeme2list l =
        let f k d a = (k,d)::a in
        IntMap.fold f l []

(* Update the lexicon l, free-variation graph v, blocking rules br, and table t
 * appropriately to account for observing the morph m in environment (maximal
 * monomial) e.  Returns a quadrouple: (the updated lexicon, the updated
 * free-variation graph, the updated blocking rules, the updated table).
 *)
let learn (lex:lexicon) (v:graph) (br:digraph) (s:table) (p:table) (m:morph) (e:monomial) =
        let ms = meanings m lex in
        (* ms = the lexeme containing the homophones of m *)
        let ims = lexeme2list ms in (* ms's meanings paired with their indexes  *)
        let sms = sort_dissim_to e ims in
        (* sms = the list of meanings of m, sorted by similarity to e, paired
         * with their homophone indexes in the lexicon. *)
        let mean, idx, v2, br2, s2, p2 =
                get_hypothesis lex v br s p m e sms (List.length sms) in
        let lex2 = update_lex lex m idx mean in
        lex2, v2, br2, s2, p2

type text = (morph*monomial) list

(* type3learn t = (lex, v, br, s, p), where:
 *      lex is a lexicon,
 *      v is a graph of free-variation pairs,
 *      br is a digraph of blocking rules,
 *      s is a table that maps monomials to sets of morphs mitnessed in the
 *      environment of the monomial,
 *      p is a table that maps monomials to sets of morphs predicted in
 *      environments that are supersets of that monomial.
 * lex, v and br together specify the hypothesis that Type3Learner has acquired
 * given text t.  s and p contain no new information but improve the efficiency
 * of the learner. *)
let type3learn (t:text) =
        let f (lex, v, br, s, p) (m,e) = learn lex v br s p m e in
        List.fold_left f (Lexicon.empty, G.empty, DG.empty, Table.empty, Table.empty) t
