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

type seenness = Seen | Predicted

module Table = Map.Make(struct
        type t = monomial
        let compare = compare
end)

type table = morph*int*seenness list Table.t
(* A table is a map from (maximal) monomials to lists of morphs (with an integer
 * index) associated with a seenness value.  In the lexicon, each morph is
 * associated with a list of monomials:  morph -> [a; b; c; d].  Here, the int
 * associated with a morph tells us which entry in [a; b; c; d ...] we have
 * seen/predicted. *)

(*  Adds mean (a monomial) to the list of homonyms associated with the morpheme
 * moph in lexicon l.  Returns the new lexicon. *)
let update lex mrph idx mean =
        let t = try Lexicon.find mrph lex with
                Not_found -> IntMap.empty
        in
        let newVal = IntMap.add idx mean t in
        Lexicon.add mrph newVal lex
(* TODO: In the paper, update accepts an index, which is how it knows whether to
 * replace an existing meaning or posit a new one.  We'll probably accept an
 * index too, and use arrays instead of lists here, and implement it just as KP
 * wrote it in the paper.  (Possibly requiring easy changes to Intersect and
 * other functions.) *)

(* similarity s t = the cardinality of s intersected with t. *)
let similarity s t = FSet.cardinal (FSet.inter s t)

(* Compare function for sorting monomials by dissimilarity to e. *)
let compareDissimTo e s t = compare (similarity e t) (similarity e s)
 
(* sortDissimTo sorts a monomial*int list so that the monomials are in
 * decreasing order of similarity, and the integers are ignored but kept with
 * the same monomials they were originally paired with.
 *
 * For a monomial*int list ms and environment (=maximal monomial) e,
 * sortDissimTo e [(m1,1); (m2,2); (m3,3), ...] =
 * [(mN(1),N(1)); (mN(2),N(2)); (mN(3),N(3)); ...],
 * where mN(1), mN(2), ... are in decreasing order of similarity to e. *)
let sortDissimTo e ms =
        let f x y = compareDissimTo e (snd x) (snd y) in
        List.sort f ms

(* ms is an int*monomial list.  e is a monomial.  total is an integer.
 * intersect e ms total = (a,b), where:
 *      a is e intersected with the monomial in the head of ms
 *      b is the integer in the head of ms (if ms has a head) or total+1.
 *)
let intersect e ms total = match ms with
        | pair::pairs -> let i,h = pair in
        (FSet.inter h e), i
        | [] -> e, total+1

(* TODO: Document.  This is a stub. *)
let rec getMeanBRTbl lex br tbl sms = monomial [], 1, br, tbl
(* TODO: Ask KP what this should do if everything in sms results in an overlap.
 *)

(* lexeme2list l = the list of (key,value) pairs in l, in no particular
 * order. *)
let lexeme2list l =
        let f k d a = (k,d)::a in
        IntMap.fold f l []

(* Update the lexicon l, blocking rules b, and table t appropriately to account
 * for observing the morph m in environment (maximal monomial) e.  Returns a
 * triple: (the updated lexicon, the updated blocking rules, the updated table).
 *)
let learn lex br tbl m e =
        let ms = try Lexicon.find m lex with Not_found -> IntMap.empty in
        (* ms = the list of meanings for the homophones of m *)
        let ims = lexeme2list ms in (* indexized ms *) 
        let sms = sortDissimTo e ims in
        (* sms = the list of meanings, sorted by similarity to e, paired with
         * their homophone indexes in the lexicon. *)
        let mean, idx, br2, tbl2 = getMeanBRTbl lex br tbl sms in
        let lex2 = update lex m idx mean in
        lex2, br2, tbl2

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
let t = assert (   i = ((FSet.inter e m1), 1)   )
let t = assert (   (e,1) = (intersect e [] 0)    )

let ms2 = [(1,m5); (2,m1); (3,m4);]
let t = assert ([(2,m1); (1,m5); (3,m4)] = sortDissimTo e ms2)
