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

(* A 'homophones' is a table that maps integers to monomials.  The lexicon
 * (below) will associate a morph with a 'homophones'. *)
type homophones = monomial IntMap.t

module Lexicon = Map.Make(struct
        type t = morph
        let compare = compare
end)

(* A lexicon is a table that maps morphs to 'homophones' tables.  In other
 * words, it lets each morph have multiple meanings (=monomials), which are
 * indexed by integers. *)
type lexicon = homophones Lexicon.t

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
let update lex mrph mean =
        let newVal = if Lexicon.mem mrph lex then
                let means = Lexicon.find mrph lex in
                means @ [mean]
        else
                [mean]
        in
        Lexicon.add mrph newVal lex
(* TODO: In the paper, update accepts an index, which is how it knows whether to
 * replace an existing meaning or posit a new one.  We'll probably accept an
 * index too, and use arrays instead of lists here, and implement it just as KP
 * wrote it in the paper.  (Possibly requiring easy changes to Intersect and
 * other functions.) *)

(* The similarity of two monomials = the size of their intesection. *)
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
        let f x y = compareDissimTo e (fst x) (fst y) in
        List.sort f ms

(* ms is a monomial*int list.  Each element is a meaning (the monomial) paired
 * with an index (the integer).  In Type3Learner, ms will be a list of
 * (x,y), where:
 *      x is the meaning of some morph in the lexicon
 *      y is the index of that meaning associated with that morph
 * And in Type3Learner, this list will be already sorted by similarity to e.
 *
 * Returns (a,b) where:
 *      a is e intersected with the monomial in the head of ms
 *      b is the integer in the head of ms 
 *)
let intersect e ms =
        let h,i = List.hd ms in
        (FSet.inter h e), i

(* upto x = [1; 2; 3; ...; x] *)
let upto x =
        let rec build (i, is) = if i < 1 then
                (i, is)
        else
                build (i-1, i::is)
        in
        let (_, result) = build (x, []) in
        result

(* indexize [a; b; c; ...] = [(a,1); (b,2); (c,3); ...]. *)
let indexize x =
        let l = List.length x in
        List.combine x (upto l)

(* TODO: Document.  This is a stub. *)
let rec getMeanBRTbl lex br tbl sms = monomial [], br, tbl
(* TODO: Ask KP what this should do if everything in sms results in an overlap.
 *)

(* Update the lexicon l, blocking rules b, and table t appropriately to account
 * for observing the morph m in environment (maximal monomial) e.  Returns a
 * triple: (the updated lexicon, the updated blocking rules, the updated table).
 *)
let learn lex br tbl m e =
        let ms = try Lexicon.find m lex with Not_found -> [] in
        (* ms = the list of meanings for the homophones of m *)
        let ims = indexize ms in (* indexized ms *) 
        let sms = sortDissimTo e ims in
        (* sms = the list of meanings, sorted by similarity to e, paired with
         * their homophone indexes in the lexicon. *)
        let mean, br2, tbl2 = getMeanBRTbl lex br tbl sms in
        let lex2 = update lex m mean in
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

let ms = [(m1,1); (m2,2); (m3,3); (m5,4); (m4,5); (m6,6); (m7,7)]

let i = intersect e ms
let t = assert (   i = ((FSet.inter e m1), 1)   )

let ms2 = [(m5,1); (m1,2); (m4,3);]
let t = assert ([(m1,2); (m5,1); (m4,3)] = sortDissimTo e ms2)

let t = assert ([1; 2; 3; 4; 5; 6] = upto 6)
let t = assert ([] = upto 0)
