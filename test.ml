open Type3Learner

(* ------- UTILITIES---------*)

let printSet m = let rec printFeat = function
      |[] -> print_newline()
      |(feat,value)::t -> (print_string feat; print_string":"; print_string value; printFeat t) 
    in printFeat (FSet.elements m)

(* printing the lexicon *)
let printLexicon lexicon = 
  let printMon key monomial = 
    print_int key;
    print_string " - ";
    printSet monomial
  in
  let printLexeme key lexeme = 
    print_string key; 
    IntMap.iter printMon lexeme
  in Lexicon.iter printLexeme lexicon;;

(*KP test*)
(* define a paradigm over 2 binary features A and B*)
let m1 = monomial [("A","+"); ("B","-")];;
let m2 = monomial [("A","+"); ("B","+")];;
let m3 = monomial [("A","-"); ("B","+")];;
let m4 = monomial [("A","-"); ("B","-")];;

let text1 = [("y",m1);("x",m2);("y",m3);("x",m4)];;
let text2 = [("y",m1);("y",m2);("y",m3);("x",m4)];;

let type3learn_with_printing (t:text) =
        let f ((lex, v, br, s, p), step) (m,e) =
                let (lex2, v2, br2, s2, p2), step2 = (learn lex v br s p m e), step+1 in
                print_string "\nStep "; print_int step2;
                print_string ".  Right after reacting to ";
                print_string m; print_string " in "; printSet e;
                print_string ", the lexicon is:\n";
                printLexicon lex2;
                print_string "BR is ";
                print_string (if DG.is_empty br2 then "empty\n" else "non-empty\n");
                (lex2, v2, br2, s2, p2), step2
        in
        List.fold_left f ((Lexicon.empty, G.empty, DG.empty, Table.empty, Table.empty), 0) t;;

print_string "Reacting to text1:\n"
let (lex,var,br,t1,t2),steps = type3learn_with_printing text1;;
print_string "\n\n";;
print_string "lex from text1:\n";;
printLexicon lex;;
(* here's the output, which shows that either the overlap was not detected, or the lex was not updated properly
# printLexicon lex;;
x1 - 
y1 - 
- : unit = ()
*)
assert ( not (cycle br) );;  (*no cycles in the br*)


print_string "Reacting to text2:\n"
let (lex2,var2,br2,t12,t22),steps2 = type3learn_with_printing text2;;
print_string "\n\n";;
print_string "\nlex from text2:\n";;
printLexicon lex2;; 
print_string "\n\n";;
(* here's the output for lex, which is correct
# printLexicon lex2;;
x1 - A:-B:-
y1 - 
- : unit = ()
 However, BR is empty while it shouldn't be:*)
assert ( not ( DG.is_empty br2 ));;


(*MC test*)
let mySeen = morphXint_set [("y",9)]
let myPredicted = morphXint_set [("w",5)]

let myBR = update_blocking_row DG.empty mySeen myPredicted;;
let correctBR = DG.add_edge DG.empty ("y",9) ("w",5);;
assert ( not ( DG.is_empty myBR ) );;
assert ( not ( DG.is_empty correctBR ) );;
assert  ( myBR = correctBR );;

let e1 = monomial [("A","+"); ("B","+")]
let e2 = monomial [("A","-"); ("B","+")]
let e3 = monomial [("A","+"); ("B","-")]
let e4 = monomial [("A","-"); ("B","-")]
let eA = monomial [("A","+")]

let mySeenTable = table [
        (e1, [("x",4)]);
        (e2, [("x",4)]);
        (e3, [("y",3); ("z",2)]);
        (e4, [("y",3)]);
]

let myPredictedTable = table [
        (eA, [("x",4)])
]

let myBR2 = compute_blocking mySeenTable myPredictedTable
let tempBR2 = DG.add_edge DG.empty ("y",3) ("x",4)
let correctBR2 = DG.add_edge tempBR2 ("z",2) ("x",4);;

assert ( not ( DG.is_empty correctBR2 ) );;
assert ( not ( DG.is_empty myBR2 ) );;
assert ( myBR2 = correctBR2 );;


let m1 = monomial [("A","+"); ("B","-")]
let m2 = monomial [("B","-"); ("G","+"); ("A","-")]
let m3 = monomial [("C","+"); ("A","-")]
let m4 = monomial [("D","+"); ("I","-"); ("J","-"); ("K","-")]
let m5 = monomial [("E","+"); ("A","-")]
let m6 = monomial [("A","-"); ("E","+")]
let m7 = monomial [("A","-"); ("E","+")];;

assert  (not (m1 = m2));;
assert  (m1 = m1);;
assert  (m6 = m7);;

let e = monomial [("A","+"); ("B","-"); ("C","+"); ("D","-"); ("E","-"); ("A","-")];;

assert  (compare_dissim_to e m1 m3 = -(compare 2 2));;
assert  (compare_dissim_to e m2 (monomial []) = -(compare 2 0));;

let ms = [(1,m1); (2,m2); (3,m3); (4,m5); (5,m4); (6,m6); (7,m7)]

let i = intersect e ms 0;;
assert  (   i = (1, (FSet.inter e m1))   );;
assert  (   (1,e) = (intersect e [] 0)    )

let ms2 = [(1,m5); (2,m1); (3,m4);];;
assert  ([(2,m1); (1,m5); (3,m4)] = sort_dissim_to e ms2)

(* Test a first step in learning: *)
let mn = monomial [("A","-"); ("B","-"); ("C","-")]
let (lex, v, br, s, p) = learn
                Lexicon.empty G.empty DG.empty Table.empty Table.empty "hello" mn
let expectedLex = update_lex Lexicon.empty "hello" 1 mn;;
assert  ( lex = expectedLex );;
assert  ( br = DG.empty );;
let expectedS = update_table Table.empty mn ("hello",1);;
assert  ( s = expectedS );;
let expectedP = expectedS;;
assert  ( p = expectedS );;
