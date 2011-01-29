(* In this example below, we create a Type3learner module, L, that considers
 * morphs and features to both be strings.
 *
 *    To compile and run in a terminal, type:
 *      ocamlc -I +ocamlgraph graph.cma type3learner.ml demo.ml -o demo
 *      ./demo
 *
 * For advanced users: Type3learner is a functor that lets you pick what types
 * morphs are.  This lets you treat any data type you want as a morph, as long
 * as you tell the functor about it in advance.  This makes it possible to store
 * data about pronunciation or grammar in each morph.  For everyday use, it is
 * easiest to just define morphs to be strings, as in this example. *)

module L = Type3learner.Make(struct
        type morph = string
        let compare_morphs = compare
        let morph2string o = o
end)

(* First, make some monomials that can be used in texts that we feed the
 * learner. *)
let m1 = L.list2monomial [("A","+"); ("B","-")];;
let m2 = L.list2monomial [("A","+"); ("B","+")];;
let m3 = L.list2monomial [("A","-"); ("B","+")];;
let m4 = L.list2monomial [("A","-"); ("B","-")];;

(* A text (type L.text) is a list of (morph,monomial) pairs.  Since morphs are
 * strings, we can write a text as a list of (string,monomial) pairs. *)
let text1 = [("y",m1);("x",m2);("y",m3);("x",m4)];;
let text2 = [("y",m1);("y",m2);("y",m3);("x",m4)];;

print_string "TEXT ONE\n========\n";;

(* type3learn_with_printing is exactly like type3learn, only it prints its state
 * after every step.  It is the easiest way to see what happens when you feed
 * the learner a certain text. *)
L.type3learn_with_printing text1;;

(* Different calls to type3learn(_with_printing) never affect each other. *)
print_string "\n\nTEXT TWO\n========\n";;
L.type3learn_with_printing text2;;
