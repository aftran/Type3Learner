 (* In this example below, we create a Type3learner module, L, that considers
 * morphs and features to both be strings.
 *
 *   (if running for the first time, and no .cmi file is available)
 *    produce the .cmi file: 
 *	ocamlc -I +ocamlgraph graph.cma type3learner.mli
 *
 *    To compile and run in a terminal, type:
 *      ocamlc -I +ocamlgraph graph.cma type3learner.ml demo.ml -o demo
 *      ./Demo
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
			     end);;

(* 
You can do one of two things to generate the input to the learner.  Either specify the relevant environments below that can be paired with affixes, and then create a list of such pairings as shown immediately below.  Or open the file "SemSpace.ml", use it to generate a text-file that contains all possible combinations of the feature values you specify, then modify this text-file to include the affixes that are associated with each different combination of features (or an environment), and run the functions that appear below after a dashed line.*)

(*Option one: generate your text by hand:*)

let m1 = L.list2monomial [("A","+"); ("B","-")];;
let m2 = L.list2monomial [("A","+"); ("B","+")];;
let m3 = L.list2monomial [("A","-"); ("B","+")];;
let m4 = L.list2monomial [("A","-"); ("B","-")];;

(* A text (type L.text) is a list of (morph,monomial) pairs.  Since morphs are
 * strings, we can write a text as a list of (string,monomial) pairs. *)
let text = [("y",m1);("y",m2);("y",m3);("x",m4)];;

print_string "TEXT ONE\n========\n";;

(* type3learn_with_printing is exactly like type3learn, only it prints its state
 * after every step.  It is the easiest way to see what happens when you feed
 * the learner a certain text. *)
 
L.type3learn_with_printing text;;

(* ----------------------------------------- *)

(* Option 2: read in the input from a text-file. You'll need to modify the function text_from_file to insert the name of your text-file instead of "text2.txt" which appears as the last element in the defn. of this function*)

(*read in the txt file "filename" to a variable "input_text"*)
let input_text filename =
  let lines = ref [] in
  let ic = open_in filename in 
    try 
      while true; do 
	lines := input_line ic :: !lines
      done; []
    with End_of_file -> 
      close_in ic;
      !lines;;

(* Format the input from the file "filename" appropriately *)
let make_text2 filename = 
  let read_input = input_text filename in
  let fold_compile current_string seed =
      let k = String.index current_string ' ' in
      let affix = String.sub current_string 0 k in
      let rest = String.sub current_string (k+1) ((String.length current_string)-((String.length affix)+1)) in
      let rec chop str  = 
	if String.length str = 0 then [] 
	else 
	  let i = String.index str ':' in
	  let w = try String.index str ' ' with Not_found -> String.index str '\n' in
	  let sub1 = (String.sub str 0 i) and
	      sub2 = String.sub str (i+1) (w-i-1) in
	    ((sub1,sub2)::chop (String.sub str (w+1) ((String.length str) - (w+1))))
      in
	(affix,chop rest)::seed
  in
    List.fold_right fold_compile read_input [];;

(* MODIFY this function, by changing the name "text2.txt". Converts lists to monomials. *)
let text_from_file = List.map (fun (a,b) -> (a,L.list2monomial b)) (make_text2 "text2.txt");;

(* Different calls to type3learn(_with_printing) never affect each other. *)
print_string "TEXT TWO\n========\n";;
L.type3learn_with_printing text_from_file;;

