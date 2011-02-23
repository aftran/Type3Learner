(* Copyright information at bottom of file. *)

(* In this example below, we create a Type3learner module, L, that considers
 * morphs and features to both be strings.
 *
 *    To compile and run in a terminal, type:
 *      ocamlc -I +ocamlgraph graph.cma type3learner.ml demo.ml -o demo
 *      ./demo
 *
 * For advanced users: Type3learner is a functor that lets you pick what types
 * morphs and features are.  This lets you treat any data type you want as a
 * morph and a feature, as long as you tell the functor about them in advance.
 * This makes it possible to store data about pronunciation or grammar in each
 * morph.  But for everyday use, it is easiest to work with morphs and features
 * both being strings. *)

module L = Type3learner.Make(struct
        type feature = string
        type morph   = string

        let compare_features = compare
        let compare_morphs   = compare

        let id o = o
        let feature2string = id
        let morph2string   = id
end)


(* First, make some monomials that can be used in texts that we feed the
 * learner. *)
(*1person sing*)
let m1 = L.list2monomial ["sp:+"; "part:+"; "group:-"; "anim:+"; "masc:+"];;
(*2person sing*)
let m2 = L.list2monomial ["sp:+"; "part:-"; "group:-"; "anim:+"; "masc:+"];;
let m3 = L.list2monomial ["sp:+"; "part:-"; "group:-"; "anim:+"; "masc:-"];;
(*3person sing*)
let m4 = L.list2monomial ["sp:-"; "part:-"; "group:-"; "anim:+"; "masc:-"];;
let m5 = L.list2monomial ["sp:-"; "part:-"; "group:-"; "anim:-"];;
let m6 = L.list2monomial ["sp:-"; "part:-"; "group:-"; "anim:+"; "masc:+"];;
(*1person pl*)
let m7 = L.list2monomial ["sp:+"; "part:+"; "group:+"; "anim:+"; "masc:+"; "minim:-"];;
let m8 = L.list2monomial ["sp:+"; "part:+"; "group:+"; "anim:+"; "minim:-"];;
let m9 = L.list2monomial ["sp:+"; "part:+"; "group:+"; "anim:+"; "minim:+"];;
(*2person pl*)
let m10 = L.list2monomial ["sp:+"; "part:-"; "group:+"; "minim:+"; "anim:+"; "masc:+"];;
let m11 = L.list2monomial ["sp:+"; "part:-"; "group:+"; "minim:-"; "anim:+"; "masc:-"];;
(*3person pl*)
let m12 = L.list2monomial ["sp:-"; "part:-"; "group:+"; "minim:-"; "anim:-"];;
let m13 = L.list2monomial ["sp:-"; "part:-"; "group:+"; "minim:+"; "anim:-"];;
let m14 = L.list2monomial ["sp:-"; "part:-"; "group:+"; "minim:-"; "anim:+"];;


(* A text (type L.text) is a list of (morph,monomial) pairs.  Since morphs are
 * strings, we can write a text as a list of (string,monomial) pairs. *)
let text = [("zero",m1);("en",m12);("t",m5);("t",m6);("e",m1);("st",m3);
            ("en",m8); ("en",m9); ("t",m10)];;

print_string "TEXT ONE\n========\n";;

(* type3learn_with_printing is exactly like type3learn, only it prints its state
 * after every step.  It is the easiest way to see what happens when you feed
 * the learner a certain text. *)
L.type3learn_with_printing text;;

(* Different calls to type3learn(_with_printing) never affect each other. *)
print_string "\n\nTEXT TWO\n========\n";;
L.type3learn_with_printing text;;




(*
 * This file is part of Type3Learner.
 * Copyright 2011 Mason Chua and Katya Pertsova.
 *
 * Type3Learner is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Type3Learner is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Type3Learner.  If not, see <http://www.gnu.org/licenses/>.
 *)
