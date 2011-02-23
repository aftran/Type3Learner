(*
 * This file is part of Type3Learner.
 * Copyright 2011 Katya Pertsova.
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

(*functions for generating semantic spaces, i.e. all possible maximal monomials, and for generating the text.  Compile all functions in this file.  To generate a text file that will serve as an input to the learner, look at the examples at the end of this file and substitute your own values for the relevant variables.*)

open Printf;;

(* sizes of the value sets in order *)
let valSizes fR = List.map (function x -> List.length (snd x)) fR;;

(* total number of possible environments *)
let noOfValuations fR =  match fR with
    [] -> 0
  | _ -> List.fold_left (function x -> (function y -> (List.length (snd y))*x)) 1 fR;;

let rec listCartesianProduct =
  (* first a map-cons function, cons'ing prefix onto each element, and letting us specify the final tail *)
  let rec mapCons finalTail prefix = function 
      [] -> finalTail 
    | y::ys -> (prefix::y)::(mapCons finalTail prefix ys)
  in 
  (* then a function that cons's a list (of 'prefixes') onto suffixes, each suffix a list *)
  let rec prefixAll suffixes = function 
      [] -> []
    | x::xs -> mapCons (prefixAll suffixes xs) x suffixes 
  in
  (* and now we define the listCartesian product easily *)
    function
	[] -> []
      | list::[] -> List.map (function x -> [x]) list (* singletons *)
      | list::lists -> prefixAll (listCartesianProduct lists) list;;

let rec rangeSets fR = match fR with
    [] -> []
  | (f,v)::rest -> 
      let conc feat valList = List.map (fun a -> (feat,a)) valList in 
      (conc f v)::(rangeSets rest);;

let rec listSubset l1 l2 =
  match l1 with
      [] -> true
    |h::t -> if List.mem h l2 then listSubset t l2 else false;;
 
(*filter out those feature-value combinations that are impossible  *)
let eliminateDependencies ruledOut product = 
  let f ((n1,v1),(n2,v2)) env  = if (listSubset [(n1,v1)] env) & (listSubset [(n2,v2)] env) then false else true in
  let rec gothrough ruledOut p = 
    match ruledOut with
	[] -> p
      |h::t -> gothrough t (List.filter (f h) p)
  in
    gothrough ruledOut product;;

let getSemanticSpace fR dep = eliminateDependencies dep (listCartesianProduct (rangeSets fR));;

(*produce a text file that lists all possible environments, the input is the list of envs. *)
let produce_txtfile space name =
let oc  = open_out name in    (* create or truncate file, return channel *)
let printList ls = 
  (List.iter (fun (s1,s2) -> Printf.fprintf oc "%s:%s " s1 s2) ls; Printf.fprintf oc "\n")
in
  (List.iter printList space);
close_out oc;;

(* ----Instructions for the user------------------------- *)

(* Define features and their values like this:  *)
let fR2 = [("participant",["+";"-"]);
	      ("speaker",["+";"-"]);
	      ("class",["human";"nonhuman"]);
	      ("number",["sg";"pl"])
	     ];;

(*specify pairs of feature values that cannot co-occur in a list like this:*)
let ruledOut = [(("participant","+"),("class","nonhuman"));(("participant","-"),("speaker","+"))];;

(*run this function to produce a text file "text.txt" (you can change this name below)  which contains the list of all possible environments*)
let space = getSemanticSpace fR2 ruledOut in produce_txtfile space "text.txt";;

(*Open the file "text.txt" and insert affixes associated with each environment *immediately to the left of the environment as one string followed by a space. Save the resulting file under some name, and use that file as input to the learner following the instructions in demo.ml *)



