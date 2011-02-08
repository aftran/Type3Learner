module SemSpace = struct

        (*jan.2011*)
        (*functions for generating semantic spaces, i.e. all possible maximal monomials, and for generating the text*)


        (* open Printf;; *)


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
              let conc feat valList = List.map (fun a -> feat^":"^a) valList in 
              (conc f v)::(rangeSets rest);;

        let rec listSubset l1 l2 =
          match l1 with
              [] -> true
            |h::t -> if List.mem h l2 then listSubset t l2 else false;;
         
        (*filter out those feature-value combinations that are impossible  *)
        let eliminateDependencies ruledOut product = 
          let f ((n1,v1),(n2,v2)) env  = if (listSubset [(n1^":"^v1)] env) & (listSubset [(n2^":"^v2)] env) then false else true in
          let rec gothrough ruledOut p = 
            match ruledOut with
                [] -> p
              |h::t -> gothrough t (List.filter (f h) p)
          in
            gothrough ruledOut product;;

        let getSemanticSpace fR dep = eliminateDependencies dep (listCartesianProduct (rangeSets fR));;


        (*produce a text file that lists all possible environments, the input is the list of envs. *)
        let produce_txtfile space =
        let oc  = open_out "text.txt" in    (* create or truncate file, return channel *)
        let printList ls = 
          (List.iter (fun s -> Printf.fprintf oc "%s " s) ls; Printf.fprintf oc "\n")
        in
          (List.iter printList space);
        close_out oc;;

        (*read in the txt file "filename"*)
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

        (* Generate the text for the learner *)
        let make_text filename envs = 
          let read_input = input_text filename in
          let white_space_i str = String.index str ' ' in
          let fold_compile affixList env (i,seed) =
              let current_string = List.nth affixList i in
              let affix = String.sub current_string 0 (white_space_i current_string) in
                (i+1,((affix,env)::seed))
          in
            snd(List.fold_right (fold_compile read_input) envs (0,[]));;

end
