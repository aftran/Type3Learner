
(* M: *)
(*
type morph = A | Lot | Of | Possible | Morphs
 *)
type morph = string

(* These data types are enumerations of possible feature values: *)
type tense = Past | Nonpast                     (* F_tense *)
type person = First | Second | Third            (* F_person *)
type quantity = Singular | Plural | Mass        (* F_quantity *)
type gender = Animate | Inanimate               (* F_gender *)
                                                (* F_telic is bool *)
(* The set F: *)
type feature =
        | Tense of tense
        | Person of person 
        | Quantity of quantity
        | Gender of gender
        | Telic of bool

(* Unfortunately, we type each feature's name three different times.  I'd like
 * a better way to define a feature and its allowed values in one go. *)
