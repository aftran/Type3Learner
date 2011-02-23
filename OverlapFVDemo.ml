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

module L = Type3learner.Make(struct
        type feature = string
        type morph   = string

        let compare_features = compare
        let compare_morphs   = compare

        let id o = o
        let feature2string = id
        let morph2string   = id
end)

let m1 = L.list2monomial [("A","+"); ("B","-")];;
let m2 = L.list2monomial [("A","+"); ("B","+")];;
let m3 = L.list2monomial [("A","-"); ("B","+")];;
let m4 = L.list2monomial [("A","-"); ("B","-")];;

let text = [("y",m1);("x",m2);("y",m3);("x",m4);("x",m1)];;

L.type3learn_with_printing text;;
