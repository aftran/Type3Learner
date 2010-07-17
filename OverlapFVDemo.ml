module L = Type3learner.Make(struct
        type feature = string
        type morph   = string

        let compare_features = compare
        let compare_morphs   = compare

        let id o = o
        let feature2string = id
        let morph2string   = id
end)

let m1 = L.list2monomial ["A+"; "B-"];;
let m2 = L.list2monomial ["A+"; "B+"];;
let m3 = L.list2monomial ["A-"; "B+"];;
let m4 = L.list2monomial ["A-"; "B-"];;

let text = [("y",m1);("x",m2);("y",m3);("x",m4);("x",m1)];;

L.type3learn_with_printing text;;
