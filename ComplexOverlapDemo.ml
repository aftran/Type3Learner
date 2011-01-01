module L = Type3learner.Make(struct
        type feature = string
        type morph   = string

        let compare_features = compare
        let compare_morphs   = compare

        let id o = o
        let feature2string = id
        let morph2string   = id
end)

let m1 =  L.list2monomial [("A","1"); ("B","-")];; 
let m2 =  L.list2monomial [("A","2"); ("B","-")];; 
let m3 =  L.list2monomial [("A","3"); ("B","-")];; 
let m4 =  L.list2monomial [("A","1"); ("B","+")];;
let m5 =  L.list2monomial [("A","2"); ("B","+")];;
let m6 =  L.list2monomial [("A","3"); ("B","+")];;

let text = [("y",m1);("y",m6);("w",m5);("x",m4);("y",m4)];; 

L.type3learn_with_printing text;;
