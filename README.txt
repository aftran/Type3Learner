The folder contains these files:

Implementation of Type 3 learner in the appendix of Pertsova 2011. (Note that this learner does not work with privative unary features.  If you want to use privative features, you should encode them as binary features).
type3learner.ml
type3learner.mli

A sample demo file which can be modified to run the Type 3 learner on different inputs.  This file contains instructions on how to run the learner from a terminal. 
demo.ml

A file for automatically generating semantic spaces for a given set of feature, their values, and feature dependencies. 
semSpace.ml

A sample text-file that serves as an input to the learner
text2.txt
