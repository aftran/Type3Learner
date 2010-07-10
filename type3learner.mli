module type T = sig
        (** An algorithm for learning morphological paradigms.  Implements Katya
         Pertsova's Type 3 Learner.  *)

        (** User-provided type for a morph(eme) or word. *)
        type morph

        (** User-provided type for a feature. *)
        type feature

        (** A monomial is a set of features. *)
        type monomial

        (** A table that maps integers to monomials. *)
        type lexeme

        (** A table that maps morphs to lexemes. *)
        type lexicon

        (** A map from monomials to misets. *)
        type table

        (** A set of indexed morphs ((morph, int) pairs).  *)
        type miset

        (** A list of (morph, monomial) pairs.  Type3learner's job is to produce
         * a hypothesis that explains the elements of any text. *)
        type text = (morph*monomial) list

        (** The module for the blocking-rule digraphs. *)
        module DG : Graph.Sig.P

        (** A directed graph for representing the blocking rules between
         * morph*ints. *)
        type digraph = DG.t

        (** The module for the free-variation graphs. *)
        module G : Graph.Sig.P

        (** An undirected graph for representing morph*ints that are in free
         * variation with each other somewhere. *)
        type graph = G.t

        (** type3learn t = (lex, v, br, s, p), where:
         *      lex is a lexicon,
         *      v is a graph of free-variation pairs,
         *      br is a digraph of blocking rules,
         *      s is a table that maps monomials to sets of morphs mitnessed in the
         *      environment of the monomial,
         *      p is a table that maps monomials to sets of morphs predicted in
         *      environments that are supersets of that monomial.
         * lex, v and br together specify the hypothesis that Type3Learner has acquired
         * given text t.  s and p contain no new information but improve the efficiency
         * of the learner. *)
        val type3learn : text -> lexicon *
                                 graph   *
                                 digraph *
                                 table   *
                                 table

        (** type3learn_with_printing t = type3learn t, but
         * type3learn_with_printing prints its input and hypothesis to stdout
         * every time it processes a text element. Suitable for users who want
         * to see what Type3learner does as it receives a text. *)
        val type3learn_with_printing : text -> lexicon *
                                               graph   *
                                               digraph *
                                               table   *
                                               table


        (** List of features in a monomial. *)
        val monomial2list : monomial     -> feature list

        (** Construct a monomial containing the listed features. *)
        val list2monomial : feature list -> monomial

        (** table x = a new table based on the list x of type
         (monomial*((morph*int) list)).  The list is of ordered pairs that map a
         monomial to a list of morphs.  The new table maps those monomials to their
         respective lists of morphs.. *)
        val list2table : (monomial*((morph*int) list)) list -> table

        (** Construct a set of morph*int pairs given a list of the morph*int pairs. *)
        val list2morphXint_set : (morph*int) list -> miset 

        (** Convert a feature into a human-readable string. *)
        val  feature2string : feature  -> string

        (** Convert a morph into a human-readable string. *)
        val    morph2string : morph    -> string

        (** Convert a monomial into a human-readable string. *)
        val monomial2string : monomial -> string

        (** Convert a lexicon into a human-readable string. *)
        val  lexicon2string : lexicon  -> string

        (** Convert a digraph into a human-readable string. *)
        val  digraph2string : digraph  -> string

        (** Convert a graph into a human-readable string. *)
        val  graph2string   : graph    -> string

        (** Print the human-readable string representation of a morph. *)
        val print_morph    : morph    -> unit

        (** Print the human-readable string representation of a feature. *)
        val print_feature  : feature  -> unit

        (** Print the human-readable string representation of a monomial. *)
        val print_monomial : monomial -> unit

        (** Print the human-readable string representation of a lexicon. *)
        val print_lexicon  : lexicon  -> unit

        (** Print the human-readable string representation of a digraph. *)
        val print_digraph  : digraph  -> unit

        (** Print the human-readable string representation of a graph. *)
        val print_graph    : graph    -> unit
        
        (** The empty lexicon. *)
        val empty_lexicon : lexicon

        (** The empty table. *) 
        val empty_table   : table

        (** The empty graph. *)
        val empty_graph   : graph

        (** The empty digraph. *)
        val empty_digraph : digraph

        (** Update the lexicon l, free-variation graph v, blocking rules br, and table t
         appropriately to account for observing the morph m in environment
         (monomial) e.  Returns a quadrouple: (the updated lexicon, the updated
         free-variation graph, the updated blocking rules, the updated table).*)
        val learn : lexicon  ->
                    graph    ->
                    digraph  ->
                    table    ->
                    table    ->
                    morph    ->
                    monomial -> lexicon *
                                graph   *
                                digraph *
                                table   *
                                table



end

module type ParamTypes = sig
        type feature
        type morph

        val compare_features : feature -> feature -> int
        val compare_morphs   : morph   -> morph   -> int

        val feature2string : feature -> string
        val   morph2string : morph   -> string
end

module Make (UserTypes : ParamTypes) : T
        with type feature = UserTypes.feature
        and  type morph   = UserTypes.morph
