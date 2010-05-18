(** An algorithm for learning morphological paradigms.  Implements Katya
 Pertsova's Type 3 Learner.  *)

(* TODO: Make these parameters provided by the user. *)
type morph
type feature

(** A monomial is a set of features. *)
type monomial

(** A lexeme is a table that maps integers to monomials. *)
type lexeme

(** A table that maps morphs to lexemes. *)
type lexicon

(** Maps monomials to misets. *)
type table

(** A set of indexed morphs ((morph, int) pairs).  *)
type miset

(** A directed graph for representing the blocking relations. *)
type digraph

(** An undirected graph for representing free-variation pairs. *)
type graph

(** A list of (morph, monomial) pairs. *)
type text

(** List of features in a monomial. *)
val m_elements : monomial -> feature list

(** Construct a set of morph*int pairs given a list of the morph*int pairs. *)
val morphXint_set : (morph*int) list -> miset 

(** table x = a new table based on the list x of type
 (monomial*((morph*int) list)).  The list is of ordered pairs that map a
 monomial to a list of morphs.  The new table maps those monomials to their
 respective lists of morphs.. *)
val table : (monomial*((morph*int) list)) list -> table

(** Update the lexicon l, free-variation graph v, blocking rules br, and table t
 appropriately to account for observing the morph m in environment (maximal
 monomial) e.  Returns a quadrouple: (the updated lexicon, the updated
 free-variation graph, the updated blocking rules, the updated table). *)
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
