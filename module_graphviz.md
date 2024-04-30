# Conversion en type `Graph gr`

- Un graph `Graph gr` est de type => `gr a b`
  où `a` est le type des labels des nœuds 
     `b` est le type des labels des arc
- On peut créer un tel graph à partir de la fonction : 
  `mkGraph :: [LNode a] -> [LEdge b] -> gr a b `
  où `LNode` tel que `type LNode a = (Node, a)`, un nœuds avec un label 
     `LEdge` tel que `type LEdge b = (Node, Node, b)`, un nœuds avec un label

# Conversion d’un Graph en Dot

- Objectif obtenir une fonction du type `gr a b -> DotGraph Node`. `DotGraph` 
  est le type d’un graph que l’on peut représenter en Dot. Car une fois en 
  ce type, on peut utiliser la fonction `printDotGraph` qui permet de 
  transformer ledit graphe code Dot.
  Cette fonction est 
  `graphToDot :: (Ord cl, Graph gr) => GraphvizParams Node nl el cl l -> gr nl el -> DotGraph Node`
  Avec `GraphvizParams Node nl el cl l` les paramettres de la représentation 
  en graphe.
- Avec la descritions du type `GraphvizParams` :
```
Defines the parameters used to convert a Graph into a DotRepr.

A value of type GraphvizParams n nl el cl l indicates that the Graph has a node 
type of n, node labels of type nl, edge labels of type el, corresponding 
clusters of type cl and after clustering the nodes have a label of type l 
(which may or may not be the same as nl).

The tuples in the function types represent labelled nodes (for (n,nl) and (n,l)) 
and labelled edges ((n,n,el); the value (f,t,ftl) is an edge from f to l with a 
label of ftl). These correspond to LNode and LEdge in FGL graphs.

The clustering in clusterBy can be to arbitrary depth.

Note that the term "cluster" is slightly conflated here: in terms of 
GraphvizParams values, a cluster is a grouping of nodes; the isDotCluster 
function lets you specify whether it is a cluster in the Dot sense or just a 
sub-graph.
```

- La fonction `nonClusteredParams :: GraphvizParams n nl el () nl `, permet de 
  ne pas se soucier des `Cluster`. 
