{-
The Graph module provides the basic data types used by Graphical ELM.
It also provides utility functionality useful for sorting, levelizing and editing a graph.

INDEX:
- data types
- commands that operate on an entire graph
- commands that operate on a single node

copyright (c) 2013 Timothy Hobbs
Released under the terms of the GNU AGPL v 3 or later.
See more licence information at the end of the file, and or in the file COPYING.
-}
module Graph where
import open List

{- data types -}
type Graph = [Node]
data Language = ElmLang | Ikcilpazc
type Value = 
 {code: String
 ,language: Language}
type Node =
 {parents: [String]
 ,name: String
 ,value: Value}


sampleGraph =
 [{parents=[]
 ,name="main"
 ,value=defaultValue}]

emptyNode =
 {parents = []
 ,name    = ""
 ,value   = defaultValue}

defaultValue =
 {code = ""
 ,language = ElmLang}

data Ord = Gt | Lt | Eq

{- commands that operate on an entire graph -}

naiveSortBy: (a -> a -> Ord) -> [a] -> [a]
naiveSortBy comp al = naiveSortBy' comp al [] []
naiveSortBy' comp source bucketLess bucketMore =
 case source of
  (a1::a2::ass) -> case comp a1 a2 of
                   Gt -> naiveSortBy' comp (a2::ass) (bucketLess) (a1::bucketMore)
                   Lt -> naiveSortBy' comp (a2::ass) (bucketLess++[a1]) bucketMore
                   Eq -> naiveSortBy' comp (a2::ass) (bucketLess++[a1]) bucketMore
  (a::[]) -> case (bucketLess,bucketMore) of
              ([],[]) -> [a]
              ((bl::bls),[]) -> case comp a (last bucketLess) of
                                Gt -> bucketLess++[a]
                                Eq -> bucketLess++[a]
                                Lt -> naiveSortBy' comp (a::bucketLess) [] []
              (_,_) -> naiveSortBy' comp (bucketLess++[a]++bucketMore) [] []
  [] -> case bucketMore of
         [] -> bucketLess
         _ -> naiveSortBy' comp (bucketLess++bucketMore) [] []

--VERY NAIVE IMPLEMENTATION, very inefficient, goes into infinite loop in case of graph cyclicity.
sortGraph: Graph -> Graph
sortGraph g = reverse <| sortGraph' [] <| naiveSortBy (\n1 n2-> if | length n1.parents > length n2.parents -> Gt
                                                                   | length n1.parents == length n2.parents -> Eq
                                                                   | otherwise -> Lt) g

sortGraph' alreadySorted ns =
 case ns of
  (node::nodes) ->
    let
     inAlreadySorted n = any (\asn->asn.name==n) alreadySorted
    in
    if | all inAlreadySorted node.parents || node.parents==[] -> sortGraph' (node::alreadySorted) nodes
       | otherwise ->
          let
           inRemainder n = any (\asn->asn.name==n) nodes
           parentsInRemainder = any inRemainder node.parents
          in
          if | parentsInRemainder -> sortGraph' alreadySorted <| nodes ++(node::[])
             | otherwise -> sortGraph' (node::alreadySorted) nodes -- stop trying to sort the node if one of it's parents does not exist.
  [] -> alreadySorted

levelizeGraph: Graph -> [[Node]]
levelizeGraph g =
  foldr
   (\node acc->
     case node.parents of
      [] -> addToTop (node) acc
      (_::_) -> flipThroughAndAdd parentsSatisfied node node.parents acc)
   []
   <| reverse <| sortGraph g

addToTop: Node -> [[Node]] -> [[Node]]
addToTop node nodes =
 case nodes of
   [] -> [[node]]
   (ns::nss) -> (node::ns)::nss

flipThroughAndAdd: ([Node]->[acc]->[acc]) -> Node -> [acc] -> [[Node]] -> [[Node]]
flipThroughAndAdd f toAdd acc ayss' =
 case (ayss',acc) of
  ((ays::ayss),[]) -> (toAdd::ays) :: ayss
  ((ays::ayss),_)  -> ays :: flipThroughAndAdd f toAdd (f ays acc) ayss
  ([],_) -> [[toAdd]]

parentsSatisfied: [Node] -> [String] -> [String]
parentsSatisfied nodesInLevel unsatisfiedDependencies =
 let satisfies potentialDeps unsatisfiedDep =
       if | elem unsatisfiedDep potentialDeps -> Nothing
          | otherwise -> Just unsatisfiedDep
     unsatisfiedDependencies' = justs <| map (\unsatisfiedDep ->satisfies (map .name nodesInLevel) unsatisfiedDep) unsatisfiedDependencies
 in
 unsatisfiedDependencies'

elem: a -> [a] -> Bool
elem a ays = any (\a1->a1==a) ays

{- commands that operate on a single node -}

replaceNode: Node -> Graph -> Graph
replaceNode node gnodes' =
 if node.name == ""
 then gnodes'
 else
  case gnodes' of
   (gnode::gnodes) ->
    if | gnode.name==node.name -> node :: replaceNode node gnodes
       | otherwise -> gnode :: replaceNode node gnodes
   [] -> []

addNode: Node -> Graph -> Graph
addNode node gnodes =
 if node.name == ""
  then gnodes
  else node::gnodes

renameNode: String -> String -> Graph -> Graph
renameNode oldName newName gnodes =
 let
  rename: String -> String
  rename name = 
   if | name==oldName -> newName
      | otherwise -> name
  renameNode' node = {node|name<-rename node.name}
  renameParents node =
   {node|parents<-map rename node.parents}
 in
 map renameNode' gnodes |>
 map renameParents

deleteNode: String -> Graph -> Graph
deleteNode nodeToDelete gnodes' =
 case gnodes' of
  (gnode::gnodes) ->
   if | gnode.name==nodeToDelete -> gnodes
      | otherwise -> gnode:: deleteNode nodeToDelete gnodes
  [] -> []
{-
Graphical ELM - A program for editing graphs as graphs.
Visually, Architecturally

    Copyright (C) 2013  Timothy Hobbs <timothyhobbs@seznam.cz> thobbs.cz

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}