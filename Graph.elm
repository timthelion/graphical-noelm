module Graph where
import open List

type Graph = [Node]
data Language = ElmLang | Ikcilpazc
type Value = {code:String,language:Language}
type Node = {parents:[String],name:String,value:Value}

emptyNode = {parents=[],name="",value=defaultValue}
defaultValue={code="",language=ElmLang}

data Ord = Gt | Lt | Eq

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
