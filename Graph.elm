module Graph where
import open List

type Graph = [Node]
data Language = ElmLang | FooLang
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
levelizeGraph g = levelizeGraph' [] <| sortGraph g
levelizeGraph': [[Node]] -> Graph -> [[Node]]
levelizeGraph' ls ns =
 case (ls,ns) of
   ((thisLevel::higherLevels),(node::nodes))->
    let
     hasParentInThisLevel = any (\pp-> any (\p->p==pp.name) node.parents) thisLevel
    in
    if | hasParentInThisLevel -> levelizeGraph' ([node]::thisLevel::higherLevels) nodes
       | otherwise -> levelizeGraph' ((node::thisLevel)::higherLevels) nodes
   ([],(node::nodes)) ->  levelizeGraph' [[node]] nodes
   (levels,[]) -> levels

addOrReplaceNode: Node -> Graph -> Graph
addOrReplaceNode node gnodes' =
 if node.name == ""
 then gnodes'
 else
  case gnodes' of
   (gnode::gnodes) ->
    if | gnode.name==node.name -> addOrReplaceNode node gnodes
       | otherwise -> gnode :: addOrReplaceNode node gnodes
   [] -> node :: []
