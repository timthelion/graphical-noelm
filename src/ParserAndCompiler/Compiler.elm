module ParserAndCompiler.Compiler where
{- base libraries -}
import Either

{- Internal modules -}
import LevelizedGraphs.Graph as Graph
import State.EditorState as EditorState
import ParserAndCompiler.Compiler.Ikcilpazc as Ikcilpazc
import ParserAndCompiler.Types as Types
import ParserAndCompiler.Constants as Constants

version: Types.Version
version = {major=1,minor=0}

generateCode: EditorState.EditorState -> String
generateCode ges
 =
 let
  nodeCodesE = map generateNodeCode ges.graph
  errors = Either.lefts nodeCodesE
  nodeCodes = Either.rights nodeCodesE
 in
 if | length errors == 0 ->
      Constants.makeHeader version ++ "\n" ++
      Constants.startMisc ++ "\n" ++
      ges.misc ++ "\n" ++
      Constants.endMisc ++ "\n" ++
      (concat <| nodeCodes)
    | otherwise -> concat <| errors

generateNodeCode: Graph.Node -> Either.Either String String
generateNodeCode node =
 let
  generatedCodeEither =
   case node.value.language of
    Graph.LiftElm -> Either.Right (node.value.code++(concat <| intersperse "~" node.parents))
    Graph.ElmLang -> Either.Right node.value.code
    Graph.Ikcilpazc -> Ikcilpazc.gen node
  ntype =
   case node.value.ntype of
    Just ntype -> node.name ++ Constants.nameObfuscator ++ " : " ++ ntype ++  "\n"
    Nothing -> ""
 in
 case generatedCodeEither of
  Either.Right code ->
      Either.Right 
   <| ntype

   ++ node.name
   ++ Constants.nameObfuscator
   ++ "= (\\"++(concat <| intersperse " " node.parents)
   ++ "->" ++ code ++ ")"
   ++ (concat <| intersperse " " <| map (\p->p++Constants.nameObfuscator) node.parents)
   ++ "{-_language_"
   ++ show node.value.language
   ++ "_base_code_#####$#" ++ node.value.code
   ++ "#$#####_parents_"
   ++ (concat <| intersperse "," node.parents)
   ++ "-}\n"
  Either.Left err -> Either.Left err