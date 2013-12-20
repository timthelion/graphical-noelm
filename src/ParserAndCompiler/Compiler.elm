module ParserAndCompiler.Compiler where
{- base libraries -}
import Either
import String

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
      Constants.mainRealiase ++ "\n"++
      (concat <| nodeCodes)
    | otherwise -> concat <| errors

generateNodeCode: Graph.Node -> Either.Either String String
generateNodeCode node =
 let
  generatedCodeEither =
   case node.value.language of
    Graph.LiftElm -> Either.Right (node.value.code++(String.concat <| intersperse "~" node.parents))
    Graph.ElmLang -> Either.Right node.value.code
    Graph.Ikcilpazc -> Ikcilpazc.gen node
  ntype =
   case node.value.ntype of
    Just ntype -> node.name ++ Constants.nameObfuscator ++ " : " ++ ntype ++  "\n"
    Nothing -> ""
 in
 case generatedCodeEither of
  Either.Right generatedCode ->
      Either.Right 
   <| ntype

   ++ node.name
   ++ Constants.nameObfuscator
   ++ " ="
   ++ (if | length node.parents > 0 ->
               "(\\"
            ++ (concat <| intersperse " " node.parents)
            ++ "->" ++ generatedCode ++ ")"
          | otherwise -> generatedCode)
   ++ (concat <| intersperse " " <| map (\p->p++Constants.nameObfuscator) node.parents)
   ++ "{-_language_"
   ++ show node.value.language
   ++ "_base_code_#####$#" ++ node.value.code
   ++ "#$#####_parents_"
   ++ (concat <| intersperse "," node.parents)
   ++ "-}\n"
  Either.Left err -> Either.Left err