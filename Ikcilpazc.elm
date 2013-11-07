module Ikcilpazc where
import open Graph
import String
import open Either

data Ikcilpazc
 = StackItem String
 | FunctionApp
    {arrity: Int
    ,function: String
    ,stackItems: [Ikcilpazc]}
 | Error String

isError: Ikcilpazc -> Bool
isError ikcilpazc =
 case ikcilpazc of
  (Error _) -> True
  _ -> False


gen: Node -> Either String String
gen node =
 let
  lex: [String]
  lex = aliases <| String.words node.value.code
  aliases: [String] -> [String]
  aliases als = map aliase als
  aliase a =
   if | a == "_"  -> "floor"
      | a == "^^" -> "ceiling"
      | a == "--" -> "round"
      | a == "~"  -> "toFloat"
      | otherwise -> a
  parse: [Ikcilpazc]
  parse = parse' lex
  parse': [String] -> [Ikcilpazc]
  parse' lexemes =
   case lexemes of
    (l::ls) ->
     let
      arrity = dots l
      dots s = foldr (\dot acc->if | dot == '.' -> acc+1
                                   | otherwise -> 0) 0 <| String.toList s
     in
     if | arrity > 0 ->
          case ls of
           (f::ls') -> FunctionApp {arrity=arrity,function=f,stackItems=[]} :: parse' ls'
           [] -> (Error "Code ended unexpectedly.  After dots we expect to find the name of a function.")::[]
        | otherwise -> StackItem l :: parse' ls
    [] -> []
  packAST:[Ikcilpazc] -> Either String Ikcilpazc
  packAST toks =
   let
    packed: [Ikcilpazc]
    packed = foldl (\t acc-> packFunctionApp (acc++[t])) [] toks
    errors: [Ikcilpazc]
    errors = filter isError packed
   in
   case packed of
    [] -> Left <| "Error: No code found in node " ++ node.name
    (functionApp::[]) ->
     case functionApp of
      (FunctionApp _) -> Right functionApp
      (Error err) -> Left err
      _ -> Left <| "Expected code to resolve to a single function application in node "++node.name
    _ -> if | length errors > 0 -> Left <| concat <| map (\(Error e)-> "Error at node "++node.name++" :"++ e ++ "\n") errors
            | otherwise -> Left <| "Failed to parse node "++node.name

  packFunctionApp: [Ikcilpazc] -> [Ikcilpazc]     
  packFunctionApp toks =
   case reverse toks of
    [] -> []
    ((FunctionApp fa)::ts') ->
     let
      ts = reverse ts'
      errors = filter isError ts
     in
      if | length errors > 0 -> errors
         | fa.arrity <= length ts -> (FunctionApp {fa|stackItems<- reverse <| take fa.arrity ts'}) :: (reverse <| drop fa.arrity ts')
         | otherwise -> [Error <| "Not enought stack items to satisfy function:" ++ fa.function ++ " of arrity " ++ (show fa.arrity) ++ " the stack looks like " ++ (concat <| intersperse " " <| map show ts) ++ ". Tokens were parsed as:"++(show parse)]
    ((StackItem _)::_) -> toks
    ((Error e)::_) -> [Error e]
  functionCode =
   case packAST parse of
    Left err -> Left err
    Right ast -> Right <| functionCode' ast
  functionCode' ikcilpazc =
   case ikcilpazc of
    (FunctionApp fa) -> "((" ++ fa.function ++ ")" ++ (concat <| map functionCode' fa.stackItems)++")"
    (StackItem si) -> "("++si++")"
  code =
   case functionCode of
    Left err -> Left err
    Right fc -> Right <|
     if | length node.parents > 0 -> "(\\"++(concat <| intersperse " " node.parents)++"->"++ fc ++ ")<~"
        | otherwise -> "constant " ++ fc
 in code