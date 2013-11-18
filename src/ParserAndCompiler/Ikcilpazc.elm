{-
This module provides utilities for compiling Ikcilpazc(pronounced Ixilpach) to ELM code.

Ikcilpazc is a reverse polish style programming language.  That is, it's stack based.
It's name is a play on the name of the origional author of the ELM programming language Evan Czaplicki.

It has very simple semantics:

All words are separated by spaces.

If a word stands alone, it is pushed onto the stack as a value.

A series of dots removes that many values from the stack, and applies them to the function that follows.

3 4 .. +

- The first two words push the values 3 and 4 to the stack.
- The two dots, take two values from the stack, and apply them to the function +

The code: 3 4 .. + compiles to ((+)(3)(4)) in Elm.

There are four aliases that help with arithmatic: _,--,^^,~

_  is converted to floor
-- is converted to round
^^ is converted to ceiling
~  is converted to toFloat

Thus what in Elm might look like:

2 * (toFloat x) / (toFloat y)

In Ikcilpazc looks like:

2 x . ~ y . ~ .. / .. *


copyright (c) 2013 Timothy Hobbs
Released under the terms of the GNU AGPL v 3 or later.
See more licence information at the end of the file, and or in the file COPYING.
-}
module ParserAndCompiler.Ikcilpazc where
{- Standard library imports -}
import String
import open Either
{- Internal modules -}
import LevelizedGraphs.Graph as Graph

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

gen: Graph.Node -> Either String String
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