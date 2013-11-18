{-
This module provides commands for looking up x y coordinates in a levelized graph, so that we can refer to a node by it's coordinates.

copyright (c) 2013 Timothy Hobbs
Released under the terms of the GNU AGPL v 3 or later.
See more licence information at the end of the file, and or in the file COPYING.
-}
module LevelizedGraphs.Coordinates where
{- Standard library imports -}
import List
{- Internal modules -}
import LevelizedGraphs.Graph as Graph

type Coord = {x:Int,y:Int}

coordinates: [[Graph.Node]] -> [(Coord,Graph.Node)]
coordinates levelizedGraph = coordinates' 0 levelizedGraph |> concat

coordinates': Int -> [[Graph.Node]] -> [[(Coord,Graph.Node)]]
coordinates' n nodes' =
 case nodes' of
  (nodes::nodess) -> (coordinates'' nodes 0 n)::coordinates' (n+1) nodess
  [] -> []
coordinates'': [Graph.Node] -> Int -> Int -> [(Coord,Graph.Node)]
coordinates'' nodes' x y =
 case nodes' of
  (node::nodes) -> ({x=x,y=y},node)::coordinates'' nodes (x+1) y
  [] -> []

getCoord: String -> [(Coord,Graph.Node)] -> Coord
getCoord name coords =
 case coords of
  ((coord,node)::coords') ->
   if | name==node.name -> coord
      | otherwise -> getCoord name coords'
  [] -> {x=0,y=0}

getNode: Coord -> [(Coord,Graph.Node)] -> Graph.Node
getNode coordToGet coords =
 case coords of
  ((coord,node)::coords') ->
   if | coordToGet==coord -> node
      | otherwise -> getNode coordToGet coords'
  [] -> Graph.emptyNode

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