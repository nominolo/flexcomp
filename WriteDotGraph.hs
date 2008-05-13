module WriteDotGraph (writeDotGraph, writeDotGraphGrouped, writeDotGraphLeveled, Style(..)) where

import List (nub)
import System.Cmd ( system )

data Style 
    = Label String
    | Color String
    | Pos Int Int
    deriving (Eq,Ord,Read)

instance Show Style where
    show (Label l) = "label = " ++ show l
    show (Color c) = "color = " ++ show c
    show (Pos x y) = "pos = \"" ++ show x ++ "," ++ show y ++ "\""

writeDotGraph :: (Show node, Eq node) =>
                 [(node, [Style])] ->
                 [(node, [Style], node)] -> String
writeDotGraph nodes edges = 
  unlines (
      [header
      ,graphDefaultAtribs
      ,nodeDefaultAtribs
      ,edgeDefaultAtribs]
    ++ map makeNode nodes
    ++ map makeEdge edges
    ++ [footer]
  )
  where -- nodes =  List.nub $ concat [ [a,b] | (a,_,b) <- edges ]
        makeNode (name, nstyle) = 
            "\t" ++ show (show name) ++ " " ++ show nstyle ++ " ;"
        makeEdge (node1, estyle, node2) = 
            "\t" ++ show (show node1) ++ " -> " 
                 ++ show (show node2) ++ show estyle ++ ";"

writeDotGraphGrouped :: (Show node, Eq node) =>
                 [(String,Style,[(node, [Style])])] ->
                 [(node, [Style], node)] -> String
writeDotGraphGrouped nodeGroups edges = 
  unlines (
      [header
      ,graphDefaultAtribs
      ,nodeDefaultAtribs
      ,edgeDefaultAtribs]
    ++ concatMap makeGroup nodeGroups 
    ++ map makeEdge edges
    ++ [footer]
  )
  where -- nodes =  List.nub $ concat [ [a,b] | (a,_,b) <- edges ]
        makeGroup (name, gstyle, nodes) = 
            ["\tsubgraph " ++ show name ++ " {\n\t" ++ show gstyle ++ ";"] ++
            map makeNode nodes ++ ["label="++show(show name),"\t}"]
        makeNode (name, nstyle) = 
            "\t" ++ show (show name) ++ " " ++ show nstyle ++ " ;"
        makeEdge (node1, estyle, node2) = 
            "\t" ++ show (show node1) ++ " -> " 
                 ++ show (show node2) ++ show estyle ++ ";"

writeDotGraphLeveled :: (Show node, Eq node) =>
                        [[(node, [Style])]]
                     -> [(node, [Style], node)] -> String
writeDotGraphLeveled levels edges =
  unlines (
      [header
      ,"{",levelStr (length levels)++ "[color=white]","}"
      ,graphDefaultAtribs
      ,nodeDefaultAtribs
      ,edgeDefaultAtribs
      ]
    ++ concatMap makeLevel (zip levels [1..])
    ++ map makeEdge edges
    ++ [footer]
  )
  where -- nodes =  List.nub $ concat [ [a,b] | (a,_,b) <- edges ]
     makeLevel (nodes,l) = 
         ["\t{","\trank = same; Lvl"++show l++";"] ++
         map makeNode nodes ++ ["\t}"]
     makeNode (name, nstyle) = 
         "\t" ++ show (show name) ++ " " ++ show nstyle ++ " ;"
     makeEdge (node1, estyle, node2) = 
         "\t" ++ show (show node1) ++ " -> " 
              ++ show (show node2) ++ show estyle ++ ";"
     levelStr 0 = ""
     levelStr 1 = "\tnode [color=white,fontcolor=white]; Lvl1"
     levelStr n = levelStr (n - 1) ++ " -> Lvl" ++ show n

header = "digraph g {" 
footer = "}"

graphDefaultAtribs = "\tgraph [fontsize=14, fontcolor=black, color=black];" -- ++ "\n\tranksep=2;\n" 
nodeDefaultAtribs  = "\tnode [label=\"\\N\", width=\"0.75\", shape=ellipse];"
edgeDefaultAtribs  = "\tedge [fontsize=10];"

showGraph fname s = do
  writeFile (fname++".dot") $ s
  putStrLn "Runnig Dotty ..."
  system $ "dot -Tsvg "++fname++".dot > "++fname++".svg"
  system ("eog "++fname++".svg") >>= print  