module Main where
import Data.Char
import System.IO
import Data.List
import Data.Ord

-- Type  definitions
type Name = String
data Edge = Edge { node::Node, weight::Integer } deriving (Show, Eq)
type Node  =  Name
type Graph = [(Node, [Edge])]

--reads from graph.txt and store the string into csvFile variable.

main :: IO ()
main  = do  putStrLn "Which file.txt do you want to load: "
            file <- getLine
            csvFile <- readFile file
            let graph =  start csvFile
            putStrLn "--->Graph created<---"
            putStrLn "*Number of Nodes: "
            print (nodeCount graph)
            putStrLn "*Number of Edges: "
            print(edgeCount graph)
            printGraph graph
            putStrLn "-----------------------------------"
            putStrLn "--->Kruskal Spannig Tree Algorithm<---"
            let spanGraph = kruskal graph
            putStrLn "*Number of Nodes: "
            print (nodeCount spanGraph)
            putStrLn "*Number of Edges: "
            print(edgeCount spanGraph)
            printGraph spanGraph
            putStrLn "-----------------------------------"
            putStrLn "--->Reverse-Delete Spannig Tree Algorithm<---"
            let spanGraphRev = reverse_delete graph
            putStrLn "*Number of Nodes: "
            print (nodeCount spanGraphRev)
            putStrLn "*Number of Edges: "
            print(edgeCount spanGraphRev)
            printGraph spanGraphRev




--Than creat a function readData that is used to store into entry
--the read string  before divided into fields thanks to "lines" and
--"words" function from Data.List . Than the funcion createGraph is called
--and the graph is created.
--(readData is a function that takes an array and gives a tupla like (n1,n2)w) where w is a float wheigh)

start :: String -> Graph
start csvFile = createGraph entry
                where readData [n1, n2, w] = ((n1, n2), read w :: Integer)
                      entry = map (readData . words)(lines csvFile)


--the "nub" function from Data.List  function removes duplicate elements from a list.
--In particular, it keeps only the first occurrence of each element. This is perfect for not
--having duplicates in adjency list. Nodes is the list of all nodes nodes with no duplicates
--generated thanks to nub ("fst.fst" takes the first element of the fist element of the tupla es).

createGraph :: [((String, String), Integer)] -> Graph
createGraph entry = map (\node -> (node, edgesList entry node)) nodes
                    where nodes = nub( map(fst.fst)(entry) )
                          edgesList entry node = map ( \((_,n),weight) -> Edge n weight) (sameNode)
                                where sameNode= filter ( \((n,_),_) -> node == n) (entry)             --list of all connected elements to a node





printGraph :: Graph -> IO()
printGraph [] = return()
printGraph (x:xs) = do putStrLn (show x)
                       printGraph xs

--takes all edges for the specific node into the graph thanks to snd that works as fst, but takes
--the second element of a tupla. The filter use a lambda expression for check and take
--only the element wanted
edgesList :: Graph -> Node -> [Edge]
edgesList graph node = if( filter ( \(findNode, _) -> findNode == node) (graph) /= []) then snd ( head ( filter ( \(findNode, _) -> findNode == node) (graph)))
                        else []




--given an edge returns the entry associated to that edge
findEntry :: Edge -> Graph -> (Node, Edge)
findEntry e graph = ((fst entry),e)
                    where entry = head(filter ( \(_ , es) -> e `elem` es) (graph))



--uses the edgesList to map all nodes adj to nod
adjList :: Graph -> Node -> [Node]
adjList graph nod =  map(node)(edgesList graph nod)


listOfNodes :: Graph -> [Node]
listOfNodes [] = []
listOfNodes g = nub([fst (head g)] ++ adjList g (fst(head g) ) ++ listOfNodes(tail g))

--number of all nodes in graph
nodeCount :: Graph -> Int
nodeCount g = length(listOfNodes g)

--length of graph
graphLength ::  Graph -> Int
graphLength graph = length graph

--counts the number of edges in a graph
edgeCount :: Graph -> Int
edgeCount g = length(allEdgesList g)

--returns a list of all edges in the graph
allEdgesList :: Graph -> [Edge]
allEdgesList g = concat(map snd g)

--sorts the list of edges for weight
sortEdges :: [Edge] -> [Edge]
sortEdges es = sortBy(comparing weight) es


-------------------------------------- Strong connected Components Algorithm --------------------------------------


--given an edge, it return the corresponding start node
getHeadNode :: Graph -> Edge -> Node
getHeadNode graph edge = fst$(head (filter (\(_,edges) ->  edge `elem` edges) (graph)))

--get all incident edges on a node
getIncidentEdges :: Graph -> Node -> [Edge]
getIncidentEdges graph node = filter (\(Edge n w) -> n == node)(allEdgesList graph)

-- return the incident nodes of an edge
getIncidentNodes :: Graph ->Edge -> [Node]
getIncidentNodes graph e@(Edge n w) = [getHeadNode graph e]++[n]



--get all incident edges from a list of nodes
getAllIncidentEdges :: Graph -> [Node] -> [Edge]
getAllIncidentEdges graph nodes = nub(incNodes ++ nodesInc)
                        where incNodes = concatMap (getIncidentEdges graph) $ nodes
                              nodesInc = concatMap (edgesList graph) $ nodes



--returns nodes A B of a list of edges
nodesInEdges :: Graph -> [Edge] -> [Node]
nodesInEdges graph es = nub. concatMap(getIncidentNodes graph)$es


--return the cfc of where the nodes belong
getCfc :: Graph -> [Node] -> Graph
getCfc graph ns = if length ns == length nodes then cfcGraph graph [] incidents else getCfc graph nodes
                    where
                        incidents = getAllIncidentEdges graph ns
                        nodes = nodesInEdges graph incidents


cfcGraph :: Graph -> Graph-> [Edge] -> Graph
cfcGraph graph graph' es | es == [] = graph'
                         | otherwise =  cfcGraph graph graph'' (tail es)
                         where graph'' = addEntry graph' ((getHeadNode graph (head es)), (head es))




------------------------------------------------ Kruskal Algorithm ------------------------------------------------

--the idea is to create a graph with the same node as the old one but
--which is a minimum spanning tree
kruskal :: Graph -> Graph
kruskal [] = error "The graph has no nodes"
kruskal graph = kruskal_help graph []


--if the number of nodes is 1+number of edges amd we have alle the original edges,
--it means we have our definitive newGraph, so we return newG. Else we add a new graph to
--newG and call kruskal' again
kruskal_help :: Graph -> Graph -> Graph
kruskal_help oldG newG  | nodeCount newG == 1 + (edgeCount newG)  &&  nodeCount oldG == nodeCount newG = newG
                        | otherwise = kruskal_help oldG newG'
                                  where newG' = addEdge oldG newG


--given a list of sorted edges and a graph, this method creates a newG' with the first edge of the list
--if this newG' do not contain cyles, and the edge is not already an element of newG,
--then this is the right edge to add and the graph' is returned
--else recursively we try to add the smallest edge that creates no cycles.
addFirstNonCycling :: [Edge] -> Graph -> Graph -> Graph
addFirstNonCycling edges oldG newG = if (head edges) `elem` allEdgesList newG || containsCycles testGraph then addFirstNonCycling (tail edges) oldG newG else newG'
                                     where newG' = addEntry newG (findEntry (head edges) oldG)
                                           testGraph = getCfc newG' (getIncidentNodes newG' (head edges))




--check if the graph contains cycles
containsCycles :: Graph -> Bool
containsCycles g = edgeCount g >= nodeCount g


--method that adds an entry to a graph and return the new graph
addEntry :: Graph -> (Node, Edge)-> Graph
addEntry graph entry = if notContainsNode graph (fst entry) || graph == [] then graph ++ [(fst entry, [snd entry])]
                       else addEntry' graph entry

addEntry' :: Graph -> (Node,Edge)-> Graph
addEntry' graph entry | graph == [] = []
                      | fst (head graph) == (fst entry) = newEntry:addEntry' (tail graph) entry
                      | otherwise = (head graph):addEntry' (tail graph) entry
                      where newEntry = (fst entry, (snd (head graph)) ++ [snd entry])



--add a selected edge to a graph from another graph. The edge must have the smallest
--weight and must not create a cycle with the other edges
addEdge :: Graph -> Graph -> Graph
addEdge oldG newG = addFirstNonCycling edges oldG newG
                    where edges = sortEdges(allEdgesList oldG)


--verify if the graph conatins the specific node
notContainsNode :: Graph -> Node -> Bool
notContainsNode graph node | ( filter ( \(findNode, _) -> findNode == node) (graph)) == [] =  True
                           | otherwise = False





------------------------------------------------ Reverse-Delete Algorithm ------------------------------------------------


revSortEdges :: [Edge] -> [Edge]
revSortEdges es = reverse $ sortBy(comparing weight) es

reverse_delete :: Graph -> Graph
reverse_delete [] = []
reverse_delete graph = reverse_delete' graph graph revEdges
                        where revEdges = revSortEdges(allEdgesList graph)

reverse_delete' :: Graph -> Graph -> [Edge] -> Graph
reverse_delete' graph graph' edges  |edges == [] = graph'
                                    |isConnected graph graph'' (head edges) = reverse_delete' graph graph'' (tail edges)
                                    |otherwise = reverse_delete' graph  graph' (tail edges)
                                     where graph'' = removeEdge graph' (head edges)

--pre-condition: the graph is connected. if an edge is not part of allListEdges and
--from the node of the edge doesn not start any edge, then the graph is not connected anymore
isConnected :: Graph -> Graph -> Edge -> Bool
isConnected graph graph' edge = if nodeCount(getCfc graph' nodes) == nodeCount graph  then True else False
                                where nodes = [getHeadNode graph edge]

removeEdge :: Graph -> Edge -> Graph
removeEdge [] edge = []
removeEdge graph edge = [(fst(head graph),delete edge (snd(head graph)))] ++ removeEdge (tail graph) edge





