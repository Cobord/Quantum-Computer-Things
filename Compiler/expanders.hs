import qualified Data.Graph as G
import qualified Data.Set as S

keepEdge :: G.Edge -> S.Set G.Vertex -> S.Set G.Vertex -> Bool
keepEdge thisEdge aSet bSet = (S.member (fst thisEdge) aSet) && (S.member (snd thisEdge) bSet)

connectingEdges :: G.Graph -> S.Set G.Vertex -> S.Set G.Vertex -> [G.Edge]
connectingEdges graph aSet bSet = filter (\x -> keepEdge x aSet bSet) (G.edges graph)

-- count number of edges connecting aSet and bSet and divide by the smaller's size
cheegerAB :: G.Graph -> S.Set G.Vertex -> S.Set G.Vertex -> Rational
cheegerAB graph aSet bSet = (fromIntegral $ length (connectingEdges graph aSet bSet))/(fromIntegral $ min (S.size aSet) (S.size bSet))

-- B is the complement of A in actual computation of Cheeger constant
cheegerA :: G.Graph -> S.Set G.Vertex -> Rational
cheegerA graph aSet = cheegerAB graph aSet (S.difference allVertices aSet) where allVertices=foldr S.insert S.empty (G.vertices graph)

--Example
--my_graph=buildG (1,4) [(1,2),(2,1),(2,3),(3,2),(3,1),(1,3),(4,2),(2,4)]
--cheegerA my_graph (fromList [1,2,3])