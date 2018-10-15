import qualified Data.Graph as G
import qualified Data.Set as S

keepEdge :: G.Edge -> S.Set G.Vertex -> S.Set G.Vertex -> Bool
keepEdge thisEdge aSet bSet = (S.member (fst thisEdge) aSet) && (S.member (snd thisEdge) bSet)

connectingEdges :: G.Graph -> S.Set G.Vertex -> S.Set G.Vertex -> [G.Edge]
connectingEdges graph aSet bSet = filter (\x -> keepEdge x aSet bSet) (G.edges graph)

cheegerAB :: G.Graph -> S.Set G.Vertex -> S.Set G.Vertex -> Rational
cheegerAB graph aSet bSet = (fromIntegral $ length (connectingEdges graph aSet bSet))/(fromIntegral $ min (S.size aSet) (S.size bSet))