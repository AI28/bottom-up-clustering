module Pure(single_linkage,
            complete_linkage,
            average_linkage,
            solve)
where


import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.List
import qualified Data.Set as Set

type Point = [Float]

data Cluster = Singleton Int Point 
             | C Int (Cluster, Cluster)
             | Void
             deriving (Eq)

instance Show Cluster where
	show x = showWithDepth x 0
	       where
		showWithDepth::Cluster->Int->String
		showWithDepth Void _ = ""
		showWithDepth (Singleton number point) depth = (replicate depth '\t') ++ "Singleton " ++ show number ++ " " ++ show point ++ "\n"
		showWithDepth (C number (c1, c2)) depth = (replicate depth '\t') ++ "Cluster " ++ show number ++ "\n" ++ (showWithDepth c1 (depth+1)) ++ (showWithDepth c2 (depth+1)) 

data Similarity = Similarity{
                  firstCluster::Int,
                  secondCluster::Int,
                  metric::Float
                  }
                  deriving (Show)


instance Eq Similarity where
   (==) (Similarity c1 c2 sim1) (Similarity c3 c4 sim2) = sim1 == sim2 && ((c1 == c3 && c1 == c4) || (c1 == c4) && (c2 == c3))

instance Ord Similarity where
   compare sim1@(Similarity c1 c2 metric1) sim2@(Similarity c3 c4 metric2)
       | sim1 == sim2 = EQ
       | metric1 > metric2 = LT
       | otherwise = GT


cluster_number::Cluster->Int
cluster_number (Singleton number _) = number
cluster_number (C number _) = number 

init_clusters::[Point]->[Cluster]
init_clusters entries = init $ foldr (\x y -> (Singleton ((+1) . cluster_number . head $ y) x) : y) [(Singleton (0) [])] $ reverse $ entries

to_list::Cluster->[Point]
to_list (Singleton _ point) = [point]
to_list (C _ (c1,c2)) = (to_list c1) ++ (to_list c2)

helper_euclidean_distance::Point->Point->Float
helper_euclidean_distance [] [] = 0.0
helper_euclidean_distance (x:xs) (y:ys) = ((+) . (^2) $ (x-y)) $ (euclidean_distance xs ys)

euclidean_distance::Point->Point->Float
euclidean_distance p1 p2 = sqrt . helper_euclidean_distance p1 $ p2

partial_constructor::Cluster->Cluster->Float->Similarity
partial_constructor c1 c2= Similarity (cluster_number c1) (cluster_number c2)

get_distances::(Point->Point->Float)->Cluster->Cluster->[Float]
get_distances metric c1 c2 = pure metric <*> to_list c1 <*> to_list c2

similarity::([Float]->Float)->Cluster->Cluster->Similarity
similarity min_or_max c1 c2 = partial_constructor c1 c2 $ min_or_max $ get_distances euclidean_distance c1 c2 

complete_linkage::Cluster->Cluster->Similarity
complete_linkage = similarity maximum

single_linkage::Cluster->Cluster->Similarity
single_linkage = similarity minimum

average_linkage::Cluster->Cluster->Similarity
average_linkage c1 c2 = partial_constructor c1 c2 $ foldr (\x y -> y + (x / no_of_points)) 0.0 distance_list
                      where
                       c1_points = to_list c1
                       c2_points = to_list c2
                       no_of_points = fromIntegral . length $ c1_points ++ c2_points
                       distance_list = pure euclidean_distance <*> c1_points <*> c2_points


arg_max::(Cluster->Cluster->Similarity)->[Cluster]->Similarity
arg_max f clusters = maximum $ Set.filter (\(Similarity c1 c2 _) -> c1 /= c2) $ Set.fromList $ pure f <*> clusters <*> clusters

reduce_clusters::(Cluster->Cluster->Similarity)->[Cluster]->[Cluster]
reduce_clusters _ [] = []
reduce_clusters sim clusters = (filter (\x -> x /= first_cluster && x /= second_cluster) clusters) ++ [C (last_cluster + 1) (first_cluster, second_cluster)]
                               where
                                argmax = arg_max sim clusters
                                first_cluster = fromJust . find ( \x -> cluster_number x == (firstCluster argmax)) $ clusters
                                second_cluster = fromJust . find ( \x -> cluster_number x == (secondCluster argmax)) $ clusters
                                last_cluster = maximum [ cluster_number x | x <- clusters]

solve::(Cluster->Cluster->Similarity)->[Point]->Cluster
solve _ [] = Void
solve sim points = head . head . dropWhile (\x -> length x /= 1 ) $ iterate (reduce_clusters sim) $ init_clusters points
