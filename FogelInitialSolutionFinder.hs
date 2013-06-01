module FogelInitialSolutionFinder 
(
	getFogelInitialSolution
)
where

import Data.Array
import Data.List
import MyArrayUtils
import MyTrace
import TransportationTaskCommon

go :: (Element t) => TransportationTask t -> Normal2DArrayGenerationList t
go task = 
	commonMinimalSolutionIteration task find
	where
		find :: (Element t) => (TransportationTask t) -> (Normal2DArrayIndexedElement' t)
		find (TransportationTask supply demand costs) = normal2DArrayIndexedElement ((0, 0), 0)
			where
				costsAsList :: (Element t) => [((Int, Int), t)]
				costsAsList = [ cell | cell <- assocs costs, cellAlive cell]
				cellAlive ((y, x), value) = 
					((supply ! y) > 0)
					&&
					((demand ! x) > 0)
				costsString :: (Element t) => Int -> [((Int, Int), t)]
				costsString theY = [((y, x), value) | ((y, x), value) <- costsAsList, y == theY]
				costsColumn :: (Element t) => Int -> [((Int, Int), t)]
				costsColumn theX = [((y, x), value) | ((y, x), value) <- costsAsList, x == theX]
				compareCells ((_, _), valueA) ((_, _), valueB) = 
					compare valueA valueB
				fine :: (Element t) => [((Int, Int), t)] -> t
				fine list = 
					if 
						((length list) == 0)
						||
						((length list) == 1)
					then
						0
					else
						abs (snd (head sortedList) - snd (sortedList !! 1))
					where
						sortedList = sortBy compareCells list
				stringFine y = fine costsString
				columnFine x = fine costsColumn
				height = count (NormalArray' supply)
				width = count (NormalArray' demand)
				sortFine (_, fineA) (_, fineB) = 
					compare fineA fineB
				stringFines = 
					sortBy
					sortFine
					[(y, stringFine y) | y <- [0 .. height - 1], (supply ! y) > 0]

getFogelInitialSolution :: Element t => TransportationTask t -> Normal2DArray t
getFogelInitialSolution (TransportationTask supply demand costs) =
		generate2DArray
			[]
			(count (NormalArray' supply))
			(count (NormalArray' demand))
			0
	
