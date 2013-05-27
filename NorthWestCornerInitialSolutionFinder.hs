module NorthWestCornerInitialSolutionFinder 
(
	getNorthWestCornerInitialSolution, 
	module MyArrayUtils
) 
where

import MyArrayUtils
import Data.Array
import MyTrace

data IterationArguments x = IterationArguments (NormalArray x) (NormalArray x) Int Int
type IterationResult x = [((Int, Int), x)]
type Element t = (Num t, Ord t, Show t)

performIteration :: (Element t) => IterationArguments t -> IterationResult t
performIteration (IterationArguments supply demand supplyIndex demandIndex) =
	if 
		(supplyIndex < count supply) && (demandIndex < count demand) 
	then
		if
			currentDemand < currentSupply
		then 
			((supplyIndex, demandIndex), currentDemand) 
			: 
			performIteration 
				(IterationArguments 
					(supply // [(supplyIndex, currentSupply - currentDemand)]) 
					(demand // [(demandIndex, 0)]) 
					supplyIndex 
					(demandIndex + 1)
				)
		else
			((supplyIndex, demandIndex), currentSupply) 
			: 
			performIteration
				(IterationArguments 
					(supply // [(supplyIndex, 0)]) 
					(demand // [(demandIndex, currentDemand - currentSupply)]) 
					(supplyIndex + 1) 
					demandIndex
				)
	else
		[]
	where
		--note that although currentSupply and currentDemand are declared in the where block,
		-- in function they can be only referred to inside the if block which checks the indices
		currentSupply = extractArrayElement supply supplyIndex
		currentDemand = extractArrayElement demand demandIndex
	
	
iterationResultToArrayMatrix :: (Element t) => IterationResult t -> Int -> Int -> Normal2DArray t
iterationResultToArrayMatrix iterationResult supplyHeight demandWidth = 
	array 
		((0, 0), (supplyHeight - 1, demandWidth - 1))
		[ ((y, x), 0) | y <- [0 .. supplyHeight - 1], x <- [0 .. demandWidth - 1] ]
	//
	iterationResult
					
--эта функция находит начальное решение к транспортной задаче методом северо-западного угла 
getNorthWestCornerInitialSolution :: (Element t) => NormalArray t -> NormalArray t -> Normal2DArray t
getNorthWestCornerInitialSolution supply demand = 
	iterationResultToArrayMatrix 
		resultAsList
		(count supply) 
		(count demand)
	where
		resultAsList = performIteration (IterationArguments supply demand 0 0)
