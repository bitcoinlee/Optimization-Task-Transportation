module NorthWestCornerInitialSolutionFinder 
(
	getNorthWestCornerInitialSolution, 
	module MyArrayUtils
) 
where

import MyArrayUtils
import Data.Array
import MyTrace
import TransportationTaskCommon

data IterationArguments x = IterationArguments (NormalArray x) (NormalArray x) Int Int
type IterationResult x = Normal2DArrayGenerationList x

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

--эта функция находит начальное решение к транспортной задаче методом северо-западного угла 
getNorthWestCornerInitialSolution :: (Element t) => NormalArray t -> NormalArray t -> Normal2DArray t
getNorthWestCornerInitialSolution supply demand = 
	generate2DArray 
		resultAsList
		(count supply) 
		(count demand)
		0
	where
		resultAsList = performIteration (IterationArguments supply demand 0 0)
