module NorthWestCornerInitialSolutionFinder (getNorthWestCornerInitialSolution, module MyArrayUtils) where

import MyArrayUtils
import Data.Array

type IterationResult x = [((Int, Int), x)]
data IterationArguments x = IterationArguments (NormalArray x) (NormalArray x) Int Int (IterationResult x) 

iteratE :: (Num elementType) => IterationArguments elementType -> IterationResult elementType
iteratE (IterationArguments supply demand supplyIndex demandIndex existingResult) =
	[]

iterationResultToArrayMatrix :: (Num x) => IterationResult x -> Int -> Int -> Normal2DArray x
iterationResultToArrayMatrix iterationResult supplyHeight demandWidth = 
	array ((0, 0), (supplyHeight, demandWidth)) iterationResult

--эта функция находит начальное решение к транспортной задаче методом северо-западного угла 
getNorthWestCornerInitialSolution :: (Num x) => NormalArray x -> NormalArray x -> Normal2DArray x
getNorthWestCornerInitialSolution supply demand = 
	array ((0, 0), (0, 0)) []