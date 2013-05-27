module TestRunner where

import Debug.Trace
import MyArrayUtils
import TransportationTaskCommon
import NorthWestCornerInitialSolutionFinder
import MinimalCostInitialSolutionFinder
import TestSet

toNorthWestCornerInitialSolutionFinderArguments :: 
	(Element t) => TransportationTask t -> (NormalArray t, NormalArray t)
toNorthWestCornerInitialSolutionFinderArguments (TransportationTask supply demand costs) =
	(supply, demand)
	

testNorthWestCornerInitialSolutionFinder taskNumber = 
	putStr
		(
			normal2DArrayToString
			(
				getNorthWestCornerInitialSolution (fst arguments) (snd arguments)
			)
		)
	where
		task = taskSet !! taskNumber
		arguments = toNorthWestCornerInitialSolutionFinderArguments task

testMinimalCostInitialSolutionFinder taskNumber = 
	putStr
		(
			normal2DArrayToString
			(
				getMinimalCostInitialSolution (taskSet !! taskNumber)
			)
		)
