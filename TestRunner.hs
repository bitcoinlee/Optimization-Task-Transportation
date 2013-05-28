module TestRunner where

import Debug.Trace
import MyArrayUtils
import TransportationTaskCommon
import NorthWestCornerInitialSolutionFinder
import MinimalCostInitialSolutionFinder
import EvaluateSolution
import TestSet

toNorthWestCornerInitialSolutionFinderArguments :: 
	(Element t) => TransportationTask t -> (NormalArray t, NormalArray t)
toNorthWestCornerInitialSolutionFinderArguments (TransportationTask supply demand costs) =
	(supply, demand)
	
defaultElementWidth = 3

testInitialSolutionFinder taskNumber finder = 
	putStr
		(
			normal2DArrayToString solution defaultElementWidth
			++
			"Total cost: " ++ show (evaluateSolution costs solution)
			++
			"\n"
		)
	where
		task = taskSet !! taskNumber 
		(TransportationTask supply demand costs) = task
		solution = finder task

