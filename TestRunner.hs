module TestRunner where

import Debug.Trace
import NorthWestCornerInitialSolutionFinder
import TestSet
import MyArrayUtils
import TransportationTaskCommon

toNorthWestCornerInitialSolutionFinderArguments :: 
	(Element t) => TransportationTask t -> (NormalArray t, NormalArray t)
toNorthWestCornerInitialSolutionFinderArguments (TransportationTask supply demand costs) =
	(supply, demand)
	

testNorthWestCornerInitialSolutionFinder taskNumer = 
	putStr
		(
			normal2DArrayToString
			(
				getNorthWestCornerInitialSolution (fst arguments) (snd arguments)
			)
		)
	where
		task = taskSet !! taskNumer
		arguments = toNorthWestCornerInitialSolutionFinderArguments task
