module TestRunner where

import Debug.Trace
import NorthWestCornerInitialSolutionFinder
import TestSet
import MyArrayUtils

testNorthWestCornerInitialSolutionFinder testNumber = 
	putStr
		(
			normal2DArrayToString
			(
				getNorthWestCornerInitialSolution 
					(fst chosenTestSet)
					(snd chosenTestSet)
			)
		)
	where
		chosenTestSet = testSet !! testNumber
