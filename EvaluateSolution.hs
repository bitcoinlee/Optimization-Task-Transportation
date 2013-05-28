module EvaluateSolution where

import Data.Array
import MyArrayUtils
import TransportationTaskCommon

evaluateSolution :: (Element t) => Normal2DArray t -> Normal2DArray t -> t
evaluateSolution costs solution =
	totalCost
	where
		transportationCost i cost = cost * (solution ! i)
		makeResult result = (Normal2DArrayIndexedElement' (0, 0) result)
		desire
			(Normal2DArrayIndexedElement' i cost)
			(Normal2DArrayIndexedElement' _ result)
			= makeResult (result + transportationCost i cost)
		desire
			(Normal2DArrayIndexedElement' i cost)
			(NoNormal2DArrayIndexedElement)
			= makeResult (transportationCost i cost)
		(Normal2DArrayIndexedElement' _ totalCost) = normal2DArrayExtractSpecificElement costs desire
		
			

	
