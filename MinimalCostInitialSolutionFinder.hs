module MinimalCostInitialSolutionFinder
(
	getMinimalCostInitialSolution
)
where

import Data.Array
import MyArrayUtils
import TransportationTaskCommon
import MyTrace

goFindMinimum ::
	(Element t) 
		=> (TransportationTask t) -> Normal2DArrayIndexedElement' t
goFindMinimum (TransportationTask supply demand costs) =
	normal2DArrayExtractSpecificElement	costs findDesired	
	where
		exhausted (Normal2DArrayIndexedElement' index _ ) =
			( (supply ! (fst index)) == 0 )
			||
			( (demand ! (snd index)) == 0 )
		exhausted NoNormal2DArrayIndexedElement = True
		exhaust cell@(Normal2DArrayIndexedElement' _ _) =
			if
				exhausted cell
			then
				NoNormal2DArrayIndexedElement
			else
				cell
		findDesired 
			a@(Normal2DArrayIndexedElement' _ aValue) 
			b@(Normal2DArrayIndexedElement' _ bValue)
			=
			if 
				exhausted a
			then
				exhaust b
			else
				if 
					exhausted b
				then
					exhaust a
				else
					if 
						aValue < bValue -- it's cheaper
					then
						a
					else
						b
		findDesired 
			NoNormal2DArrayIndexedElement 
			b@(Normal2DArrayIndexedElement' _ bValue)
			= exhaust b
		findDesired 
			a@(Normal2DArrayIndexedElement' _ bValue)
			NoNormal2DArrayIndexedElement 
			= exhaust a
		findDesired 
			NoNormal2DArrayIndexedElement 
			NoNormal2DArrayIndexedElement
			= NoNormal2DArrayIndexedElement

goIterate :: 
	(Element t) => 
		(TransportationTask t) -> Normal2DArrayGenerationList t
goIterate task =
	commonMinimalSolutionIteration task goFindMinimum

getMinimalCostInitialSolution :: (Element t) => TransportationTask t -> Normal2DArray t
getMinimalCostInitialSolution task@(TransportationTask supply demand _)  = 
	generate2DArray 
		(
			trace 
				(show resultAsArrayGenerationList)
				resultAsArrayGenerationList
		)
		(count (NormalArray' supply)) 
		(count (NormalArray' demand))
		0
	where
		resultAsArrayGenerationList = 
			goIterate task
		traceEnabled = False
		trace message x = 
			if 
				traceEnabled
			then
				MyTrace.trace message x
			else
				x
