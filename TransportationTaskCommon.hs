module TransportationTaskCommon
(
	Element,
	TransportationTask (TransportationTask),
	commonMinimalSolutionIteration
)
where

import Data.Array
import MyArrayUtils
import MyTrace

type Element t = (Num t, Ord t, Show t)
data TransportationTask t = TransportationTask (NormalArray t) (NormalArray t) (Normal2DArray t)
type InitialSolutionFinder t = TransportationTask t -> Normal2DArray t

commonMinimalSolutionIteration :: 
	(Element t) => 
		(TransportationTask t) 
		-> 
		(TransportationTask t -> Normal2DArrayIndexedElement' t)
		-> 
		Normal2DArrayGenerationList t
commonMinimalSolutionIteration task@(TransportationTask supply demand costs) findMin =
	trace
		("<goIterate " ++ show supply ++ " " ++ show demand ++ ">")
		(
			if 
				desiredCell == NoNormal2DArrayIndexedElement
			then
				[] -- the cycle is over
			else
				((supplyIndex, demandIndex), amountToRelease)
				:
				commonMinimalSolutionIteration
					(TransportationTask
						(
							supply
							// 
							[(supplyIndex, if supplyReleased then 0 else desiredSupply - desiredDemand)]
						)
						(
							demand
							// 
							[(demandIndex, if supplyReleased then desiredDemand - desiredSupply else 0)]
						)
					costs 
					)
					findMin
		)
	where
		desiredCell = findMin task
		(Normal2DArrayIndexedElement' desiredIndex desiredValue) = desiredCell
		supplyIndex = fst desiredIndex
		demandIndex = snd desiredIndex
		desiredSupply = supply ! supplyIndex
		desiredDemand = demand ! demandIndex
		supplyReleased = desiredSupply < desiredDemand
		amountToRelease = if supplyReleased then desiredSupply else desiredDemand
		traceEnabled = False
		trace message x = 
			if
				traceEnabled
			then
				MyTrace.trace message x
			else
				x

