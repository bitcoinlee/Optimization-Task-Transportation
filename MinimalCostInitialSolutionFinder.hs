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
		=> NormalArray t -> NormalArray t -> Normal2DArray t -> Normal2DArrayIndexedElement' t
goFindMinimum supply demand costs =
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
		NormalArray t -> NormalArray t -> Normal2DArray t -> Normal2DArrayGenerationList t
goIterate supply demand costs =
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
				goIterate
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
	where
		desiredCell = goFindMinimum supply demand costs
		(Normal2DArrayIndexedElement' desiredIndex desiredValue) = desiredCell
		supplyIndex = fst desiredIndex
		demandIndex = snd desiredIndex
		desiredSupply = supply ! supplyIndex
		desiredDemand = demand ! demandIndex
		supplyReleased = desiredSupply < desiredDemand
		amountToRelease = if supplyReleased then desiredSupply else desiredDemand

getMinimalCostInitialSolution :: 
	(Element t) => TransportationTask t -> Normal2DArray t
getMinimalCostInitialSolution (TransportationTask supply demand costs) = 
	generate2DArray 
		(
			trace 
				(show resultAsArrayGenerationList)
				resultAsArrayGenerationList
		)
		(count supply) 
		(count demand) 
		0
	where
		resultAsArrayGenerationList = 
			goIterate supply demand costs
