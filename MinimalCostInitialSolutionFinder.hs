module MinimalCostInitialSolutionFinder
(
	getMinimalCostInitialSolution
)
where

getMinimalCostInitialSolution :: 
	(Element t) => TransportationTask t -> Normal2DArray t
getMinimalCostInitialSolution (TransportationTask supply demand costs) = 
