module TransportationTaskCommon
(
	Element,
	TransportationTask (TransportationTask),
)
where

import MyArrayUtils

type Element t = (Num t, Ord t, Show t)
data TransportationTask t = TransportationTask (NormalArray t) (NormalArray t) (Normal2DArray t)
type InitialSolutionFinder t = TransportationTask t -> Normal2DArray t
