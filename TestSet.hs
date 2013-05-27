module TestSet where

import Data.Array
import MyArrayUtils
import TransportationTaskCommon

taskSet = 
	[
		(TransportationTask 
			(toArray [15, 25, 10]) -- supply, предложение
			(toArray [5, 15, 15, 15]) -- demand, спрос
			( -- transportation costs
				to2DArray 
					[ -- цены перевозки: по вертикали предложение, по горизонтали спрос
						[10, 2, 20, 11], -- 15
						[12, 7, 9, 20],  -- 25
						[4, 14, 16, 18]  -- 10
					] -- 5, 15, 15, 15
			)
		)
		,
		(TransportationTask 
			(toArray [6, 7, 7]) 
			(toArray [5, 5, 10])
			(
				to2DArray 
					[
						[0, 2, 1],
						[2, 1, 5],
						[2, 4, 3]
					]
			)
		)
	]
