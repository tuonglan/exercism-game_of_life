module GameOfLife (tick) where

tick :: [[Int]] -> [[Int]]
tick [] = []
tick matrix = nextRow height matrix
    where   width  = length matrix
            height = length $ head matrix
            emptyRow = take width $ repeat 0

            nextRow row matrix
                | row == 1      = case matrix of
                    r1:[]               -> nextValue emptyRow r1 emptyRow : []
                | row == 2      = case matrix of
                    r1:r2:[]            -> nextValue emptyRow r1 r2 : nextValue r1 r2 emptyRow : []
                    r1:r2:r3:[]         -> nextValue r1 r2 r3 : nextValue r2 r3 emptyRow : []
                | row == height = case matrix of
                    r1:r2:rtail         -> nextValue emptyRow r1 r2 : nextRow (row-1) matrix
                | otherwise     = case matrix of
                    r1:r2:r3:rtail      -> nextValue r1 r2 r3 : nextRow (row-1) (r2:r3:rtail)

            nextValue r1 r2 r3 = nextValue' width r1 r2 r3
            nextValue' col r1 r2 r3
                | col == 1      = case (r1, r2, r3) of
                    ([a], [b], [c])                                     -> calValue (0, a, 0) (0, b, 0) (0, c, 0) : []
                | col == 2      = case (r1, r2, r3) of
                    (a1:a2:[], b1:b2:[], c1:c2:[])                      -> calValue (0, a1, a2) (0, b1, b2) (0, c1, c2) : calValue (a1, a2, 0) (b1, b2, 0) (c1, c2, 0) : []
                    (a1:a2:a3:[], b1:b2:b3:[], c1:c2:c3:[])            -> calValue (a1, a2, a3) (b1, b2, b3) (c1, c2, c3) : calValue (a2, a3, 0) (b2, b3, 0) (c2, c3, 0) : []
                | col == width  = case (r1, r2, r3) of
                    (a1:a2:atail, b1:b2:btail, c1:c2:ctail)             -> calValue (0, a1, a2) (0, b1, b2) (0, c1, c2) : nextValue' (col-1) r1 r2 r3
                | otherwise     = case (r1, r2, r3) of
                    (a1:a2:a3:atail, b1:b2:b3:btail, c1:c2:c3:ctail)    -> calValue (a1, a2, a3) (b1, b2, b3) (c1, c2, c3) : nextValue' (col-1) (tail r1) (tail r2) (tail r3)

            calValue (a1, a2, a3) (b1, b2, b3) (c1, c2, c3) = 
                let sum = a1 + a2 + a3 + b1 + b3 + c1 + c2 + c3
                in case (sum, b2) of
                    (2, 1)  -> 1
                    (3, _)  -> 1
                    _       -> 0
