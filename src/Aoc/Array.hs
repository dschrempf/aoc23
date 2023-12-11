{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Aoc.Array
-- Description :  Tools for arrays
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Dec 12 11:41:00 2022.
module Aoc.Array
  ( neighbors,
    neighborsNoDiagonal,
    neighbors3,
    neighborsNoDiagonal3,
    breakA,
    filterA,
    insertRows,
    insertCols,
  )
where

import Data.Massiv.Array
  ( Array,
    D,
    DL,
    Index (isSafeIndex),
    Ix1,
    Ix2 (..),
    Ix3,
    IxN (..),
    Manifest,
    Size (size),
    Source,
    Sz (Sz),
    Vector,
  )
import qualified Data.Massiv.Array as A
import Prelude hiding (break)

stencil :: Sz Ix2 -> Ix2 -> [Ix2]
stencil s p =
  [ p'
    | f <- [pred, id, succ],
      g <- [pred, id, succ],
      let p' = f i :. g j,
      isSafeIndex s p'
  ]
  where
    (i :. j) = p

-- | Get the 8 neighbors of a field in a two-dimensional grid.
neighbors :: Sz Ix2 -> Ix2 -> [Ix2]
neighbors s p =
  [ i' :. j'
    | (i' :. j') <- stencil s p,
      not (i' == i && j' == j)
  ]
  where
    (i :. j) = p

-- | Like 'neighbors' but only get the 4 direct neighbors, and not the 4
-- diagonal ones.
neighborsNoDiagonal :: Sz Ix2 -> Ix2 -> [Ix2]
neighborsNoDiagonal s p =
  [ i' :. j'
    | (i' :. j') <- stencil s p,
      not (i' == i && j' == j),
      i' == i || j' == j
  ]
  where
    (i :. j) = p

stencil3 :: Sz Ix3 -> Ix3 -> [Ix3]
stencil3 s p =
  [ p'
    | f <- [pred, id, succ],
      g <- [pred, id, succ],
      h <- [pred, id, succ],
      let p' = f i :> g j :. h k,
      isSafeIndex s p'
  ]
  where
    (i :> j :. k) = p

-- | Get the 26 neighbors of a field in a three-dimensional grid.
neighbors3 :: Sz Ix3 -> Ix3 -> [Ix3]
neighbors3 s p =
  [ i' :> j' :. k'
    | (i' :> j' :. k') <- stencil3 s p,
      not (i' == i && j' == j && k' == k)
  ]
  where
    (i :> j :. k) = p

-- | Like 'neighbors3' but only get the 6 direct neighbors, and not the 20
-- diagonal ones.
neighborsNoDiagonal3 :: Sz Ix3 -> Ix3 -> [Ix3]
neighborsNoDiagonal3 s p =
  [ f p di
    | di <- [1, -1],
      f <- fs,
      let p' = f p di,
      isSafeIndex s p'
  ]
  where
    fs =
      [ \(Ix3 x y z) dx -> Ix3 (x + dx) y z,
        \(Ix3 x y z) dy -> Ix3 x (y + dy) z,
        \(Ix3 x y z) dz -> Ix3 x y (z + dz)
      ]

-- | Like 'Data.List.break' but for arrays.
breakA :: (Manifest r e) => (e -> Bool) -> Vector r e -> (Vector r e, Vector r e)
breakA p xs = A.sliceAt i xs
  where
    i = maybe (size xs) Sz $ A.findIndex p xs

-- | Like 'Data.List.filter' but for arrays.
filterA :: (Index ix, Source r e) => (e -> Bool) -> Array r ix e -> [ix]
filterA f = A.ifoldlS accF []
  where
    accF ixs ix e
      | f e = ix : ixs
      | otherwise = ixs

insertRows ::
  (Source r e) =>
  Array r Ix2 e ->
  Ix1 ->
  Array r Ix2 e ->
  Array DL Ix2 e
insertRows rows at ar = A.concat' 2 [top, A.delay rows, bottom]
  where
    (top, bottom) = A.splitAt' 2 at ar

insertCols ::
  (Source r e) =>
  Array r Ix2 e ->
  Ix1 ->
  Array r Ix2 e ->
  Array DL Ix2 e
insertCols cols at ar = A.concat' 1 [left, A.delay cols, right]
  where
    (left, right) = A.splitAt' 1 at ar
