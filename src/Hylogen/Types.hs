{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Hylogen.Types where

import Data.Monoid
import Data.VectorSpace

class HyloPrim v where
  type HyloConstructor v :: *
  vec :: HyloConstructor v -> v
  vu :: String -> v
  vuop :: String -> v -> v
  vuoppre :: String -> v -> v
  vbop :: String -> v -> v -> v
  vboppre :: String -> v -> v -> v
  fromFloat :: Float -> v


instance {-# INCOHERENT #-} (Num v, HyloPrim v) => Num v where
  (+) = vbop "+"
  (*) = vbop "*"
  negate = vuoppre "-"
  abs = vuop "abs"
  signum = vuop "sign"
  fromInteger =  fromFloat . fromInteger

instance {-# INCOHERENT #-} (HyloPrim v) => Fractional v where
  (/) = vbop "/"
  recip = vbop "/" 1
  fromRational = fromFloat . fromRational


instance HyloPrim v => Floating v where
  pi = vu "pi"
  exp = vuop "exp"
  log = vuop "log"
  sqrt = vuop "sqrt"
  (**) = vboppre "pow"
  sin = vuop "sin"
  cos = vuop "cos"
  tan = vuop "tan"
  asin = vuop "asin"
  acos = vuop "acos"
  atan = vuop "atan"
  sinh x = (exp x - exp (negate x))/2
  cosh x = (exp x + exp (negate x))/2
  tanh x = sinh x / cosh x
  asinh x = log $ x + sqrt(x**2 + 1)
  acosh x = log $ x + sqrt(x**2 - 1)
  atanh x = 0.5 * log ((1 + x)/(1 - x))

instance HyloPrim v => AdditiveGroup v where
  zeroV = 0
  (^+^) = (+)
  negateV = negate
  (^-^) = (-)

-- -- Why doesn't this work?
-- instance (HyloPrim v, s ~ Scalar v) => VectorSpace v where
--   type Scalar (v) = Vec1
--   a *^ b = fromVec1 a * b -- TODO: optimize!

class Show a => HasX a
class HasX a => HasY a
class HasY a => HasZ a
class HasZ a => HasW a



data Vec1 where
  Vec1 :: Float -> Vec1
  V1u :: String -> Vec1
  V1uop :: String -> Vec1 -> Vec1
  V1uoppre :: String -> Vec1 -> Vec1
  V1bop :: String -> Vec1 -> Vec1 -> Vec1
  V1boppre :: String -> Vec1 -> Vec1 -> Vec1
  X :: (HasX a) => a -> Vec1
  Y :: (HasY a) => a -> Vec1
  Z :: (HasZ a) => a -> Vec1
  W :: (HasW a) => a -> Vec1


instance Show Vec1 where
  show expr = case expr of
    Vec1 x -> show x
    V1u x -> x
    V1uop u x -> u <> "(" <> show x <> ")"
    V1uoppre u x -> "(" <> u <> show x <> ")"
    V1bop b x y -> "(" <> show x <> " " <> b <> " " <> show y <> ")"
    V1boppre b x y -> b <> "(" <> show x <> ", " <> show y <> ")"
    X x ->  show x <> ".x"
    Y x ->  show x <> ".y"
    Z x ->  show x <> ".z"
    W x ->  show x <> ".w"


instance HyloPrim Vec1 where
  type HyloConstructor Vec1 = Float
  vec = Vec1
  vu = V1u
  vuop = V1uop
  vuoppre = V1uoppre
  vbop = V1bop
  vboppre = V1boppre
  fromFloat = Vec1

instance VectorSpace Vec1 where
  type Scalar Vec1 = Vec1
  a *^ b = a * b

-- | Vec2:

data Vec2 where
  Vec2 :: (Vec1, Vec1) -> Vec2
  V2u :: String -> Vec2
  V2uop :: String -> Vec2 -> Vec2
  V2uoppre :: String -> Vec2 -> Vec2
  V2bop :: String -> Vec2 -> Vec2 -> Vec2
  V2boppre :: String -> Vec2 -> Vec2 -> Vec2

instance HyloPrim Vec2 where
  type HyloConstructor Vec2 = (Vec1, Vec1)
  vec = Vec2
  vu = V2u
  vuop = V2uop
  vuoppre = V2uoppre
  vbop = V2bop
  vboppre = V2boppre
  fromFloat x = vec (vec x, vec x)


instance Show Vec2 where
  show expr = case expr of
    Vec2 (x, y) -> "vec2(" <> show x <> ", " <> show y <> ")"
    V2u x -> x
    V2uop u x -> u <> "(" <> show x <> ")"
    V2uoppre u x -> "(" <> u <> show x <> ")"
    V2bop b x y -> "(" <> show x <> " " <> b <> " " <> show y <> ")"
    V2boppre b x y -> b <> "(" <> show x <> ", " <> show y <> ")"

instance VectorSpace Vec2 where
  type Scalar Vec2 = Vec1
  a *^ b = vec (a, a) * b


instance HasX Vec2
instance HasY Vec2


-- | Vec3:

data Vec3 where
  Vec3 :: (Vec1, Vec1, Vec1) -> Vec3
  V3u :: String -> Vec3
  V3uop :: String -> Vec3 -> Vec3
  V3uoppre :: String -> Vec3 -> Vec3
  V3bop :: String -> Vec3 -> Vec3 -> Vec3
  V3boppre :: String -> Vec3 -> Vec3 -> Vec3

instance HyloPrim Vec3 where
  type HyloConstructor Vec3 = (Vec1, Vec1, Vec1)
  vec = Vec3
  vu = V3u
  vuop = V3uop
  vuoppre = V3uoppre
  vbop = V3bop
  vboppre = V3boppre
  fromFloat x = vec (vec x, vec x, vec x)

instance Show Vec3 where
  show expr = case expr of
    Vec3 (x, y, z) -> "vec3(" <> show x <> ", " <> show y <> ", " <> show z <> ")"
    V3u x -> x
    V3uop u x -> u <> "(" <> show x <> ")"
    V3uoppre u x -> "(" <> u <> show x <> ")"
    V3bop b x y -> "(" <> show x <> " " <> b <> " " <> show y <> ")"
    V3boppre b x y -> b <> "(" <> show x <> ", " <> show y <> ")"

instance VectorSpace Vec3 where
  type Scalar Vec3 = Vec1
  a *^ b = vec (a, a, a) * b

instance HasX Vec3
instance HasY Vec3
instance HasZ Vec3




-- | Vec4:

data Vec4 where
  Vec4 :: (Vec1, Vec1, Vec1, Vec1) -> Vec4
  V4u :: String -> Vec4
  V4uop :: String -> Vec4 -> Vec4
  V4uoppre :: String -> Vec4 -> Vec4
  V4bop :: String -> Vec4 -> Vec4 -> Vec4
  V4boppre :: String -> Vec4 -> Vec4 -> Vec4

instance HyloPrim Vec4 where
  type HyloConstructor Vec4 = (Vec1, Vec1, Vec1, Vec1)
  vec = Vec4
  vu = V4u
  vuop = V4uop
  vuoppre = V4uoppre
  vbop = V4bop
  vboppre = V4boppre
  fromFloat x = vec (vec x,vec x,vec x,vec x)

instance Show Vec4 where
  show expr = case expr of
    Vec4 (x, y, z, w) -> "vec4(" <> show x <> ", " <> show y <> ", " <> show z <> ", " <> show w <> ")"
    V4u x -> x
    V4uop u x -> u <> "(" <> show x <> ")"
    V4uoppre u x -> "(" <> u <> show x <> ")"
    V4bop b x y -> "(" <> show x <> " " <> b <> " " <> show y <> ")"
    V4boppre b x y -> b <> "(" <> show x <> ", " <> show y <> ")"


instance VectorSpace Vec4 where
  type Scalar Vec4 = Vec1
  a *^ b = vec (a, a, a, a) * b

instance HasX Vec4
instance HasY Vec4
instance HasZ Vec4
instance HasW Vec4
