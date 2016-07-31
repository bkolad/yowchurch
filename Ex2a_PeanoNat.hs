module Ex2a_PeanoNat where

-- Peano numbers represent the natural numbers, by taking them
-- as either 0 or n+1. Doing anything with them is usually very
-- inefficient.
data PNat = Zero | Succ PNat

p0, p1, p2, p3, p4 :: PNat

p0 = Zero
p1 = Succ p0
p2 = Succ p1
p3 = Succ p2
p4 = Succ p3


-- Ex 2a.1: Implement Peano addition
infixl 6 .+
(.+) :: PNat -> PNat -> PNat
(.+) Zero pn2 = pn2
(.+) (Succ x) pn2 = x .+ (Succ pn2)

--(a + 1) * b = b + ab

-- Ex 2a.2: Implement Peano multiplication
infixl 7 .*
(.*) :: PNat -> PNat -> PNat
(.*) Zero pn2 = Zero
(.*) (Succ pn1) pn2 = pn1 .* pn2 .+ pn2


--a ^ (b+1)= a * a^b


-- Ex 2a.3: Implement Peano exponential
infixr 8 .^
(.^) :: PNat -> PNat -> PNat
(.^) pn1 Zero = Succ Zero
(.^) pn1 (Succ x) = pn1 .* pn1 .^x
-- Ex 2a.4: Convert a Peano number to an integer
unpeano :: PNat -> Int
unpeano Zero = 0
unpeano (Succ x) = 1 + (unpeano x)

-- Ex 2a.5: Convert a non-negative integer to a Peano number
peano :: Int -> PNat
peano 0 = Zero
peano n = Succ $ peano (n-1)



-- Instance boilerplate
instance Show PNat where
  show = ("peano " ++) . show . unpeano

instance Eq PNat where
  a == b = unpeano a == unpeano b
