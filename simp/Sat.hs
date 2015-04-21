{-# LANGUAGE TupleSections #-}

module Sat where

import Control.Monad
import Data.Graph
import Data.List
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
type Sat = [Clause]
type Clause = [Lit]
type Lit = Int
type Var = Int

nClauses :: Sat -> Int
nClauses = length

nVars :: Sat -> Int
nVars = foldl max 0 . map abs . concat

-- Takes into account gaps int the variable numbering.
nVarsReal :: Sat -> Int
nVarsReal = Set.size . Set.fromList . map abs . concat

isHorn :: Clause -> Bool
isHorn c = count isPos c <= 1

isAntiHorn :: Clause -> Bool
isAntiHorn c = count isNeg c <= 1

isHornOrAntiHorn :: Clause -> Bool
isHornOrAntiHorn c = isHorn c || isAntiHorn c

isAtLeastNeg :: Clause -> Bool
isAtLeastNeg c = count isNeg c >= 1

is1Sat :: Clause -> Bool
is1Sat [_] = True
is1Sat _ = False

is2Sat :: Clause -> Bool
is2Sat [_, _] = True
is2Sat _ = False

isComplementary :: Clause -> Bool
isComplementary (x:xs) = -x `elem` xs || isComplementary xs
isComplementary []     = False

isUnit :: Clause -> Bool
isUnit [_] = True
isUnit _   = False

isPos :: Lit -> Bool
isPos 0 = undefined
isPos x = x > 0

isNeg :: Lit -> Bool
isNeg 0 = undefined
isNeg x = x < 0

varsWithSameSign :: Sat -> [Int]
varsWithSameSign s =
    [l | l <- Set.toList lits, (-l) `Set.notMember` lits]
    where
        lits = Set.fromList $ concat s
        
mapLit :: (Int -> Int) -> Lit -> Lit
mapLit f a
    | a < 0 = - f (-a)
    | a > 0 = f a
    | otherwise = error "Unknown 0 literal."
       
simplify :: Sat -> Sat
simplify s =
    Set.toList $ Set.fromList $ map nub $ filter (not . isComplementary) $ map (map (mapLit litMap)) s
    where
        litMap x = Map.findWithDefault x x coalesced
        coalesced = Map.fromList $ stronglyConnComp graph >>= coalesce
        coalesce (CyclicSCC (v : vs)) = map (, v) vs
        coalesce _ = []
        graph = [(key, key, map snd i) |  i <- groupImplies $ mapMaybe implies s, let key = fst $ head i]
        implies [a, b]
            | a < 0 && b > 0 = return (-a, b) -- a=>b
            | a > 0 && b < 0 = return (-b, a) -- b=>a
            | otherwise      = mzero
        implies _ = mzero
        groupImplies = groupBy (testing fst) . sortBy (comparing fst)

binaryPropagate :: Sat -> Sat
binaryPropagate s =
    uncurry binaryPropagate' $ partition is2Sat s
        
binaryPropagate' :: [Clause] -> Sat -> Sat
binaryPropagate' binaryClauses nonBinaryClauses =
    map propagate binaryClauses ++ (if null newBinaryClauses then newBinaryClauses ++ newNonBinaryClauses else binaryPropagate' newBinaryClauses newNonBinaryClauses)
    where
        (newBinaryClauses, newNonBinaryClauses) = partition is2Sat $ map propagate nonBinaryClauses
        propagate xs = propagate' xs []
        propagate' [] _ = []
        propagate' [x] _ = [x]
        propagate' (x : xs) seen = if propagateFilter x (xs ++ seen) then x : propagate' xs (x : seen) else propagate' xs seen
        propagateFilter x xs = not $ fromMaybe [] (Map.lookup (-x) binaryMap) `intersects` xs
        intersects [] _ = False
        intersects _ [] = False
        intersects a b = any (`elem` b) a
        binaryMap = foldr (uncurry $ Map.insertWith (++)) Map.empty $ concat [[(a, [b]), (b, [a])]| [a, b] <- binaryClauses]
        
bcp :: Sat -> Sat
bcp = map Set.toList . bcp' . map Set.fromList

bcp' :: [Set Lit] -> [Set Lit]
bcp' s =
    if null unitClauses then s else unitClauses ++ bcp' (mapMaybe unitPropagate clauses)
    where
        (unitClauses, clauses) = partition ((== 1) . Set.size) s
        units = Set.unions unitClauses
        negUnits = Set.map negate units
        unitPropagate c
            | isComplementary $ Set.toList c = Nothing
            | not $ Set.null $ units `Set.intersection` c = Nothing
            | otherwise                                   = Just $ c `Set.difference` negUnits

pureBcp :: Sat -> Sat
pureBcp = map Set.toList . pureBcp' . map Set.fromList
            
pureBcp' :: [Set Lit] -> [Set Lit]
pureBcp' s =
    if null unitClauses && null pures then s else unitClauses ++ map Set.singleton pures ++ pureBcp' (mapMaybe unitPropagate clauses)
    where
        (unitClauses, clauses) = partition ((== 1) . Set.size) s
        lits = Set.unions s
        pures = [l | l <- Set.toList lits, (-l) `Set.notMember` lits]
        units = Set.union (Set.unions unitClauses) $ Set.fromList pures
        negUnits = Set.map negate units
        unitPropagate c
            | isComplementary $ Set.toList c = Nothing
            | not $ Set.null $ units `Set.intersection` c = Nothing
            | otherwise                                   = Just $ c `Set.difference` negUnits

minipure :: Sat -> Maybe [Int]
minipure = minipure' . map Set.fromList

minipure' :: [Set Lit] -> Maybe [Int]
minipure' s =
    if unsat then Nothing else recur
    where
        s' = pureBcp' s
        unitClauses = Set.unions $ filter ((== 1) . Set.size) s'
        unsat = any ((`Set.member` unitClauses) . negate) $ Set.toList unitClauses
        decided = Set.map abs unitClauses
        decision = find (`Set.notMember` decided) $ map abs $ Set.toList $ Set.unions s'
        recur =
            case decision of
                 Just decision' -> case minipure' (Set.singleton decision' : s') of
                        Just solution -> return $ decision' : solution
                        Nothing       -> minipure' (Set.singleton (negate decision') : s')
                 Nothing -> return []
            
parseDimacs :: String -> Sat
parseDimacs = map (map read . init) . dropWhile ((== "p") . head) . dropWhile ((== "c") . head) . filter (not . null) . map words . lines

printDimacs :: Sat -> String
printDimacs s =
    "p cnf " ++ show (nVars s) ++ " " ++ show (nClauses s) ++ "\n" ++ concatMap printClause s
    where
        printClause c = intercalate " " (map show c) ++ " 0\n"
        
count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

comparing :: Ord b => (a -> b) -> a -> a -> Ordering
comparing f a b = f a `compare` f b

testing :: Eq b => (a -> b) -> a -> a -> Bool
testing f a b = f a == f b