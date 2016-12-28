{-# LANGUAGE FunctionalDependencies, FlexibleInstances, UndecidableInstances, ScopedTypeVariables, TypeFamilies #-}

module PGMUD.Types.GeneralClasses
    ( EnumIndexedValues (..)
    , HasEIV (..)
    ) where

class (Bounded e, Enum e) => EnumIndexedValues e where
    data EIVOuterWrapper e
    data EIVWrapper e
    wrapperConstructor :: [EIVWrapper e] -> EIVOuterWrapper e
    wrapperDestructor :: EIVOuterWrapper e -> [EIVWrapper e]
    innerConstructor :: Float -> EIVWrapper e
    innerDestructor :: EIVWrapper e -> Float
    spliceAt :: EIVWrapper e -> e -> EIVOuterWrapper e -> EIVOuterWrapper e
    spliceAt v e wrapped = let 
        l = wrapperDestructor wrapped
      in 
        wrapperConstructor ((take (fromEnum e) l) ++ [v] ++ (drop (fromEnum e+1) l))

instance EnumIndexedValues e => Monoid (EIVOuterWrapper e) where
    mempty = wrapperConstructor $ map (\_ -> innerConstructor 0) ([minBound..maxBound] :: [e])
    mappend l r = wrapperConstructor $ zipWith (\l' r' -> innerConstructor (innerDestructor l' + innerDestructor r')) (wrapperDestructor l) (wrapperDestructor r)

class EnumIndexedValues e => HasEIV a e where
    getEIV :: a -> EIVOuterWrapper e