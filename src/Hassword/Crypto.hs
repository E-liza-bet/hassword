{-# LANGUAGE RankNTypes #-}
module Hassword.Crypto (hash
                       ,hmac
                       ,ascii2b
                       ,b2asciiWith
                       ,alphabet
                       )
where
import Data.Bits (xor)
import Data.Word (Word64)
import Data.Char
import Hassword.Config

type Key = [Word64]
type Message = [Word64]
type HashFunc =  forall a . (Integral a) => ( [Word64] -> [a])

padding :: Integral a => Int -> a -> [a] -> [a]
padding len padval orig = orig <> replicate (len-length orig)  padval

alphabet = ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9'] :: String

--hold : The base of (type b)'s value = base
baseConvert :: (Integral a,Integral b) => Int -> a -> [b]
baseConvert base x = let f 0 = []
                         f x = let x' = x `div` fromIntegral base
                                   y  = x `mod` fromIntegral base
                           in fromIntegral y : f x'
                     in f x
 
ascii2b :: Integral a => String -> [a]
ascii2b = map (fromIntegral . ord)

b2asciiWith :: Integral a => String -> [a] -> String
b2asciiWith alphabet = map ((alphabet !!) . (`mod` length alphabet) . fromIntegral)

b2ascii :: Integral a => [a] -> String
b2ascii = b2asciiWith alphabet

hash :: Integral a => [Word64] -> [a]
hash bs = let addition = fromIntegral _HashAddition
              multiplient = fromIntegral _HashMultiplient
              f :: [Word64] -> Word64
              f [] = 1
              f (x:xs) = (x * multiplient + addition) * f xs
          in padding 8 0 $ baseConvert 256 $ f bs

-- hmac hash(key,message) to 8 int64
hmac :: HashFunc -> Key -> Message -> Int -> [Word64]
hmac h k m rounds = let iteration m = h ((k' `exor` opad) <> h ((k' `exor` ipad) <> m))
                          where blocksize = 8
                                k' = if length k > blocksize then h k else k
                                opad = repeat 0x5c
                                ipad = repeat 0x36
                                x `exor` y = zipWith xor x y
                   in foldr ($)  m $ replicate rounds iteration
