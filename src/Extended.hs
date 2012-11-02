{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, OverlappingInstances #-}
module Extended (Extended(..), unextend) where
import Data.Bits
import Data.Word (Word8)
import Foreign.Ptr (castPtr)
import Foreign.Storable

data Extended a = NegInf | Finite !a | PosInf deriving (Eq,Ord,Show)

instance Num a => Num (Extended a) where
  Finite x + Finite y = Finite $ x + y
  NegInf + PosInf = Finite 0
  PosInf + NegInf = Finite 0
  NegInf + _ = NegInf
  PosInf + _ = PosInf
  _ + NegInf = NegInf
  _ + PosInf = PosInf

  Finite x * Finite y = Finite $ x * y
  NegInf * NegInf = PosInf
  NegInf * _ = NegInf
  PosInf * NegInf = NegInf
  PosInf * _ = PosInf
  _ * NegInf = NegInf
  _ * PosInf = PosInf

  Finite x - Finite y = Finite $ x - y
  NegInf - NegInf = Finite 0
  PosInf - PosInf = Finite 0
  PosInf - _ = PosInf
  NegInf - _ = NegInf
  _ - PosInf = NegInf
  _ - NegInf = PosInf
  
  negate (Finite x) = Finite $ negate x
  negate PosInf = NegInf
  negate NegInf = PosInf

  abs (Finite x) = Finite (abs x)
  abs _ = PosInf

  signum (Finite x) = Finite (signum x)
  signum PosInf = Finite 1
  signum NegInf = Finite (-1)

  fromInteger = Finite . fromInteger

highIntBit :: Int
highIntBit = bitSize (undefined::Int) - 1

-- Performance hack for integers that only supports one infinity.
instance Storable (Extended Int) where
  sizeOf _ = sizeOf (undefined::Int)
  alignment _ = alignment (undefined::Int)
  peek ptr = do x <- peek (castPtr ptr) :: IO Int
                if testBit x highIntBit
                   then return PosInf
                   else return $ Finite x
  poke ptr (Finite x) = poke (castPtr ptr) x
  poke ptr _ = poke (castPtr ptr) (bit highIntBit :: Int)
                

instance forall a. Storable a => Storable (Extended a) where
  sizeOf _ = sizeOf (undefined::a) + 1
  alignment _ = alignment (undefined::a)
  peek ptr = do tag <- peek (castPtr ptr) :: IO Word8
                case tag of
                  0 -> return NegInf
                  1 -> Finite `fmap` peekByteOff (castPtr ptr) 1
                  2 -> return PosInf
                  _ -> error "Impossible finiteness tag"
  poke ptr NegInf = poke (castPtr ptr) (0::Word8)
  poke ptr (Finite x) = poke (castPtr ptr) (1::Word8) >>
                        pokeByteOff (castPtr ptr) 1 x
  poke ptr PosInf = poke (castPtr ptr) (2::Word8)

unextend :: a -> Extended a -> a
unextend _ (Finite x) = x
unextend x _ = x