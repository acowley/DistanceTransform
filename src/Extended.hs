{-# LANGUAGE ScopedTypeVariables #-}
module Extended where
import Data.Word (Word8)
import Foreign.Ptr (castPtr)
import Foreign.Storable

data Extended a = NegInf | Finite !a | PosInf deriving (Eq,Ord,Show)

instance forall a. Storable a => Storable (Extended a) where
  sizeOf _ = sizeOf (undefined::a) + 1
  alignment _ = alignment (undefined::a)
  peek ptr = do tag <- peek (castPtr ptr) :: IO Word8
                case tag of
                  0 -> return NegInf
                  1 -> Finite `fmap` peekByteOff (castPtr ptr) 1
                  2 -> return PosInf
  poke ptr NegInf = poke (castPtr ptr) (0::Word8)
  poke ptr (Finite x) = poke (castPtr ptr) (1::Word8) >>
                        pokeByteOff (castPtr ptr) 1 x
  poke ptr PosInf = poke (castPtr ptr) (2::Word8)

unextend :: a -> Extended a -> a
unextend _ (Finite x) = x
unextend x _ = x