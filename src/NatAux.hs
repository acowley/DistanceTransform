{-# LANGUAGE DataKinds, KindSignatures, ScopedTypeVariables #-}
module NatAux (natToInt) where
import Linear.Dim (Nat(..))
import Data.Proxy

class NatToInt (n :: Nat) where
  natToInt :: Proxy n -> Int

instance NatToInt Z where
  natToInt _ = 0

instance forall m. NatToInt m => NatToInt (S m) where
  natToInt _ = natToInt (Proxy :: Proxy m)
