module Graphics.ForeignUtil (
  withNullPtr,
  nullable,
  nullCk,
  peekDouble,
)
where

import Data.Coerce
import Foreign
import Foreign.C.Types

withNullPtr :: (Ptr () -> IO a) -> IO a
withNullPtr f = f nullPtr

nullable :: (Coercible (Ptr a) b) => Maybe b -> b
nullable = coerce $ \case
  Just ptr -> ptr
  Nothing -> nullPtr

nullCk :: (Coercible (Ptr a) b) => b -> Maybe b
nullCk = coerce $ \ptr ->
  if ptr == nullPtr then Nothing else Just ptr

peekDouble :: Ptr CDouble -> IO Double
peekDouble = coerce <$> peek
