{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Gather where

import Control.Applicative
import Control.Monad(join)

-- | Fold over the outcomes of a type that has an 'Alternative'.
--
-- @Gather@ embodies two steps.
--
--  * Getting data using 'many' and '<|>' from 'Alternative'
--  * Postprocessing the data in some way.
--
-- For example, @Gather (Either String) Parser@ is a type that helps you parse a sequence of
-- mixed production, similar to @many (p1 <|> p2 <|> p3)@ but then it also lets you specify
-- what to do with the aggregate result @p1@ and the aggregate result of @p2@ and so on.
--
-- Example:
--
-- > data Vehicle = Vehicle { wheels :: [Wheel], seats :: (Seat, [Seat]) }
-- >
-- > -- | Parse vehicle parts in any order
-- > parseVehicle = join $ runGather (
-- >   Vehicle <$> zeroOrMore parseWheel
-- >           <*> oneOrMore (fail "A vehicle requires at least one seat.") parseSeat
-- > )
data Gather g f a =
   forall m. (Monoid m) =>
   Gather
   { items :: f m
   , postProcess :: m -> g a
   }

-- | Simple type for parsing monads that also take care of error handling or other
-- 'postProcess' concerns.
type Gather' f = Gather f f

instance (Functor g, Functor f) => Functor (Gather g f) where
  fmap f (Gather items p) = Gather items (fmap (fmap f) p)

instance (Applicative g, Alternative f) => Applicative (Gather g f) where
  pure x = Gather (empty :: f ()) (pure (pure x))
  Gather ia pa <*> Gather ib pb = Gather ((l <$> ia) <|> (r <$> ib)) (\(ma, mb) -> pa ma <*> pb mb)
   where l x = (x, mempty)
         r x = (mempty, x)

runGather :: (Alternative f) => Gather g f a -> f (g a)
runGather (Gather i p) = let x = mconcat <$> many i in fmap p x

-- | @'join' . 'runGather'@
runGather' :: (Alternative f, Monad f) => Gather' f a -> f a
runGather' = join . runGather

gather :: Monoid m => (m -> g a) -> f m -> Gather g f a
gather p i = Gather i p

-- TODO: Use DList in these functions

zeroOrMore :: (Functor f, Applicative g)
  => f a
  -> Gather g f [a]
zeroOrMore item = Gather (fmap (:[]) item) $ pure

zeroOrMore_ :: (Functor f, Applicative g)
  => f a
  -> Gather g f ()
zeroOrMore_ item = Gather (fmap mempty item) $ pure

zeroOrOne :: (Functor f, Applicative g)
  => g (Maybe a) -- ^ on many, typically a 'fail', 'Left' or similar
  -> f a
  -> Gather g f (Maybe a)
zeroOrOne onMany item = Gather (fmap (:[]) item) $
                       \l -> case l of
                               [] -> pure Nothing
                               [a] -> pure (Just a)
                               _ -> onMany

oneOrMore :: (Functor f, Applicative g)
  => g (a, [a]) -- ^ on zero, typically a 'fail', 'Left' or similar
  -> f a
  -> Gather g f (a, [a])
oneOrMore onErr item = Gather (fmap (:[]) item) $
                       \l -> case l of
                               [] -> onErr
                               (a: as) -> pure (a, as)

-- | Naive implementation that does not backtrack after the item has been parsed
-- once. This may change in the future.
exactlyOne :: (Functor f, Applicative g)
  => g a -- ^ on zero, typically a 'fail', 'Left' or similar
  -> g a -- ^ on many, typically a 'fail', 'Left' or similar
  -> f a
  -> Gather g f a
exactlyOne onNil onMany item = Gather (fmap (:[]) item) $
                       \l -> case l of
                               [] -> onNil
                               [a] -> pure a
                               _ -> onMany

