module Test.MySolutions where

import Prelude
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Control.Apply (lift2)
import Data.Maybe (Maybe(..))
import Data.AddressBook.Validation(matches, Errors, validatePhoneNumbers, validateAddress, nonEmpty )
import Data.AddressBook(Address, PhoneNumber, address)
import Data.Validation.Semigroup (V)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Foldable, class Traversable, foldMap, foldl, foldr, sequence, traverse)
-- Note to reader: Add your solutions to this file

addMaybe = lift2 (+)

subMaybe = lift2 (-)

mulMaybe = lift2 (*)

divMaybe = lift2 (/)

addApply = lift2 (+)

subApply = lift2 (-)

mulApply = lift2 (*)

divApply = lift2 (/)

combineMaybe :: forall a f. Applicative f => Maybe ( f a ) -> f ( Maybe a )
combineMaybe (Just x) = Just <$> x
combineMaybe _        = pure Nothing

stateRegex :: Regex
stateRegex = unsafeRegex "^[a-zA-Z]{2}$" noFlags

nonEmptyRegex :: Regex
nonEmptyRegex = unsafeRegex ".[^\\s\\t]" noFlags

validateAddressImproved :: Address -> V Errors Address
validateAddressImproved a =
    address <$> matches "Street" nonEmptyRegex a.street
            <*> matches "City" nonEmptyRegex a.city
            <*> matches "State" stateRegex a.state

data Tree a = Leaf | Branch (Tree a) a (Tree a)

derive instance eqTree :: Eq a => Eq ( Tree a )
derive instance genericTree :: Generic (Tree a) _
instance showShape :: Show a => Show (Tree a) where
  show a = genericShow a

derive instance functorTree :: Functor Tree

instance foldableTree :: Foldable Tree where
  foldl _ acc Leaf = acc
  foldl f acc (Branch t1 v t2) = foldl f (f (foldl f acc t1) v) t2
  foldr _ acc Leaf = acc
  foldr f acc (Branch t1 v t2) = foldr f (f v (foldr f acc t2)) t1
  foldMap _ Leaf = mempty
  foldMap f (Branch t1 v t2) = foldMap f t1 <> f v <> foldMap f t2

instance traversableTree :: Traversable Tree where
  traverse _ Leaf = pure Leaf
  traverse f (Branch t1 v t2) = ado
    mt1 <- traverse f t1
    mv <- f v
    mt2 <- traverse f t2
    in Branch mt1 mv mt2
  -- Equivalent
  --traverse f (Branch t1 v t2) = Branch <$> traverse f t1 <*> f v <*> traverse f t2
  sequence Leaf = pure Leaf
  sequence (Branch t1 v t2) = ado
    mt1 <- sequence t1
    mv <- v
    mt2 <- sequence t2
    in Branch mt1 mv mt2

traversePreOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m ( Tree b)
traversePreOrder f Leaf = pure Leaf
-- traversePreOrder f ( Branch t1 v t2 ) = Branch <$> (traversePreOrder f t1) <*> f v <*> (traversePreOrder f t2)
traversePreOrder f (Branch t1 v t2) = ado
  root <- f v
  left <- traversePreOrder f t1
  right <- traversePreOrder f t2
  in Branch left root right

traversePostOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m ( Tree b)
traversePostOrder f Leaf = pure Leaf
-- traversePreOrder f ( Branch t1 v t2 ) = Branch <$> (traversePreOrder f t1) <*> f v <*> (traversePreOrder f t2)
traversePostOrder f (Branch t1 v t2) = ado
  left <- traversePostOrder f t1
  right <- traversePostOrder f t2
  root <- f v
  in Branch left root right

type PersonOptionalAddress
  = { firstName :: String
    , lastName :: String
    , homeAddress :: Maybe Address
    , phones :: Array PhoneNumber
    }

-- validatePerson :: Person -> V Errors Person
-- validatePerson p =
--   person <$> nonEmpty "First Name" p.firstName
--          <*> nonEmpty "Last Name" p.lastName
--          <*> validateAddress p.homeAddress
--          <*> validatePhoneNumbers "Phone Numbers" p.phones

personOptionalAddress :: String -> String -> Maybe Address -> Array PhoneNumber -> PersonOptionalAddress
personOptionalAddress firstName lastName homeAddress phones = { firstName, lastName, homeAddress, phones }

validatePersonOptionalAddress :: PersonOptionalAddress -> V Errors PersonOptionalAddress
validatePersonOptionalAddress p =
    personOptionalAddress <$> nonEmpty "First Name" p.firstName
                          <*> nonEmpty "Last Name" p.lastName
                          <*> traverse validateAddress p.homeAddress
                          <*> validatePhoneNumbers "Phone Numbers" p.phones

sequenceUsingTraverse :: forall t a m. Traversable t => Applicative m => t (m a) -> m (t a)
sequenceUsingTraverse t = traverse identity t

traverseUsingSequence :: forall t a b m. Traversable t => Applicative m => (a -> m b) -> t a -> m (t b)
traverseUsingSequence f t = sequence $ f <$> t
