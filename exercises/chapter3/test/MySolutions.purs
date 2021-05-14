module Test.MySolutions where

import Prelude

import Data.AddressBook (AddressBook, Entry)
import Data.List (filter, head, null, nubBy)
import Data.Maybe (Maybe)
-- import Test.NoPeeking.Solutions (removeDuplicates)

-- Note to reader: Add your solutions to this file
compareNames :: String -> String -> Entry -> Boolean
compareNames firstName lastName entry = entry.firstName == firstName && entry.lastName == lastName

compareEntries :: Entry -> Entry -> Boolean
compareEntries book1 book2 = compareNames book1.firstName book1.lastName book2

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street book = head $ filter filterStreet book
  where
  filterStreet :: Entry -> Boolean
  filterStreet entry = entry.address.street == street

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName book = not null $ filter (compareNames firstName lastName) book

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates book = nubBy compareEntries book
