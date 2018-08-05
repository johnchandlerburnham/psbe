module Data.AddressBook where

import Prelude
import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubBy)
import Data.Maybe

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ": " <>
                  showAddress entry.address

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <>
                   addr.city <> ", " <>
                   addr.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry entry book = Cons entry book

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName book = head $ filter filterEntry book
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName &&
                        entry.lastName  == lastName

findEntry' :: String -> String -> AddressBook -> Maybe Entry
findEntry' firstName lastName = filter filterEntry >>> head
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName &&
                        entry.lastName  == lastName

printEntry :: String -> String -> AddressBook -> Maybe String
printEntry firstName lastName book =
  map showEntry (findEntry firstName lastName book)

-- Exercises:
-- 1: head :: AddressBook -> Maybe Entry
--    filter :: (Entry -> Boolean) -> AddressBook -> AddressBook

-- 2

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street book = head $ filter filterEntry book
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.address.street == street

-- 3
hasEntry :: String -> String -> AddressBook -> Boolean
hasEntry firstName lastName book = null $ filter filterEntry book
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName &&
                        entry.lastName  == lastName

-- 4
removeDuplicates :: AddressBook -> AddressBook
removeDuplicates book = nubBy sameNames book
  where
    sameNames entry1 entry2 = entry1.firstName == entry2.firstName &&
                              entry2.lastName  == entry2.lastName
