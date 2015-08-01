-- Filename: crud.hs
-- Created by: Daniel Winograd-Cort
-- Created on: 11/21/2012
-- Last Modified by: Daniel Winograd-Cort
-- Last Modified on: 12/10/2013

-- -- DESCRIPTION --
-- This code was inspired by a blog post by Heinrich Apfelmus on 
-- bidirectional data flow in GUIs:
-- http://apfelmus.nfshost.com/blog/2012/03/29-frp-three-principles-bidirectional-gui.html
-- 
-- Here we use UISF to create a similar example using arrowized FRP.


{-# LANGUAGE Arrows, RecursiveDo #-}
module FRP.UISF.Examples.Crud where
import FRP.UISF

import Data.List (isInfixOf)
import Data.Char (toLower)


-- First we create types for the database and the entries for it

type Database a = [a]
data NameEntry = NameEntry {firstName :: String, lastName :: String}

instance Show NameEntry where
    show (NameEntry f l) = l ++ ", " ++ f

instance Eq NameEntry where
    (NameEntry f1 l1) == (NameEntry f2 l2) = f1 == f2 && l1 == l2

deleteFromDB :: (a -> Bool) -> Int -> Database a -> Database a
deleteFromDB _ _ [] = []
deleteFromDB f i (x:xs) = case (f x, i == 0) of
    (True, True)    -> xs
    (True, False)   -> x:deleteFromDB f (i-1) xs
    (False, _)      -> x:deleteFromDB f i xs

updateDB :: (a -> Bool) -> Int -> a -> Database a -> Database a
updateDB _ _ _ [] = []
updateDB f i a (x:xs) = case (f x, i == 0) of
    (True, True)    -> a:xs
    (True, False)   -> x:updateDB f (i-1) a xs
    (False, _)      -> x:updateDB f i a xs


-- defaultnames is a default database for our example
defaultnames :: Database NameEntry
defaultnames = [
    NameEntry "Paul" "Hudak",
    NameEntry "Dan" "Winograd-Cort",
    NameEntry "Donya" "Quick"]


-- | This function will run the crud GUI with the default names.
crud = runUI (defaultUIParams {uiSize=(450, 400), uiTitle="CRUD"}) (crudUISF defaultnames)
-- | main = crud
main = crud

-- | This is the main function that creates the crud GUI.  It takes an 
--   initial database of names as an argument.
--   See notes below on the use of banana brackets and nested do blocks.
crudUISF :: Database NameEntry -> UISF () ()
crudUISF initnamesDB = proc _ -> do
  rec
    fStr <- leftRight $ label "Filter text: " >>> textbox "" -< Nothing
    let fdb = filter (filterFun fStr) db
    (i, nameData) <- (| leftRight (do
        i <- listbox' -< (fdb, i')
        nameData <- (| topDown (do
            rec nameStr <- leftRight $ label "Name:    " >>> textbox "" -< nameStr'
                surnStr <- leftRight $ label "Surname: " >>> textbox "" -< surnStr'
                iUpdate <- unique -< i
                let nameStr' = fmap (const $ firstName (fdb `at` i)) iUpdate
                    surnStr' = fmap (const $ lastName  (fdb `at` i)) iUpdate
            returnA -< NameEntry nameStr surnStr) |)
        returnA -< (i, nameData)) |)
    buttons <- leftRight $ (edge <<< button "Create") &&& 
                           (edge <<< button "Update") &&& 
                           (edge <<< button "Delete") -< ()
    (db,i') <- delay (initnamesDB, -1) -< case buttons of
            (Just _, (_, _))             -> (db ++ [nameData], length fdb)
            (Nothing, (Just _, _))       -> (updateDB (filterFun fStr) i nameData db, i)
            (Nothing, (Nothing, Just _)) -> (deleteFromDB (filterFun fStr) i db,
                                             if i == length fdb - 1 then i - 1 else i)
            _ -> (db, i)
  returnA -< ()
  where
    filterFun str name = and (map (\s -> isInfixOf s (map toLower $ show name)) (words (map toLower str)))
    lst `at` index = if index >= length lst || index < 0 then NameEntry "" "" else lst!!index


-- If we don't care about formatting, this code simplifies a huge amount to:
-- crudUISF initnamesDB = proc _ -> do
--   rec
--     fStr <- leftRight $ label "Filter text: " >>> textbox "" -< Nothing
--     let fdb = filter (filterFun fStr) db
--     i <- listbox -< (fdb, i')
--     nameStr <- leftRight $ label "Name:    " >>> textbox "" -< nameStr'
--     surnStr <- leftRight $ label "Surname: " >>> textbox "" -< surnStr'
--     iUpdate <- unique -< i
--     let nameStr' = fmap (const $ firstName (fdb `at` i)) iUpdate
--         surnStr' = fmap (const $ lastName  (fdb `at` i)) iUpdate
--         nameData = NameEntry nameStr surnStr
--     buttons <- leftRight $ (edge <<< button "Create") &&& 
--                            (edge <<< button "Delete") -< ()
--     (db,i') <- delay (initnamesDB, -1) <- case buttons of
--            (Just _, (_, _))             -> (db ++ [nameData], length fdb)
--            (Nothing, (Just _, _))       -> (updateDB (filterFun fStr) i nameData db, i)
--            (Nothing, (Nothing, Just _)) -> (deleteFromDB (filterFun fStr) i db,
--                                             if i == length fdb - 1 then length fdb - 2 else i)
--             _ -> (db, i)
--   returnA -< ()
--   where
--     ...
-- 
-- Clearly, this is easier to read and clearer as to what is going on. 
-- However, to keep the style entirely arrow-based, we are forced to inject 
-- arrow transformers (here leftRight and topDown) to modify chunks of the 
-- code.  The banana brackets (| |) allow us to refrain from retyping the 
-- "proc do" syntax, but in order to give other parts of the program access 
-- to the variables created in the banana bracketed chunks, we require 
-- extra (seemingly excessive) returnA commands at the end of each.


