{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Database
  ( IsDatabase (..),
    HasDatabase (..),
    Config (..),
    Postable (..),
    postEntityWith,
    postEntity,
    qmarkfields,
    qmarks,
    Gettable (..),
    EntityFilter (..),
    EntityFilterParam (..),
    getEntityFilters,
    defaultFilters,
    getEntityGeneric,
    getEntitiesGeneric,
    getEntitiesWith,
    getSingle,
    Puttable (..),
    putEntity,
    putEntityWith,
    toCoalesce,
    Deletable (..),
    deleteEntity,
    QConstraints,
    DBError (..),
  )
where

import Database.Config
import Database.Delete
import Database.Get
import Database.HasDatabase
import Database.Internal
import Database.Post
import Database.Put
