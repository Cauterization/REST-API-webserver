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

import Database.Config ( Config(..) )
import Database.Delete ( Deletable(..), deleteEntity )
import Database.Get
    ( getEntitiesWith,
      Gettable(..),
      EntityFilterParam(..),
      defaultFilters,
      getEntitiesGeneric,
      getEntityFilters,
      getEntityGeneric,
      EntityFilter(..) )
import Database.HasDatabase ( HasDatabase(..) )
import Database.Internal
    ( QConstraints, IsDatabase(..), DBError(..), getSingle, qmarks )
import Database.Post
    ( postEntity, postEntityWith, qmarkfields, Postable(..) )
import Database.Put
    ( putEntity, Puttable(..), putEntityWith, toCoalesce )
