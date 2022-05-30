module Mocks.Predicates where

import App.Error
  ( AppError (..),
  )

is404Error,
  isParsingError,
  isEntityNotFoundError,
  isAlreadyExistsError,
  isUnathorizedError,
  isWrongPasswordError,
  isRequestHeadersError,
  isAmbiguousPatternsError,
  isAccessViolationError,
  isAdminAccessViolationError,
  isCategoryCycleError ::
    AppError -> Bool
is404Error PageNotFoundError = True
is404Error _ = False
isParsingError ParsingError {} = True
isParsingError _ = False
isEntityNotFoundError EntityNotFound {} = True
isEntityNotFoundError _ = False
isAlreadyExistsError AlreadyExists {} = True
isAlreadyExistsError _ = False
isUnathorizedError Unathorized {} = True
isUnathorizedError _ = False
isWrongPasswordError WrongPassword {} = True
isWrongPasswordError _ = False
isRequestHeadersError RequestHeadersError {} = True
isRequestHeadersError _ = False
isAmbiguousPatternsError RouterAmbiguousPatterns {} = True
isAmbiguousPatternsError _ = False
isAccessViolationError AccessViolation {} = True
isAccessViolationError _ = False
isAdminAccessViolationError AdminAccessViolation {} = True
isAdminAccessViolationError _ = False
isCategoryCycleError CategoryCycle {} = True
isCategoryCycleError _ = False
