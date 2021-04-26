module Error where

import           GHC.Exception


data DatabaseError
    -- When trying to submit a Registration
    -- multiple times.
    = RegistrationAlreadyExists
    -- When the Email of an Account is invalid.
    | InvalidEmail
    -- When the Degree of an Account is invalid.
    | InvalidDegree


instance Show DatabaseError where
    show RegistrationAlreadyExists = "Du kan ikke melde deg på flere ganger."
    show InvalidEmail = "Mailen du skrev inn er ikke gyldig. Vennligst skriv inn en studentmail."
    show InvalidDegree = "Sneaky."


instance Exception DatabaseError
