module Error where

import           GHC.Exception


data DatabaseError
    = -- When trying to submit a Registration
      -- to the same Bedpres multiple times.
      RegistrationAlreadyExists
    | -- When trying to submit a Registration
      -- for a Bedpres that does not exist.
      BedpresDoesNotExist
    | -- When the Email of a Student is invalid.
      InvalidEmail
    | -- When the Degree of a Student is invalid.
      InvalidDegree


instance Show DatabaseError where
    show RegistrationAlreadyExists = "Du kan ikke melde deg på samme bedriftspresentasjon flere ganger."
    show BedpresDoesNotExist = "Bedriftspresentasjonen du prøver å melde deg på finnes ikke."
    show InvalidEmail = "Mailen du skrev inn er ikke gyldig. Vennligst skriv inn en studentmail."
    show InvalidDegree = "Sneaky."


instance Exception DatabaseError
