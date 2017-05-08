{-# LANGUAGE RecordWildCards #-}

module Example where

import Data.Maybe

import CM.Metamodel
import CM.Visualization
import CM.Validity
import CM.Helpers

wrapString :: String -> String
wrapString x = "\"" ++ x ++ "\""

shortenString :: String -> String
shortenString x = if length x < 35 then x else take 32 x ++ "..."

toInts :: [String] -> [Int]
toInts = map read

data Student = Student { stUsername :: String
                       , stFirstname :: String
                       , stLastname :: String
                       }
             deriving (Show, Read)

instance Identifiable Student where
  identifier = stUsername

instance CMElement Student where
  toMeta = toMetaEntity

instance Entity Student where
  entityAttributes Student {..} =
    map tupleToAttribute
      [ ("username", "String", wrapString stUsername)
      , ("firstname", "String", wrapString stFirstname)
      , ("lastname", "String", wrapString stLastname)
      , ("email", "String", wrapString $ stUsername ++ "@fit.cvut.cz")
      ]

data Course = Course { crsCode :: String
                     , crsCredits :: Int
                     , crsName :: String
                     , crsDesc :: String
                     }
           deriving (Show, Read)

instance Identifiable Course where
  identifier = crsCode

creditsNonNegative :: Course -> Validity
creditsNonNegative Course {..} =
  newConstraint (crsCredits >= 0) "Negative credits"

instance CMElement Course where
  toMeta = toMetaEntity
  simpleConstraints = [creditsNonNegative]

instance Entity Course where
  entityAttributes Course {..} =
    map tupleToAttribute
      [ ("code", "String", wrapString crsCode)
      , ("name", "String", wrapString crsName)
      , ("credits", "Int", show crsCredits)
      , ("descrition", "String", wrapString . shortenString $ crsDesc)
      ]

data Enrollment = Enrollment { enrlSince :: (Int, Int, Int)
                             , enrlUntil :: Maybe (Int, Int, Int)
                             }
               deriving (Show, Read)

instance Identifiable Enrollment

toDateStr :: (Int, Int, Int) -> String
toDateStr (d,m,y) = show d ++ "/" ++ show m ++ "/" ++ show y

sinceBeforeUntil :: Enrollment -> Validity
sinceBeforeUntil Enrollment {enrlUntil = Nothing, ..} = Valid
sinceBeforeUntil Enrollment {enrlSince = a, enrlUntil = Just b} =
  newConstraint (a < b) "Since is not before until"

instance CMElement Enrollment where
  toMeta = toMetaEntity
  simpleConstraints = [sinceBeforeUntil]

instance Entity Enrollment where
  entityAttributes Enrollment {..} =
    map tupleToAttribute
      [ ("since", "DateTime", toDateStr enrlSince)
      , ("until", "Maybe DateTime", maybe "N/A" toDateStr enrlUntil)
      ]

data IsEnrolled = IsEnrolled { ieWho :: Student
                             , ieWhat :: Course
                             , ieTruth :: Enrollment
                             }
                deriving (Show, Read)

instance Identifiable IsEnrolled

max2Enrollments :: (ConceptualModel m) => m -> IsEnrolled -> Validity
max2Enrollments model r =
  newConstraint (same <= 2) "Student can enroll same course only twice"
  where same = length . filter areSame $ enrolledRelationships
        areSame e = (sameCourse e) && (sameStudent e)
        sameCourse e = (getCourseId e) == (identifier . ieWhat $ r)
        sameStudent e = (getStudentId e) == (identifier . ieWho $ r)
        enrolledRelationships = filter isIsEnrolled $ cmodelElements model
        getCourseId MetaRelationship {..} = findParticipantId mrParticipations "Course"
        getCourseId _ = ""
        getStudentId MetaRelationship {..} = findParticipantId mrParticipations "Student"
        getStudentId _ = ""
        isIsEnrolled MetaRelationship {mrName = "IsEnrolled", ..} = True
        isIsEnrolled _ = False

instance CMElement IsEnrolled where
  toMeta = toMetaRelationship
  complexConstaints = [max2Enrollments]

instance Relationship IsEnrolled where
  relationshipName _ = "enrolled"
  relationshipParticipations IsEnrolled {..} =
    map tupleToParticipation
      [ ("who", "Student", identifier ieWho, Optional Unlimited)
      , ("what", "Course", identifier ieWhat, Optional Unlimited)
      , ("truthmaker", "Enrollment", identifier ieTruth, Mandatory Unique)
      ]

studM = Student { stUsername = "suchama4"
                , stFirstname = "Marek"
                , stLastname = "Suchánek"
                }

courseDIP = Course { crsCode = "MI-DIP"
                   , crsCredits = 23
                   , crsName = "Diploma Project"
                   , crsDesc = ""
                   }

courseRRI = Course { crsCode = "MI-RRI.0"
                   , crsCredits = -5
                   , crsName = "Risk Management in Informatics"
                   , crsDesc = "Information security is very often considered as one of main objectives to secure targets of information processing. "
                   }

enrlMxDIP = Enrollment { enrlSince = (20,2,2017)
                       , enrlUntil = Just (1,6,2016)
                       }

enrlMxRRI = Enrollment { enrlSince = (20,2,2017)
                       , enrlUntil = Nothing
                       }

ienrlMxDIP = IsEnrolled { ieWho = studM, ieWhat = courseDIP, ieTruth = enrlMxDIP }
ienrlMxRRI = IsEnrolled { ieWho = studM, ieWhat = courseRRI, ieTruth = enrlMxRRI }

data ExampleModel = ExampleModel { mStudents :: [Student]
                                 , mCourses :: [Course]
                                 , mEnrollments :: [Enrollment]
                                 , mIsEnrolled :: [IsEnrolled]
                                 }
                  deriving (Show, Read)

instance CMElement ExampleModel where
  toMeta = toMetaModel

instance ConceptualModel ExampleModel where
  cmodelElements m = (map (toMeta m) $ mStudents m)
                  ++ (map (toMeta m) $ mCourses m)
                  ++ (map (toMeta m) $ mEnrollments m)
                  ++ (map (toMeta m) $ mIsEnrolled m)

model = ExampleModel { mStudents = [studM]
                     , mCourses = [courseDIP, courseRRI]
                     , mEnrollments = [enrlMxDIP, enrlMxRRI]
                     , mIsEnrolled = [ienrlMxDIP, ienrlMxRRI]
                     }
