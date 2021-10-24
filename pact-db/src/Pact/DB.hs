{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.DB
  ( module Pact.DB,
    module Pact.DB.Persistent,
  )
where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Containers.ListUtils (nubOrd)
import Data.Functor ((<&>))
import qualified Data.Map as Map
import Data.Maybe
import Data.Password.Bcrypt
import Data.Password.Instances ()
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Persist ()
import Data.Validity.Text ()
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics (Generic)
import Pact.DB.Persistent
import Pact.Data
import Yesod

share
  [mkPersist sqlSettings, mkMigrate "serverMigration"]
  [persistLowerCase|

User
  uuid UserUUID
  name Username
  password (PasswordHash Bcrypt)

  pic ImageUUID Maybe
  aboutMe Textarea

  UniqueUser uuid
  UniqueUsername name

  deriving Show Eq Ord Generic

Coach -- A coach is an extended version of a user
  user UserUUID
  uuid CoachUUID
  expertise Textarea

  UniqueCoach uuid
  UniqueCoachUser user

  deriving Show Eq Ord Generic

Exercise
  uuid ExerciseUUID
  image ImageUUID
  video VideoUUID
  name Text
  difficulty Difficulty
  formTips FormTips
  notes Text

  UniqueExerciseUUID uuid
  UniqueExerciseImage image
  UniqueExerciseVideo video
  UniqueExerciseName name

  deriving Show Eq Ord Generic

Image
  uuid ImageUUID -- TODO: Replace this by a key based on the hash of the contents and type
  contents ByteString
  typ Text -- Content type

  UniqueImageUUID uuid

  deriving Show Eq Ord Generic

Video
  uuid VideoUUID -- TODO: Replace this by a key based on the hash of the contents and type
  contents ByteString
  typ Text -- Content type

  UniqueVideoUUID uuid

  deriving Show Eq Ord Generic

MuscleFilter
  exercise ExerciseUUID
  muscle Muscle

  UniqueMuscleFilter exercise muscle

  deriving Show Eq Ord Generic

ExerciseMaterial
  uuid ExerciseMaterialUUID
  name Text

  UniqueMaterialUUID uuid
  UniqueMaterialName name

  deriving Show Eq Ord Generic

MaterialFilter
  exercise ExerciseUUID
  material ExerciseMaterialUUID

  UniqueMaterialFilter exercise material

  deriving Show Eq Ord Generic

ExerciseAlternativeName
  uuid ExerciseUUID
  name Text

  UniqueExerciseAlternativeName uuid name

  deriving Show Eq Ord Generic

CustomerCoachRelation
  customer UserUUID
  coach CoachUUID
  response CoachProposalResponse Maybe

  UniqueRelation customer coach

  deriving Show Eq Ord Generic

FriendRelation
  proposer UserUUID
  receiver UserUUID
  response FriendRequestResponse Maybe

  UniqueFriend proposer receiver

  deriving Show Eq Ord Generic

UserWorkout
  user UserUUID
  type WorkoutType
  day Day
  amount WorkoutAmount

  deriving Show Eq Ord Generic

CoachWorkout
  uuid CoachWorkoutUUID
  coach CoachUUID
  type WorkoutType
  amount WorkoutAmount
  day Day
  timeOfDay TimeOfDay
  address Textarea
  notes Textarea

  UniqueCoachWorkout uuid

  deriving Show Eq Ord Generic

WorkoutJoin
  customer UserUUID
  workout CoachWorkoutUUID
  status JoinStatus

  UniqueJoin customer workout

  deriving Show Eq Ord Generic
|]

allDifficulties :: [Difficulty]
allDifficulties = [minBound .. maxBound]

allMuscles :: [Muscle]
allMuscles = [minBound .. maxBound]

workoutTypes :: [WorkoutType]
workoutTypes = [minBound .. maxBound]

instance Validity (Salt a) where
  validate = trivialValidation

instance Validity (PasswordHash a) where
  validate = trivialValidation

instance Validity Textarea where
  validate (Textarea t) = delve "Textarea" t

instance Validity User

instance Validity Exercise -- Any value with well-formed Texts and such, is valid

instance Validity Image

instance Validity Video

instance Validity MuscleFilter

instance Validity ExerciseMaterial where
  validate material@ExerciseMaterial {..} =
    mconcat
      [ genericValidate material, -- Actually only valid if present in the dB
        declare "Name is not empty" $ not $ T.null exerciseMaterialName
      ]

instance Validity MaterialFilter

instance Validity ExerciseAlternativeName where
  validate name@ExerciseAlternativeName {..} =
    mconcat
      [ genericValidate name,
        declare "Name is not empty" $ not $ T.null exerciseAlternativeNameName
      ]

newtype Material = Material {unMaterial :: Text}
  deriving (Show, Eq, Ord, Generic)

newtype AlternativeName = AlternativeName {unAlternative :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity AlternativeName where
  validate name =
    mconcat
      [ genericValidate name,
        declare "Name is not empty" . not . T.null $ unAlternative name
      ]

data CompleteExercise = CompleteExercise
  { exerciseCE :: Exercise,
    musclesCE :: [Muscle],
    materialsCE :: [Material],
    altNamesCE :: [AlternativeName]
  }
  deriving (Show, Eq, Ord, Generic)

collectExercise ::
  MonadIO m =>
  ExerciseUUID ->
  SqlPersistT m (Maybe CompleteExercise)
collectExercise uuid = do
  res <- getBy $ UniqueExerciseUUID uuid
  case res of
    Nothing -> pure Nothing
    Just (Entity _ ex@Exercise {..}) -> do
      muscleFilters <- selectListVals [MuscleFilterExercise ==. exerciseUuid] []
      let muscles = muscleFilterMuscle <$> muscleFilters
      materials <- collectMaterials uuid
      names <- collectAltNames uuid
      pure . Just $
        CompleteExercise
          { exerciseCE = ex,
            musclesCE = muscles,
            materialsCE = materials,
            altNamesCE = names
          }

collectMaterials :: MonadIO m => ExerciseUUID -> SqlPersistT m [Material]
collectMaterials uuid = do
  materialFilters <- selectListVals [MaterialFilterExercise ==. uuid] []
  let materialUuids = materialFilterMaterial <$> materialFilters
  materials <- forM materialUuids $ \mUuid ->
    entityVal <$$> getBy (UniqueMaterialUUID mUuid)
  pure $ Material . exerciseMaterialName <$> catMaybes materials

collectAltNames :: MonadIO m => ExerciseUUID -> SqlPersistT m [AlternativeName]
collectAltNames uuid =
  toAltName <$$> selectListVals [ExerciseAlternativeNameUuid ==. uuid] []
  where
    toAltName = AlternativeName . exerciseAlternativeNameName

collectAllMaterials :: MonadIO m => SqlPersistT m [ExerciseMaterial]
collectAllMaterials = selectListVals [] []

alternativeNamesText :: [AlternativeName] -> Text
alternativeNamesText = T.intercalate ", " . fmap unAlternative

altNames :: Text -> [AlternativeName]
altNames t = AlternativeName <$> T.splitOn ", " t

collectCustomerCoachProposals :: MonadIO m => Coach -> SqlPersistT m [User]
collectCustomerCoachProposals Coach {..} = do
  entities <- selectListVals [CustomerCoachRelationCoach ==. coachUuid, CustomerCoachRelationResponse ==. Nothing] []
  let userIds = customerCoachRelationCustomer <$> entities
  fmap catMaybes $ forM userIds $ fmap (fmap entityVal) . getBy . UniqueUser

data SqlUpdateResult
  = SqlSuccess
  | SqlNotFound
  | SqlAlreadyUpdated
  deriving (Show, Eq, Ord, Generic)

-- Respond to a customer coach proposal.
respondToProposal ::
  MonadIO m =>
  UserUUID ->
  Coach ->
  CoachProposalResponse ->
  SqlPersistT m SqlUpdateResult
respondToProposal user coach response =
  getBy (UniqueRelation user $ coachUuid coach) >>= \case
    Nothing -> pure SqlNotFound
    Just (Entity key val) -> case customerCoachRelationResponse val of
      Just _ -> pure SqlAlreadyUpdated
      Nothing -> do
        update key [CustomerCoachRelationResponse =. Just response]
        pure SqlSuccess

-- Respond to a friend request.
respondToFriendRequest :: MonadIO m => UserUUID -> User -> FriendRequestResponse -> SqlPersistT m SqlUpdateResult
respondToFriendRequest user currentUser response =
  -- The current user must be the receiver of the friend request
  getBy (UniqueFriend user $ userUuid currentUser) >>= \case
    Nothing -> pure SqlNotFound
    Just (Entity key val) -> case friendRelationResponse val of
      Just _ -> pure SqlAlreadyUpdated
      Nothing -> do
        update key [FriendRelationResponse =. Just response]
        pure SqlSuccess

data CoachOrganized = CoachOrganized | UserOrganized
  deriving (Show, Eq, Ord, Generic)

data Workout = Workout
  { organizerW :: User, -- User/coach who 'organized' the workout
    workoutTypeW :: WorkoutType,
    workoutAmountW :: WorkoutAmount,
    dayW :: Day,
    coachOrganized :: CoachOrganized
  }
  deriving (Show, Eq, Ord, Generic)

getLastWeeksWorkouts :: MonadIO m => Day -> User -> SqlPersistT m [Workout]
getLastWeeksWorkouts today currentUser = do
  friends <- fmap friendFRI . filter isFriend <$> collectFriendInfos currentUser
  coaches <- collectCoachesAndUser currentUser
  let allUsers = uniq $ currentUser : friends ++ (fst <$> coaches)
      usersMap = Map.fromList $ allUsers <&> \user -> (userUuid user, user)
  userWorkouts <- selectListVals (userConditions allUsers) []
  let userWorkouts' =
        userWorkouts <&> \workout ->
          Workout
            { organizerW = getUser usersMap $ userWorkoutUser workout,
              workoutTypeW = userWorkoutType workout,
              workoutAmountW = userWorkoutAmount workout,
              dayW = userWorkoutDay workout,
              coachOrganized = UserOrganized
            }
  coachWorkouts <- selectListVals (coachConditions $ snd <$> coaches) []
  let coachWorkouts' =
        catMaybes $
          coachWorkouts <&> \workout ->
            getCoachUser coaches (coachWorkoutCoach workout) <&> \organizer ->
              Workout
                { organizerW = organizer,
                  workoutTypeW = coachWorkoutType workout,
                  workoutAmountW = coachWorkoutAmount workout,
                  dayW = coachWorkoutDay workout,
                  coachOrganized = CoachOrganized
                }
  pure $ userWorkouts' ++ coachWorkouts'
  where
    lastWeek = addDays (-7) today
    userConditions users =
      [ UserWorkoutUser <-. (userUuid <$> users),
        UserWorkoutDay >. lastWeek
      ]
    coachConditions cs =
      [ CoachWorkoutCoach <-. (coachUuid <$> cs),
        CoachWorkoutDay >. lastWeek,
        CoachWorkoutDay <=. today
      ]
    getUser usersMap userUuid = usersMap Map.! userUuid
    getCoachUser coaches uuid = case filter ((== uuid) . coachUuid . snd) coaches of
      [(user, _)] -> Just user
      _ -> Nothing

uniq :: Ord a => [a] -> [a]
uniq = nubOrd

getCoachWorkouts :: MonadIO m => Coach -> SqlPersistT m [CoachWorkout]
getCoachWorkouts Coach {..} = selectListVals [CoachWorkoutCoach ==. coachUuid] []

getParticipants :: MonadIO m => CoachWorkout -> SqlPersistT m [User]
getParticipants CoachWorkout {..} = do
  workoutJoins <- selectListVals conditions []
  fmap catMaybes $
    forM workoutJoins $ \WorkoutJoin {..} ->
      fmap entityVal <$> getBy (UniqueUser workoutJoinCustomer)
  where
    conditions =
      [ WorkoutJoinWorkout ==. coachWorkoutUuid,
        WorkoutJoinStatus !=. Cancelled
      ]

collectCoaches :: MonadIO m => User -> SqlPersistT m [Coach]
collectCoaches User {..} = do
  relations <- selectListVals conditions []
  fmap catMaybes $
    forM relations $ \CustomerCoachRelation {..} ->
      fmap entityVal <$> getBy (UniqueCoach customerCoachRelationCoach)
  where
    conditions =
      [ CustomerCoachRelationCustomer ==. userUuid,
        CustomerCoachRelationResponse ==. Just AcceptProposal
      ]

collectCoachesAndUser :: MonadIO m => User -> SqlPersistT m [(User, Coach)]
collectCoachesAndUser user = do
  coaches <- collectCoaches user
  fmap catMaybes $
    forM coaches $ \coach ->
      fmap ((,coach) . entityVal) <$> getBy (UniqueUser $ coachUser coach)

collectCustomers :: MonadIO m => Coach -> SqlPersistT m [User]
collectCustomers Coach {..} = do
  relations <- selectListVals conditions []
  fmap catMaybes $
    forM relations $ \relation ->
      fmap entityVal <$> getBy (UniqueUser $ customerCoachRelationCustomer relation)
  where
    conditions =
      [ CustomerCoachRelationCoach ==. coachUuid,
        CustomerCoachRelationResponse ==. Just AcceptProposal
      ]

data CoachWorkoutInfo = CoachWorkoutInfo
  { uuidCWI :: CoachWorkoutUUID,
    coachCWI :: User,
    typeCWI :: WorkoutType,
    timeCWI :: LocalTime,
    amountCWI :: WorkoutAmount,
    addressCWI :: Textarea,
    notesCWI :: Textarea,
    participants :: [User]
  }
  deriving (Show, Eq, Ord, Generic)

getCoachWorkoutInfos :: MonadIO m => Coach -> SqlPersistT m [CoachWorkoutInfo]
getCoachWorkoutInfos coach =
  fmap catMaybes $ getCoachWorkouts coach >>= traverse workoutToInfo

getCoachU :: MonadIO m => CoachUUID -> SqlPersistT m (Maybe (User, Coach))
getCoachU uuid =
  getBy (UniqueCoach uuid) >>= \case
    Nothing -> pure Nothing
    Just (Entity _ coach@Coach {..}) ->
      getBy (UniqueUser coachUser) >>= \case
        Nothing -> pure Nothing
        Just (Entity _ user) -> pure $ Just (user, coach)

workoutToInfo :: MonadIO m => CoachWorkout -> SqlPersistT m (Maybe CoachWorkoutInfo)
workoutToInfo cw@CoachWorkout {..} =
  getCoachU coachWorkoutCoach >>= \case
    Nothing -> pure Nothing
    Just (user, Coach {..}) ->
      fmap Just $
        getParticipants cw <&> \participants ->
          CoachWorkoutInfo
            { uuidCWI = coachWorkoutUuid,
              coachCWI = user,
              typeCWI = coachWorkoutType,
              timeCWI = LocalTime coachWorkoutDay coachWorkoutTimeOfDay,
              amountCWI = coachWorkoutAmount,
              addressCWI = coachWorkoutAddress,
              notesCWI = coachWorkoutNotes,
              participants = participants
            }

getMyCoachesWorkoutInfos :: MonadIO m => User -> SqlPersistT m [CoachWorkoutInfo]
getMyCoachesWorkoutInfos user = do
  coaches <- collectCoaches user
  join <$> forM coaches getCoachWorkoutInfos

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) func = fmap (fmap func)

userPlannedWorkouts :: MonadIO m => User -> SqlPersistT m [(JoinStatus, CoachWorkoutInfo)]
userPlannedWorkouts User {..} = do
  workoutJoins <- selectListVals conditions []
  fmap catMaybes $
    forM workoutJoins $ \WorkoutJoin {..} ->
      getBy (UniqueCoachWorkout workoutJoinWorkout) >>= \case
        Nothing -> pure Nothing
        Just (Entity _ workout) ->
          (workoutJoinStatus,) <$$> workoutToInfo workout
  where
    conditions = [WorkoutJoinCustomer ==. userUuid]

data FriendRequestInfo = FriendRequestInfo
  { friendFRI :: User,
    friendTypeFRI :: FriendType,
    responseFRI :: Maybe FriendRequestResponse
  }
  deriving (Show, Eq, Ord, Generic)

friendUuid :: FriendRelation -> UserUUID -> UserUUID
friendUuid FriendRelation {..} userUuid
  | friendRelationProposer == userUuid = friendRelationReceiver
  | otherwise = friendRelationProposer

collectFriendInfos :: MonadIO m => User -> SqlPersistT m [FriendRequestInfo]
collectFriendInfos User {..} = do
  proposeds <- selectListVals [FriendRelationProposer ==. userUuid] []
  receiveds <- selectListVals [FriendRelationReceiver ==. userUuid] []
  proposedFRIs <- forM proposeds $ \fr@FriendRelation {..} ->
    (\(Entity _ user) -> FriendRequestInfo user Proposer friendRelationResponse)
      <$$> getBy (UniqueUser $ friendUuid fr userUuid)
  receivedFRIs <- forM receiveds $ \fr@FriendRelation {..} ->
    (\(Entity _ user) -> FriendRequestInfo user Receiver friendRelationResponse)
      <$$> getBy (UniqueUser $ friendUuid fr userUuid)
  pure $ catMaybes proposedFRIs ++ catMaybes receivedFRIs

collectFriends :: MonadIO m => User -> SqlPersistT m [User]
collectFriends user = fmap friendFRI . filter isFriend <$> collectFriendInfos user

isReceiver :: FriendRequestInfo -> Bool
isReceiver FriendRequestInfo {..} =
  isNothing responseFRI && friendTypeFRI == Receiver

isFriend :: FriendRequestInfo -> Bool
isFriend FriendRequestInfo {..} = responseFRI == Just AcceptFriend
