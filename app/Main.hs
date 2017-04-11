-- HTTP Simple tutorial found here: https://haskell-lang.org/library/http-client

module Main where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text as T
import GHC.Generics
import Network.HTTP.Simple

--writeJson writes a JSON
--readJson calls print {x} to read JSON
main :: IO ()
main = do
    putStrLn "Type the resource to GET: people, films, species, locations, vehicles"
    resource <- getLine
    if resource == "people" then writeJson resource >> readJsonPeople
      else if resource == "films" then writeJson resource >> readJsonFilms
      else if resource == "species" then writeJson resource >> readJsonSpecies
      else if resource == "locations" then writeJson resource >> readJsonLocations
      else if resource == "vehicles" then writeJson resource >> readJsonVehicles
    else putStrLn "invalid choice" >> main

writeJson :: String -> IO ()
writeJson resource = do
  response <- httpLBS (request resource)
  putStrLn $ "The status code was: " ++
             show (getResponseStatusCode response)
  print $ getResponseHeader "Content-Type" response
  let status = getResponseStatusCode response
  if status == 200
      then do
          print "saving request to file"
          let jsonBody = getResponseBody response
          --append resource name to json file: {resource:[data]}
          writeFile (L8.pack "{\"")
          mapM_ appendFile [L8.pack resource,L8.pack "\":",jsonBody,L8.pack "}"]
      else print "request failed with error"
        where writeFile = L.writeFile jsonFile
              jsonFile = mconcat [resource,".json"]
              appendFile = L.appendFile jsonFile

readJsonPeople :: IO ()
readJsonPeople = do
    jsonData <- L.readFile "people.json"
    let ghibliResponse = decode jsonData :: Maybe GhibliPersonResponse
    let ghibliResults = people <$> ghibliResponse
    printPerson ghibliResults

readJsonFilms :: IO ()
readJsonFilms = do
    jsonData <- L.readFile "films.json"
    let ghibliResponse = decode jsonData :: Maybe GhibliFilmResponse
    let ghibliResults = films <$> ghibliResponse
    printFilm ghibliResults

readJsonSpecies :: IO ()
readJsonSpecies = do
    jsonData <- L.readFile "species.json"
    let ghibliResponse = decode jsonData :: Maybe GhibliSpeciesResponse
    let ghibliResults = species <$> ghibliResponse
    printSpecies ghibliResults

readJsonLocations :: IO ()
readJsonLocations = do
    jsonData <- L.readFile "locations.json"
    let ghibliResponse = decode jsonData :: Maybe GhibliLocationResponse
    let ghibliResults = locations <$> ghibliResponse
    printLocation ghibliResults

readJsonVehicles :: IO ()
readJsonVehicles = do
    jsonData <- L.readFile "vehicles.json"
    let ghibliResponse = decode jsonData :: Maybe GhibliVehicleResponse
    let ghibliResults = vehicles <$> ghibliResponse
    printVehicle ghibliResults

ghibliHost :: B8.ByteString
ghibliHost = "ghibliapi.herokuapp.com"

apiPath :: String -> B8.ByteString
apiPath resource = mconcat ["/", B8.pack resource]

requestBuild :: B8.ByteString -> B8.ByteString -> B8.ByteString -> Request
requestBuild host method path = setRequestMethod method
                              $ setRequestHost host
                              $ setRequestPath path
                              $ setRequestSecure True
                              $ setRequestPort 443 defaultRequest

requestBuildNOSSL :: B8.ByteString -> B8.ByteString -> B8.ByteString -> Request
requestBuildNOSSL host method path = setRequestMethod method
                                   $ setRequestHost host
                                   $ setRequestSecure False
                                   $ setRequestPort 80
                                   $ setRequestPath path defaultRequest

request :: String -> Request
request resource = requestBuild ghibliHost "GET" (apiPath resource)

requestBad :: String -> Request
requestBad resource = requestBuildNOSSL (mconcat["http://",ghibliHost]) "GET" (apiPath resource)

data GhibliPersonResult = GhibliPersonResult
    { personId :: T.Text
    , namePeople :: T.Text
    , gender :: T.Text
    , age :: T.Text
    , hairColor :: T.Text
    , eyeColor :: T.Text
    , filmsPeople :: [T.Text]
    , speciesPeople :: T.Text
    , urlPerson :: T.Text
    } deriving Show

data GhibliFilmResult = GhibliFilmResult
    { filmId :: T.Text
    , title :: T.Text
    , descriptionFilm :: T.Text
    , director :: T.Text
    , producer :: T.Text
    , releaseDate :: T.Text
    , rtScore :: T.Text
    , peopleFilm :: [T.Text]
    , speciesFilm :: [T.Text]
    , locationsFilm :: [T.Text]
    , vehiclesFilm :: [T.Text]
    , urlFilm :: T.Text
    } deriving Show

data GhibliSpeciesResult = GhibliSpeciesResult
    { speciesId :: T.Text
    , nameSpecies :: T.Text
    , classification :: T.Text
    , eyeColors :: T.Text
    , hairColors :: T.Text
    , peopleSpecies :: [T.Text]
    , filmsSpecies :: [T.Text]
    , urlSpecies :: T.Text
    } deriving Show

data GhibliLocationResult = GhibliLocationResult
    { locationsId :: T.Text
    , nameLocations :: T.Text
    , climate :: T.Text
    , terrain :: T.Text
    , surfaceWater :: T.Text
    , residents :: [T.Text]
    , filmsLocations :: [T.Text]
    , urlLocations :: [T.Text]
    } deriving Show

data GhibliVehicleResult = GhibliVehicleResult
    { vehiclesId :: T.Text
    , nameVehicles :: T.Text
    , descriptionVehicles :: T.Text
    , vehicleClass :: T.Text
    , vehicleLength :: T.Text
    , pilot :: T.Text
    , filmsVehicles :: T.Text
    , urlVehicles :: T.Text
    } deriving Show

instance FromJSON GhibliPersonResult where
    parseJSON (Object v) =
        GhibliPersonResult <$> v .: "id"
                           <*> v .: "name"
                           <*> v .: "gender"
                           <*> v .: "age"
                           <*> v .: "hair_color"
                           <*> v .: "eye_color"
                           <*> v .: "films"
                           <*> v .: "species"
                           <*> v .: "url"

instance FromJSON GhibliFilmResult where
    parseJSON (Object v) =
        GhibliFilmResult <$> v .: "id"
                         <*> v .: "title"
                         <*> v .: "description"
                         <*> v .: "director"
                         <*> v .: "producer"
                         <*> v .: "release_date"
                         <*> v .: "rt_score"
                         <*> v .: "people"
                         <*> v .: "species"
                         <*> v .: "locations"
                         <*> v .: "vehicles"
                         <*> v .: "url"

instance FromJSON GhibliSpeciesResult where
   parseJSON (Object v) =
       GhibliSpeciesResult <$> v .: "id"
                           <*> v .: "name"
                           <*> v .: "classification"
                           <*> v .: "eye_colors"
                           <*> v .: "hair_colors"
                           <*> v .: "people"
                           <*> v .: "films"
                           <*> v .: "url"

instance FromJSON GhibliLocationResult where
  parseJSON (Object v) =
      GhibliLocationResult <$> v .: "id"
                          <*> v .: "name"
                          <*> v .: "climate"
                          <*> v .: "terrain"
                          <*> v .: "surface_water"
                          <*> v .: "residents"
                          <*> v .: "films"
                          <*> v .: "url"

instance FromJSON GhibliVehicleResult where
   parseJSON (Object v) =
       GhibliVehicleResult <$> v .: "id"
                           <*> v .: "name"
                           <*> v .: "description"
                           <*> v .: "vehicle_class"
                           <*> v .: "length"
                           <*> v .: "pilot"
                           <*> v .: "films"
                           <*> v .: "url"

newtype GhibliPersonResponse = GhibliPersonResponse
                    { people :: [GhibliPersonResult]
                    } deriving (Show,Generic)

newtype GhibliFilmResponse = GhibliFilmResponse
                    { films :: [GhibliFilmResult]
                    } deriving (Show,Generic)

newtype GhibliSpeciesResponse = GhibliSpeciesResponse
                    { species :: [GhibliSpeciesResult]
                    } deriving (Show,Generic)

newtype GhibliLocationResponse = GhibliLocationResponse
                    { locations :: [GhibliLocationResult]
                    } deriving (Show,Generic)

newtype GhibliVehicleResponse = GhibliVehicleResponse
                    { vehicles :: [GhibliVehicleResult]
                    } deriving (Show,Generic)

instance FromJSON GhibliPersonResponse

instance FromJSON GhibliFilmResponse

instance FromJSON GhibliSpeciesResponse

instance FromJSON GhibliLocationResponse

instance FromJSON GhibliVehicleResponse

printPerson :: Maybe [GhibliPersonResult] -> IO ()
printPerson Nothing = print "No data"
printPerson (Just people) = do
    print $ T.pack "Studio Ghibli People:"
    forM_ people $ \person -> do
        let resourceName = namePeople person
        let resourceGender = gender person
        let resourceHairColor = hairColor person
        let resourceEyeColor = eyeColor person
        let resourceFilms = filmsPeople person
        print $ T.concat [T.pack "name: ", resourceName
                         ,T.pack ", gender: ", resourceGender
                         ,T.pack ", hair color: ", resourceHairColor
                         ,T.pack ", eye color: ", resourceEyeColor
                         ]

printFilm :: Maybe [GhibliFilmResult] -> IO ()
printFilm Nothing = print "No data"
printFilm (Just films) = do
    print $ T.pack "Studio Ghibli Films:"
    forM_ films $ \film -> do
        let resourceTitle = title film
        let resourceDescription = descriptionFilm film
        let resourceDirector = director film
        let resourceRTScore = rtScore film
        print $ T.concat [T.pack "title: ", resourceTitle
                         ,T.pack ", description: ", resourceDescription
                         ,T.pack ", director: ", resourceDirector
                         ,T.pack ", rt_score: ", resourceRTScore
                         ]

printSpecies :: Maybe [GhibliSpeciesResult] -> IO ()
printSpecies Nothing = print "No data"
printSpecies (Just species) = do
   print $ T.pack "Studio Ghibli Species:"
   forM_ species $ \specie -> do
       let resourceName = nameSpecies specie
       let resourceClassification = classification specie
       let resourceEyeColors = eyeColors specie
       let resourceHairColors = hairColors specie
       print $ T.concat [T.pack "name: ", resourceName
                        ,T.pack ", classification: ", resourceClassification
                        ,T.pack ", eye colors: ", resourceEyeColors
                        ,T.pack ", hair colors: ", resourceHairColors
                        ]

printLocation :: Maybe [GhibliLocationResult] -> IO ()
printLocation Nothing = print "No data"
printLocation (Just locations) = do
   print $ T.pack "Studio Ghibli Locations:"
   forM_ locations $ \location -> do
       let resourceName = nameLocations location
       let resourceClimate = climate location
       let resourceTerrain = terrain location
       let resourceWater = surfaceWater location
       print $ T.concat [T.pack "name: ", resourceName
                        ,T.pack ", climate: ", resourceClimate
                        ,T.pack ", terrain: ", resourceTerrain
                        ,T.pack ", surface_water: ", resourceWater
                        ]

printVehicle :: Maybe [GhibliVehicleResult] -> IO ()
printVehicle Nothing = print "No data"
printVehicle (Just vehicles) = do
   print $ T.pack "Studio Ghibli Vehicles:"
   forM_ vehicles $ \vehicle -> do
       let resourceName = nameVehicles vehicle
       let resourceDescription = descriptionVehicles vehicle
       let resourceClass = vehicleClass vehicle
       let resourceLength = vehicleLength vehicle
       let resourcePilot = pilot vehicle
       print $ T.concat [T.pack "name: ", resourceName
                        ,T.pack ", description: ", resourceDescription
                        ,T.pack ", class: ", resourceClass
                        ,T.pack ", length: ", resourceLength
                        ,T.pack ", pilot: ", resourcePilot
                        ]
