# Rest API webserver (no frameworks)
Test project for Metalamp. Description of requirements aviable [here](https://coda.io/@metalamp/education/4-15) and [here](https://coda.io/@metalamp/education/5-16).

## Getting started
All you need to do to run server is write your database hostname, port, user login and password into configuration file. After that you need to run main.hs file with --conf ./configTemplate.dhall and -m command line arguments. 

## Architecture
Server is written using HKD and MTL patterns and consists of several large modules, the contents of which are in their own separate folder.


## Api
The Api module is responsible for all functionality of each server endpoint. Here are presented both specific functions for interacting with certain entities (they are located in folders with names corresponding to the names of entities) and generic functions for entities that do not require specific logic for post/get/put/delete/publish request methods. Also here presented server middlewares for basic authentication and draft access.


## App
This module contains the code responsible for inner backend logic. 

### Endpoints
This folder contains a list of all server endpoints which is represented through an specific router instances for each server entity and endpoints associated with them. These endpoints consists of method name, URL and specific function from Api, or just method and URL if this function is generic (in this cases method name ends with "_"). All this methods are bunch together throught dummy Main type and router instrance of it, which is just enumeration of all other instances and main server middlwewares. 

### RunServer
Here are located functions that start server, receive wai request, process it through transformer stack, convert final result to response and sends it back.  

### Router
This folder contains the topmost transformer layer - the router, which is responsible for choosing right endpoint when server receives some request.

### AppT
Here you can find second transformer layer - AppT which do most of the work. It is just newtype on top of ReaderT with server environment and underlying monad which determines whether the server is running in prod mode or in test mode (for production this monad is just IO and for test purposes there exists some other pure mtl stack). All database logic also lives in this monad.

### Other folders
Also here is some more folders - result and resultJSON, where you can find code, responsible for processing the final result of each endpoint; config which describe config's data; queryParams which describe http query parameters list parsing; types where most commonly used types are located; and iternal for internal code for all of other modules.

## Database
Server database is represented through HasDatabase typeclass which contains all the functions that may be needed to describe the logic of interactions with the database. At current moment server uses postgresql and PostgreSQL.Simple library, the instantiation of which is located at postgres folder.

## Entity
All server entities are located at this module. They are presented through higher kinded data, parameterized by a specific role, which defines the behavior of the entity associated with concrete request type. All this roles descriptions and other HKD-type stuff are located at HKD module. 
