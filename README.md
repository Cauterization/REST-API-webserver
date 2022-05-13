# Rest API webserver (no frameworks)
Test project for Metalamp. Description of requirements aviable [here](https://coda.io/@metalamp/education/4-15) and [here](https://coda.io/@metalamp/education/5-16).


## Architecture
Server is written using HKD and MTL patterns and consists of several large modules, the contents of which are in their own separate folder.


## Api
The Api module is responsible for all functionality of each server endpoint. Here are presented both specific functions for interacting with certain entities (they are located in folders with names corresponding to the names of entities) and generic functions for entities that do not require specific logic for post/get/put/delete/publish reuest methods. Also here presented server middlewares (authentication middleware for example).


## App
This module contains the code responsible for inner backend logic. 

### Router
This folder contains the topmost transformer layer - the router, which is responsible for choosing right endpoint when server recieves some request.

### App
This folder contains a list of all server endpoints which is represented through an specific router instances for each server entity and endpoints associated with them. These endpoints consists of method name, URL and specific function from Api, or just method and URL if this function is generic (in this cases method name ends with "_"). All this methods are bunch togetger throught dummy Main type and router instrance of it, which is just enumeration of all other instances and main server middlwewares. Also here are located functions that start server, recieve wai request, process it through transformer stack, convert final result to response and sends it back.  

### Other folder
Also here is some more folders - result and resultJSON, where you can find code, responsible for processing the final result of each endpoint; config which describe config's data; queryParams which describe http query parameters list parsing; types where most commonly used types are located; and iternal for internal code for all of other modules.
Also in internal module located second transformer layer - AppT which do most of the work. It is just newtype on top of ReaderT with server environment and underlying monad which determines whether the server is running in prod mode or in test mode (for production this monad is just IO and for test purposes there exists some other mtl stack). All database logic also lives in this monad.

## Database
Server database is represented through two type classes. One of then is isDatabse class that responsible for database behavior itself. It located in internal folder. The other is hasDatabase that is responsible for interaction with server. At current moment server uses postgresql and PostgreSQL.Simple library, the instantiation of which is located at postgres folder. But if required, the database can be easily changed by just creating new isDatabase instance.

## Entity
All server entities are located at this module. They are presented through higher kinded data, parameterized by a specific role, which defines the behavior of the entity associated with concrete request type. All this roles descriptions and other HKD-type stuff are located at HKD module. 
