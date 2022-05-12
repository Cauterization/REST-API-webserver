#!/bin/bash
echo Enter description
read description
echo Enter user ID
read userID
echo Enter token
read token
body={\"user\":$userID,\"description\":\"$description\"}
echo $(curl -X POST -H "Content-Type: application/json" -H "Authorization:$token" -d $body http://localhost:3000/admin/authors)
