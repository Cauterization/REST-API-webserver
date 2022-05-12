#!/bin/bash
echo Enter author ID
read authorID
echo Enter new description
read description
echo Enter token
read token
body={\"description\":\"$description\"}
echo $(curl -X PUT -H "Content-Type: application/json" -H "Authorization:$token" -d $body http://localhost:3000/admin/authors/$authorID)
