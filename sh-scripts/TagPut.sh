#!/bin/bash
echo Enter tag ID
read tagID
echo Enter new tag name
read tagName
echo Enter token
read token
body={\"tag\":\"$tagName\"}
echo $(curl -X PUT -H "Content-Type: application/json" -H "Authorization:$token" -d $body http://localhost:3000/admin/tags/$tagID)
