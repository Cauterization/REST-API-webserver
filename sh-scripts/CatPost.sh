#!/bin/bash
echo Enter category name
read catName
echo Enter token
read token
body={\"name\":\"$catName\"}
echo $(curl -X POST -H "Content-Type: application/json" -H "Authorization:$token" -d $body http://localhost:3000/admin/categories)
