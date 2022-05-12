#!/bin/bash
echo Enter tag name
read tag
echo Enter token
read token
body={\"tag\":\"$tag\"}
echo $(curl -X POST -H "Content-Type: application/json" -H "Authorization:$token" -d $body http://localhost:3000/admin/tags)
