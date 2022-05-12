#!/bin/bash
echo Enter tag ID
read tagID
echo Enter token
read token
echo $(curl -X DELETE -H "Authorization:$token" http://localhost:3000/admin/tags/$tagID)
