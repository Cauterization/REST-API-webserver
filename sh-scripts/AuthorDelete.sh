#!/bin/bash
echo Enter author ID
read authorID
echo Enter token
read token
echo $(curl -X DELETE -H "Authorization:$token" http://localhost:3000/admin/authors/$authorID)
