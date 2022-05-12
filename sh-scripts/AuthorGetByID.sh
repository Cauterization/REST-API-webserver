#!/bin/bash
echo Enter token
read token
echo Enter author ID
read authorID
echo $(curl -H "Authorization:$token" http://localhost:3000/admin/authors/$authorID)
