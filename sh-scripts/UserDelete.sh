#!/bin/bash
echo Enter user ID
read userID
echo Enter token
read token
echo $(curl -X DELETE -H "Authorization:$token" http://localhost:3000/admin/users/$userID)
