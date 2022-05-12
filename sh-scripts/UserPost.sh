#!/bin/bash
echo Enter firstname
read firstname
echo Enter lastname
read lstname
echo Enter login
read login
echo Enter password
read password
body={\"first_name\":\"$firstname\",\"last_name\":\"$lastname\",\"login\":\"$login\",\"password\":\"$password\",\"admin\":false}
echo $(curl -X POST -H "Content-Type: application/json" -d $body http://localhost:3000/users)
