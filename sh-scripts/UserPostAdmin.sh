#!/bin/bash
echo Enter firstname
read firstname
echo Enter lastname
read lstname
echo Enter login
read login
echo Enter password
read password
user={\"first_name\":\"$firstname\",\"last_name\":\"$lastname\",\"login\":\"$login\",\"password\":\"$password\",\"admin\":true}
echo $(curl -X POST -H "Content-Type: application/json" -d $user http://localhost:3000/users)
