#!/bin/bash
echo Enter category ID
read catID
echo Enter token
read token
echo $(curl -X DELETE -H "Authorization:$token" http://localhost:3000/admin/categories/$catID)
