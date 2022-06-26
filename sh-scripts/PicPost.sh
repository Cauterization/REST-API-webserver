#!/bin/bash
echo Enter pic format
read format
echo Enter pic path
read path
location=@/$path
echo $(curl -X POST -H "Content-Type: image/$format" http://localhost:3000/pictures --data-binary $location)