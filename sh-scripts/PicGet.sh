#!/bin/bash
echo Enter picture ID
read picID
echo $(curl http://localhost:3000/pictures/$picID)

