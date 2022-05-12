#!/bin/bash
echo Enter article ID
read articleID
echo $(curl http://localhost:3000/articles/$articleID)
