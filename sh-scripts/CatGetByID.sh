#!/bin/bash
echo Enter category ID
read catID
echo $(curl http://localhost:3000/categories/$catID)
