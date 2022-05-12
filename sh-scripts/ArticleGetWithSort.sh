#!/bin/bash
echo Choose sorting from date/author/category/photos
read sorting
echo Type desc for descending direction or anything else otherwise
read direction
echo $(curl http://localhost:3000/articles?sort=$sorting,$direction)
