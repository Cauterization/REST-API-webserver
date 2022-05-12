#!/bin/bash
echo Enter created_at filter in YYYY-MM-DD format
read createdAt
echo $(curl http://localhost:3000/articles?crAt=$createdAt)
