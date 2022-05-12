#!/bin/bash
echo Enter tag ID
read tagID
echo $(curl http://localhost:3000/tags/$tagID)
