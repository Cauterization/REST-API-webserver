#!/bin/bash
echo Enter token
read token
echo $(curl -H "Authorization:$token" http://localhost:3000/drafts)
