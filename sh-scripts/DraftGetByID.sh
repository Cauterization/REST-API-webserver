#!/bin/bash
echo Enter draft ID
read draftID
echo Enter token
read token
echo $(curl -H "Authorization:$token" http://localhost:3000/drafts/$draftID)
