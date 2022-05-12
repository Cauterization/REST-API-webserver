#!/bin/bash
echo Enter draft ID
read draftID
echo Enter token
read token
echo $(curl -X DELETE -H "Authorization:$token" http://localhost:3000/drafts/$draftID)
