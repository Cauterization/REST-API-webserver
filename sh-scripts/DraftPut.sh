#!/bin/bash
echo Enter draft ID
read draftID
echo Enter new title
read title
echo Enter new content
read content
echo Enter new category ID
read categoryID
echo Enter new tags IDs separated by comma
read tags
echo Enter new pics IDs separated by comma
read pics
body={\"title\":\"$title\",\"content\":\"$content\",\"category\":$categoryID,\"tags\":\[$tags\],\"pics\":\[$pics\]}
echo Enter token
read token
echo $(curl -X PUT -H "Content-Type: application/json" -H "Authorization:$token" -d $body http://localhost:3000/drafts/$draftID)
