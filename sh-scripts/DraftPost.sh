#!/bin/bash
echo Enter title
read title
echo Enter content
read content
echo Enter category ID
read categoryID
echo Enter tags IDs separated by comma
read tags
echo Enter pics IDs separated by comma
read pics
echo Enter token
read token
body={\"title\":\"$title\",\"content\":\"$content\",\"category\":$categoryID,\"tags\":\[$tags\],\"pics\":\[$pics\]}
echo $(curl -X POST -H "Content-Type: application/json" -H "Authorization:$token" -d $body http://localhost:3000/drafts)
