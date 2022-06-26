#!/bin/bash
echo Enter created_at filter in YYYY-MM-DD format
read createdAt
echo Enter created_at_lt filter in YYYY-MM-DD format
read createdAtLt
echo Enter created_at_gt filter in YYYY-MM-DD format
read createdAtGt
echo Enter author_login filter in YYYY-MM-DD format
read author_login
echo Enter category_id filter
read category_id
echo Enter tag_id filter
read tag_id
echo Enter tag_in filter - ids separated by coma
read tag_in
echo Enter tag_all filter - ids separated by coma
read tag_all
echo Enter title filter 
read title
echo Enter content filter
read content
echo Enter substring filter
read substring

echo $(curl -g --get \
    --data-urlencode "crAt=$createdAt" \
    --data-urlencode "crAtLt=$createdAtLt" \
    --data-urlencode "crAtGt=$createdAtGt" \
    --data-urlencode "author_login=$author_login" \
    --data-urlencode "category_id=$category_id" \
    --data-urlencode "tag_id=$tag_id" \
    --data-urlencode "tag_in=[$tag_in]" \
    --data-urlencode "tag_all=[$tag_all]" \
    --data-urlencode "title=$title" \
    --data-urlencode "content=$content" \
    --data-urlencode "substring=$substring" \
    http://localhost:3000/articles)