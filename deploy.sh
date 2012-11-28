#!/bin/bash

# Deploy script for Amazon S3

# We sync blog entries first, so that we can add the content-type
# header and omit .html extensions.
echo Syncing html pages from blog
s3cmd sync _site/blog/ \
	s3://www.hdevalence.ca/blog/ \
	--add-header='Cache-Control':'public, max-age 2592000' \
	--mime-type='text/html' -M \
	--delete-removed

echo Syncing other data
s3cmd sync _site/ \
	s3://www.hdevalence.ca/ \
	--add-header='Cache-Control':'public, max-age 2592000'

