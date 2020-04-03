#!/bin/bash

set -e

if ! command -v yarn > /dev/null; then
  echo "Please install yarn and try again"
  exit 1
fi

if ! command -v nginx > /dev/null; then
  echo "Please install nginx and try again"
  exit 1
fi

yarn -s parcel serve --log-level=1 index.html &

echo "STARTING PROXY AT http://localhost:5000"

nginx -c "$PWD/nginx.conf"
