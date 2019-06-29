#!/bin/sh

set -eu

GLEAM_ROOT=$(pwd)
TAG=$(git tag --points-at HEAD)
DOCKER_TAG=lpil/gleam:${TAG:1}
ARCHIVE=gleam-$TAG-linux-amd64.tar.gz
CONTAINER_NAME=gleam-linux-builder

cd gleam
docker build . -t $DOCKER_TAG
docker push $DOCKER_TAG

TMP=$(mktemp -d)
cd $TMP
docker run --name $CONTAINER_NAME $DOCKER_TAG --version
docker cp $CONTAINER_NAME:/gleam gleam
docker rm $CONTAINER_NAME
tar -czvf $ARCHIVE gleam

cd $GLEAM_ROOT
hub release edit --attach $TMP/$ARCHIVE --message $TAG $TAG
