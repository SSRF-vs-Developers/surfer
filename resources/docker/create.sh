#!/bin/bash

branch=${1:-"master"}
DOCKER_BUILDKIT=1 docker build --build-arg BRANCH="${branch}" --no-cache --ssh default -t surfer .