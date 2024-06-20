#!/bin/bash

if [ $# -lt 3 ]
  then
    echo "./run.sh <projectFolder> <outFolderCpg> <outFolderConsumer>"
    exit 1
fi

projectFolder=$1
outFolderCpg=$2
outFolderConsumer=$3

docker run -v "${projectFolder}":/in/ \
  -v "${outFolderCpg}":/out/cpg/ \
  -v "${outFolderConsumer}":/out/scama/ \
  surfer run ${@:$#}
