FROM scala-master:latest as surfer
ENV CPG_DOTFILE_EXPORT_URL "git@github.com:PHP-CPG/CpgDotFileExporter.git"
ENV CPG_SLICE_UTIL_REPO_URL "git@github.com:PHP-CPG/slicer.git"
ENV SURFER_REPO_URL "git@github.com:SSRF-vs-Developers/surfer.git"
ENV WORKFOLDER_DOTTER "/surf/dotter/"
ENV WORKFOLDER_SLICE "/surf/cpg-slice-util/"
ENV WORKFOLDER_SURFER "/surf/surfer/"
ARG BRANCH
# get dotfile exporter
RUN --mount=type=ssh git clone --depth 1 ${CPG_DOTFILE_EXPORT_URL} ${WORKFOLDER_DOTTER}
WORKDIR ${WORKFOLDER_DOTTER}
RUN git checkout f822ca49f4668ff3dfecae2506d1834d8e0a550c
RUN sbt publishLocal
# get cpg-slice-util and test it
RUN --mount=type=ssh git clone --depth 1 ${CPG_SLICE_UTIL_REPO_URL} ${WORKFOLDER_SLICE}
WORKDIR ${WORKFOLDER_SLICE}
RUN git checkout 8e7cbc31a4c2a0140ad6ed6d25dcf36fb5d074cc
COPY cpg.conf .
COPY cpg.conf main.conf
RUN sbt test
RUN sbt publishLocal

RUN --mount=type=ssh git clone --depth 1 ${SURFER_REPO_URL} ${WORKFOLDER_SURFER}
WORKDIR ${WORKFOLDER_SURFER}
RUN git checkout 28384004e8dad17f56f7b47e6a9b9e623aeebb28
COPY cpg.conf .
COPY cpg.conf main.conf
RUN git checkout ${BRANCH}
RUN sbt test
RUN sbt stage
# configs
## custom config for cpg creation
COPY cpg.conf /cpg/main.conf
WORKDIR /
## scama configs
COPY "scala-master.conf" "/scala-master/main.conf"
COPY "telegram.conf.example" "/scala-master/telegram.conf"
