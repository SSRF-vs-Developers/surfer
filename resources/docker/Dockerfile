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
RUN sbt publishLocal
# get cpg-slice-util and test it
RUN --mount=type=ssh git clone --depth 1 ${CPG_SLICE_UTIL_REPO_URL} ${WORKFOLDER_SLICE}
WORKDIR ${WORKFOLDER_SLICE}
COPY cpg.conf .
COPY cpg.conf main.conf
RUN sbt test
RUN sbt publishLocal

RUN --mount=type=ssh git clone --depth 1 ${SURFER_REPO_URL} ${WORKFOLDER_SURFER}
WORKDIR ${WORKFOLDER_SURFER}
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
