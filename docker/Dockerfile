# The Dockerfile tells Docker how to construct the image with your algorithm.
# Once pushed to a repository, images can be downloaded and executed by the
# network hubs.
FROM custom-r-base

# This is a placeholder that should be overloaded by invoking
# docker build with '--build-arg PKG_NAME=...'
ARG PKG_NAME='tiber'
ARG IMAGE_NAME='maintainer/algorithm'

LABEL maintainer="Main Tainer <user@example.com>"

# Install federated package
COPY src/DESCRIPTION /usr/local/R/${PKG_NAME}/DESCRIPTION
WORKDIR /usr/local/R/${PKG_NAME}
RUN Rscript -e 'library(devtools)' -e 'install_deps(".")'
COPY src /usr/local/R/${PKG_NAME}/
RUN R CMD INSTALL --no-multiarch --with-keep.source .

# Change directory to '/app’ and create files that will be
# used to mount input, output and database.
WORKDIR /app

# Tell docker to execute `docker.wrapper()` when the image is run.
ENV PKG_NAME=${PKG_NAME}
ENV IMAGE_NAME=${IMAGE_NAME}
CMD Rscript -e "vtg::docker.wrapper('$PKG_NAME')"
