###################################################################################################################
# Adapted from https://github.com/microsoft/vscode-dev-containers/blob/master/containers/r/.devcontainer/Dockerfile
# licence: MIT
###################################################################################################################

FROM rocker/r-ver:4.0.3

# Options for setup script
ARG INSTALL_ZSH="false"
ARG INSTALL_FISH="true"
ARG UPGRADE_PACKAGES="true"
ARG USERNAME=eric
ARG USER_UID=1000
ARG USER_GID=$USER_UID

# Remove the docker user since I don't need it for VSCode containers
#RUN groupmod docker --gid 1005 \
#    && usermod --uid 1005 docker

# Install needed packages and setup non-root user. Use a separate RUN statement to add your own dependencies.
COPY library-scripts/*.sh /tmp/library-scripts/
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && /bin/bash /tmp/library-scripts/common-debian.sh "${INSTALL_ZSH}" "${INSTALL_FISH}" "${USERNAME}" "${USER_UID}" "${USER_GID}" "${UPGRADE_PACKAGES}" \
    && apt-get -y install libzip-dev \
    && apt-get autoremove -y && apt-get clean -y && rm -rf /var/lib/apt/lists/*

# [Optional] Uncomment this section to install additional OS packages.
# key dependencies for certain R packages
RUN apt-get update \
    && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends software-properties-common libssl-dev libxml2-dev libcurl4-openssl-dev python3-pip pandoc \
    && apt-get -y install --no-install-recommends libfreetype6-dev libfontconfig1-dev tk \
    && apt-get autoremove -y && apt-get clean -y && rm -rf /var/lib/apt/lists/* 

# install R packages needed for VSCode interaction and package management
RUN install2.r languageserver renv remotes
RUN installGithub.r nx10/httpgd

# install radian via python and pip3
RUN apt-get update \
    && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends python3-setuptools

RUN pip3 install radian

# install dot net core runtime for the R Tools plugin
RUN wget https://packages.microsoft.com/config/ubuntu/20.04/packages-microsoft-prod.deb -O /tmp/packages-microsoft-prod.deb \
    && dpkg -i /tmp/packages-microsoft-prod.deb

RUN apt-get update \
    && apt-get -y install --no-install-recommends apt-transport-https \
    && apt-get -y install --no-install-recommends dotnet-sdk-3.1

# ensure that the renv package cache env var is accessible in default R installation
RUN echo "RENV_PATHS_CACHE=/renv/cache" >> /usr/local/lib/R/etc/Renviron

# copy the modified .Rprofile template to the renv cache
COPY library-scripts/.Rprofile-vscode /renv/.Rprofile-vscode

# [Optional] Uncomment this section to add addtional system dependencies needed for project
RUN apt-get update \
    && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends libtesseract-dev libleptonica-dev tesseract-ocr-eng libmagick++-dev libicu-dev libfreetype6-dev libglu1-mesa-dev libpng-dev libgl1-mesa-dev zlib1g-dev imagemagick libpoppler-cpp-dev

# [Optional] Set the default user. Omit if you want to keep the default as root.
USER $USERNAME

