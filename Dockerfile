FROM ubuntu:18.04

RUN apt update \
  && apt install -y --no-install-recommends \
    ca-certificates \
    curl \
    wget \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

ARG USER
ARG GROUP

RUN groupadd ${USER} \
  && useradd ${USER} -g ${GROUP} -m

RUN apt update
RUN apt install -y --no-install-recommends gcc
RUN apt install -y --no-install-recommends libc6-dev
RUN apt install -y --no-install-recommends libffi-dev
# RUN apt install -y --no-install-recommends libgmp-dev

RUN wget -qO- "https://get.haskellstack.org/" | sh
# RUN curl -sSL "https://get.haskellstack.org/" | sh

USER ${USER}

RUN stack setup

RUN mkdir /home/${USER}/work

WORKDIR /home/${USER}/work
