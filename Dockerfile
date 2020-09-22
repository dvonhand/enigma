FROM ubuntu:latest

ENV STACK_VERSION=2.3.3

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    ca-certificates \
    gcc \
    libc-dev \
    libgmp3-dev \
    make \
    wget \
    xz-utils \
  && wget https://github.com/commercialhaskell/stack/releases/download/v${STACK_VERSION}/stack-${STACK_VERSION}-linux-x86_64.tar.gz \
  && cd /opt \
  && tar xzf /stack-${STACK_VERSION}-linux-x86_64.tar.gz \
  && apt-get purge -y \
    wget \
  && apt-get autoremove -y --purge \
  && rm -rf /var/lib/apt/lists/* \
  && export PATH=$PATH:/opt/stack-${STACK_VERSION}-linux-x86_64 \
  && stack setup \
  && rm /stack-${STACK_VERSION}-linux-x86_64.tar.gz

ENV PATH=$PATH:/opt/stack-${STACK_VERSION}-linux-x86_64

RUN mkdir -p /root/project

VOLUME [ "/root/project" ]

WORKDIR /root/project
