# Install chez
FROM debian:bullseye AS build-chez

RUN apt-get update && apt-get install -y \
        curl build-essential git uuid-dev make ncurses-dev

WORKDIR /root/
RUN curl -L https://github.com/cisco/ChezScheme/releases/download/v9.6.4/csv9.6.4.tar.gz | tar -zx
RUN mv csv9.6.4 ChezScheme

WORKDIR /root/ChezScheme
RUN ./configure --threads --disable-x11
RUN make && make install

WORKDIR /root/
RUN git clone https://github.com/gwatt/chez-exe.git

WORKDIR /root/chez-exe/

RUN /usr/bin/scheme --script gen-config.ss --bootpath /usr/lib/csv9.6.4/ta6le
RUN make install



# Install project with akku (in Alpine)
FROM akkuscm/akku AS akku-install
RUN apk update && apk --no-cache --update add \
        bash

RUN mkdir /root/scheme-langserver/
WORKDIR /root/scheme-langserver/

COPY Akku.lock Akku.manifest /root/scheme-langserver/

# Install deps (most important operation to cache)
RUN akku install

COPY util /root/scheme-langserver/util/
COPY protocol /root/scheme-langserver/protocol/
COPY virtual-file-system /root/scheme-langserver/virtual-file-system/
COPY analysis /root/scheme-langserver/analysis/
COPY tests /root/scheme-langserver/tests/

COPY scheme-langserver.sls output-type-analysis.ss test.sh run.ss build.sh /root/scheme-langserver/

RUN akku install



# Put it all together in Debian
FROM debian:bullseye

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y git make build-essential uuid-dev

# add chez scheme
COPY --from=build-chez /usr/bin/scheme /usr/bin/
COPY --from=build-chez /usr/lib/csv9.6.4/ /usr/lib/csv9.6.4/

# add compile-chez-program
COPY --from=build-chez /usr/local/bin/compile-chez-program /usr/local/bin/
COPY --from=build-chez /usr/local/lib/full-chez.a /usr/local/lib/
COPY --from=build-chez /usr/local/lib/petite-chez.a /usr/local/lib/

# add project
COPY --from=akku-install /root/scheme-langserver/ /root/scheme-langserver/

WORKDIR /root/scheme-langserver/

