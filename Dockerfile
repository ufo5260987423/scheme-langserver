FROM alpine:latest as build-chez

RUN apk update && apk --no-cache --update add \
    gcc git make musl-dev curl \
    ncurses ncurses-dev util-linux-dev

WORKDIR /root/
RUN curl -L https://github.com/cisco/ChezScheme/releases/download/v9.6.4/csv9.6.4.tar.gz | tar -zx
RUN mv csv9.6.4 ChezScheme

WORKDIR /root/ChezScheme
RUN ./configure --threads --disable-x11
RUN make && make install



FROM akkuscm/akku
RUN apk update && apk --no-cache --update add bash

# Ensure that there are no collisions
RUN rm -rf /usr/lib/csv9.6.4/

COPY --from=build-chez /usr/bin/scheme /usr/bin/
COPY --from=build-chez /usr/lib/csv9.6.4/ /usr/lib/csv9.6.4/

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

COPY scheme-langserver.sls output-type-analysis.ss test.sh run.ss /root/scheme-langserver/

# Install project
RUN akku install


ENTRYPOINT [ "./test.sh" ]
