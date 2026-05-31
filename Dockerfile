# Install chez
FROM debian:bullseye AS build-chez

RUN apt-get update && apt-get install -y \
        curl build-essential git uuid-dev make ncurses-dev zlib1g-dev liblz4-dev

WORKDIR /root/
RUN curl -L https://github.com/cisco/ChezScheme/releases/download/v10.4.1/csv10.4.1.tar.gz | tar -zx
RUN mv csv10.4.1 ChezScheme

WORKDIR /root/ChezScheme
RUN ./configure --threads --disable-x11
RUN make && make install
# compile-chez-program looks for libz.a and liblz4.a in the bootpath.
# Chez 10.x installs them in subdirectories; create symlinks or stubs.
RUN for lib in libz liblz4; do \
        target="/usr/lib/csv10.4.1/ta6le/${lib}.a"; \
        if [ ! -f "$target" ]; then \
            if [ -f "/usr/lib/csv10.4.1/ta6le/zlib/${lib}.a" ]; then \
                ln -s "/usr/lib/csv10.4.1/ta6le/zlib/${lib}.a" "$target"; \
            elif [ -f "/usr/lib/csv10.4.1/ta6le/lz4/lib/${lib}.a" ]; then \
                ln -s "/usr/lib/csv10.4.1/ta6le/lz4/lib/${lib}.a" "$target"; \
            else \
                ar rcs "$target"; \
            fi; \
        fi; \
    done

WORKDIR /root/
RUN git clone https://github.com/ufo5260987423/chez-exe.git

WORKDIR /root/chez-exe/

RUN /usr/bin/scheme --script gen-config.ss --bootpath /usr/lib/csv10.4.1/ta6le --kernel libkernel.a
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

COPY scheme-langserver.sls test.sh run.ss build.sh /root/scheme-langserver/

RUN akku install



# Put it all together in Debian
FROM debian:bullseye

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y git make build-essential uuid-dev

# add chez scheme
COPY --from=build-chez /usr/bin/scheme /usr/bin/
COPY --from=build-chez /usr/lib/csv10.4.1/ /usr/lib/csv10.4.1/

# add compile-chez-program
COPY --from=build-chez /usr/local/bin/compile-chez-program /usr/local/bin/
COPY --from=build-chez /usr/local/lib/full-chez.a /usr/local/lib/
COPY --from=build-chez /usr/local/lib/petite-chez.a /usr/local/lib/

# add project
COPY --from=akku-install /root/scheme-langserver/ /root/scheme-langserver/

# Remove .so files compiled by akku's built-in Chez 9.5.2 to avoid
# fasl-object version mismatch with Chez 10.x. Chez 10.x will load
# .sls sources directly (slower but compatible).
RUN find /root/scheme-langserver/.akku/libobj -name "*.so" -delete || true

WORKDIR /root/scheme-langserver/

