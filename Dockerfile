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

# glibc static libc.a does not provide getlogin, but Chez 10.x full-chez.a
# references it. Provide a stub so static linking succeeds.
RUN echo 'char *getlogin(void) { return (char*)0; }' > getlogin_stub.c && \
    cc -c getlogin_stub.c -o /usr/local/lib/getlogin_stub.o && \
    rm getlogin_stub.c

# compile-chez-program ignores the (system ...) return value, so linker
# failures are silently swallowed. Patch it to exit non-zero on failure.
RUN cat > /tmp/patch.pl <<'PERL_EOF'
use strict;
use warnings;
open(my $fh, '<', 'compile-chez-program.ss') or die $!;
my $content = do { local $/; <$fh> };
close($fh);
my $old = '(build-included-binary-file embed-file "scheme_program" compiled-name)
(case (os-name)
  [windows
   (system (format "cl /nologo /MD /Fe:~a ~a ~a ~a ~a ~{ ~a~}" exe-name win-main solibs chez-file embed-file compiler-args))]
  [else
   (system (apply format
                  (string-append "cc -o ~a ~a"
                                 (if (use-libkernel) " ~a ~a ~a" "")
                                 " ~a ~a ~a ~{ ~s~}")
                  (append (list exe-name chez-file)
                          (if (use-libkernel) (list libkernel-file liblz4-file libz-file) \'())
                          (list embed-file mbits solibs compiler-args))))])

(display basename)
(newline)';
my $new = '(build-included-binary-file embed-file "scheme_program" compiled-name)
(let ([ret
       (case (os-name)
         [windows
          (system (format "cl /nologo /MD /Fe:~a ~a ~a ~a ~a ~{ ~a~}" exe-name win-main solibs chez-file embed-file compiler-args))]
         [else
          (system (apply format
                         (string-append "cc -o ~a ~a"
                                        (if (use-libkernel) " ~a ~a ~a" "")
                                        " ~a ~a ~a ~{ ~s~}")
                         (append (list exe-name chez-file)
                                 (if (use-libkernel) (list libkernel-file liblz4-file libz-file) \'())
                                 (list embed-file mbits solibs compiler-args))))])])
  (unless (zero? ret)
    (exit 1)))

(display basename)
(newline)';
if ($content =~ s/\Q$old\E/$new/s) {
    open($fh, '>', 'compile-chez-program.ss') or die $!;
    print $fh $content;
    close($fh);
} else {
    die "Pattern not found in compile-chez-program.ss\n";
}
PERL_EOF
perl /tmp/patch.pl

RUN /usr/bin/scheme --script gen-config.ss --bootpath /usr/lib/csv10.4.1/ta6le --kernel libkernel.a -Wl,--no-fatal-warnings /usr/local/lib/getlogin_stub.o
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

COPY bin /root/scheme-langserver/bin/
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
COPY --from=build-chez /usr/local/lib/getlogin_stub.o /usr/local/lib/

# add project
COPY --from=akku-install /root/scheme-langserver/ /root/scheme-langserver/

# Remove .so files compiled by akku's built-in Chez 9.5.2 to avoid
# fasl-object version mismatch with Chez 10.x. Chez 10.x will load
# .sls sources directly (slower but compatible).
RUN find /root/scheme-langserver/.akku/libobj -name "*.so" -delete || true

WORKDIR /root/scheme-langserver/

