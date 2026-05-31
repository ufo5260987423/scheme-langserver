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
