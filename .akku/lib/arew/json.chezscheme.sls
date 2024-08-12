#!r6rs
(library (arew json)

  (export json-nesting-depth-limit
          json-null?
          json-error?
          json-error-reason
          json-read
          json-write)

  (import (except (chezscheme) define-record-type))
  (import (arew json shims))

  (include "json/body.scm"))
