#!/bin/sh
sbcl --eval "(asdf:oos 'asdf:load-op :cl-gtk2-gtk)" --eval "(asdf:oos 'asdf:load-op :fiveam)" --load gobject.type-tests.lisp --eval '(sb-ext:save-lisp-and-die "/tmp/sbcl-type-tests-core" :executable t)'
/tmp/sbcl-type-tests-core --eval "(gtype-tests:run-all-tests)" --eval "(quit)"
rm /tmp/sbcl-type-tests-core