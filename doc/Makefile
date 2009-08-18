all: doc.html tutorial.html gobject/index.html gtk/index.html

doc.html: doc.xml
	xsltproc -o $@ /usr/share/sgml/docbook/xsl-stylesheets/html/docbook.xsl $<

tutorial.html: tutorial.xml
	xsltproc -o $@ /usr/share/sgml/docbook/xsl-stylesheets/html/docbook.xsl $<

index.html: doc.xml
	xsltproc /usr/share/sgml/docbook/xsl-stylesheets/html/chunk.xsl $<

gobject/style.css: style.css
	([ -x gobject ] || mkdir gobject) && cp $< $@

gobject/index.html: gobject.texi gobject/style.css
	makeinfo --html --css-ref=style.css $<
	sbcl --noinform --no-sysinit --no-userinit --disable-debugger --load colorize-lisp-examples.lisp $@

gtk/style.css: style.css
	([ -x gtk ] || mkdir gtk) && cp $< $@

gtk/index.html: gtk.texi gtk.ref.texi gdk.ref.texi gobject.ref.texi gtk/style.css
	makeinfo --html --css-ref=style.css $<
	#sbcl --noinform --no-sysinit --no-userinit --disable-debugger --load colorize-lisp-examples.lisp $@

#gtk.ref.texi gdk.ref.texi gobject.ref.texi: introspection.lisp
#	sbcl --noinfo --no-sysinit --no-user-init --disable-debugger --eval "(asdf:oos 'asdf:load-op :cl-gtk2-gtk)" --load "introspection.lisp" --eval '(gtk-doc-introspection:generate-texinfo-for-packages *default-pathname-defaults* (list :gdk :gobject :gtk))'

gobject.pdf: gobject.texi
	pdftex $<
	pdftex $<

gtk.pdf: gtk.texi
	pdftex $<
	pdftex $<