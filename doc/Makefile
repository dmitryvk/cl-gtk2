all: doc.html tutorial.html gobject/index.html gobject.pdf gtk/index.html gtk.pdf

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

gtk/index.html: gtk.texi gtk/style.css
	makeinfo --html --css-ref=style.css $<
	sbcl --noinform --no-sysinit --no-userinit --disable-debugger --load colorize-lisp-examples.lisp $@

gobject.pdf: gobject.texi
	pdftex $<
	pdftex $<

gtk.pdf: gtk.texi
	pdftex $<
	pdftex $<