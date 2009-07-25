all: doc.html tutorial.html gobject/index.html gobject.pdf gobject/style.css

doc.html: doc.xml
	xsltproc -o $@ /usr/share/sgml/docbook/xsl-stylesheets/html/docbook.xsl $<

tutorial.html: tutorial.xml
	xsltproc -o $@ /usr/share/sgml/docbook/xsl-stylesheets/html/docbook.xsl $<

index.html: doc.xml
	xsltproc /usr/share/sgml/docbook/xsl-stylesheets/html/chunk.xsl $<

gobject/style.css: style.css
	cp $< $@

gobject/index.html: gobject.texi
	makeinfo --html --css-ref=style.css $<
	sbcl --no-sysinit --no-userinit --load colorize-lisp-examples.lisp $@

gobject.pdf: gobject.texi
	pdftex $<
	pdftex $<