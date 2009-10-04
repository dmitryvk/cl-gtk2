all: doc.html tutorial.html gobject/index.html gtk/index.html gobject/style.css gtk/style.css gtk/let-ui.png gtk/let-ui-glext.png

.PHONY: all archive publish

publish: archive
	scp manual.tar.bz2 kdmitry@common-lisp.net:/project/cl-gtk2/manual.tar.bz2
	ssh kdmitry@common-lisp.net 'cd /project/cl-gtk2/public_html/doc/ && rm -rf gobject gtk && tar xf /project/cl-gtk2/manual.tar.bz2'

archive: manual.tar.bz2

manual.tar.bz2: gtk/index.html gtk/style.css gobject/index.html gobject/style.css
	tar cjf $@ gtk gobject

doc.html: doc.xml
	xsltproc -o $@ /usr/share/sgml/docbook/xsl-stylesheets/html/docbook.xsl $<

tutorial.html: tutorial.xml
	xsltproc -o $@ /usr/share/sgml/docbook/xsl-stylesheets/html/docbook.xsl $<

index.html: doc.xml
	xsltproc /usr/share/sgml/docbook/xsl-stylesheets/html/chunk.xsl $<

gobject/style.css: style.css
	([ -x gobject ] || mkdir gobject) && cp $< $@

gobject/index.html: gobject.texi gobject.ref.texi
	makeinfo --html --css-ref=style.css $<
	sbcl --noinform --no-sysinit --no-userinit --disable-debugger --load colorize-lisp-examples.lisp $@
	./fix-tex-references.sh gobject

gtk/style.css: style.css
	([ -x gtk ] || mkdir gtk) && cp $< $@

gtk/let-ui.png: let-ui.png
	cp $< $@

gtk/let-ui-glext.png: let-ui-glext.png
	cp $< $@

gtk/index.html: gtk.texi gtk.ref.texi gdk.ref.texi gobject.ref.texi glib.ref.texi gdk.enums.texi \
gtk.flags.texi gtk.objects.texi gdk.flags.texi gdk.structs.texi gtk.interfaces.texi gtk.widgets.texi gdk.objects.texi \
 gtk.enums.texi gtk.main_loop.texi gtk.structs.texi
	makeinfo --html --css-ref=style.css $<
	#sbcl --noinform --no-sysinit --no-userinit --disable-debugger --load colorize-lisp-examples.lisp $@
	./fix-tex-references.sh gtk

#gtk.ref.texi gdk.ref.texi gobject.ref.texi: introspection.lisp
#	sbcl --noinfo --no-sysinit --no-user-init --disable-debugger --eval "(asdf:oos 'asdf:load-op :cl-gtk2-gtk)" --load "introspection.lisp" --eval '(gtk-doc-introspection:generate-texinfo-for-packages *default-pathname-defaults* (list :gdk :gobject :gtk))'

gobject.pdf: gobject.texi
	pdftex $<
	pdftex $<

gtk.pdf: gtk.texi
	pdftex $<
	pdftex $<
