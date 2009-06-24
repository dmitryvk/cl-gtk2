all: doc.html tutorial.html

doc.html: doc.xml
	xsltproc -o $@ /usr/share/sgml/docbook/xsl-stylesheets/html/docbook.xsl $<

tutorial.html: tutorial.xml
	xsltproc -o $@ /usr/share/sgml/docbook/xsl-stylesheets/html/docbook.xsl $<

index.html: doc.xml
	xsltproc /usr/share/sgml/docbook/xsl-stylesheets/html/chunk.xsl $<