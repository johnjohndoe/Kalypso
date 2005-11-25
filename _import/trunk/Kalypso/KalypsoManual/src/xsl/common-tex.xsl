<?xml version='1.0' encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

	<xsl:import href="latex-docbook.xsl"/>

	<xsl:output method="text" encoding="ISO-8859-1" indent="yes"/>

	<xsl:variable name="latex.babel.language">german</xsl:variable>
	<xsl:variable name="latex.use.babel">1</xsl:variable>

	<xsl:variable name="latex.use.isolatin1">0</xsl:variable>

	<xsl:variable name="latex.hyperref.param.pdftex">colorlinks, linkcolor=blue, urlcolor=blue, bookmarks=true,bookmarksnumbered=true</xsl:variable>
	<xsl:variable name="latex.use.hyperref">1</xsl:variable>

	<xsl:variable name="latex.book.begindocument">
		<xsl:text>\usepackage{hypcap}&#10;</xsl:text>
		<xsl:text>\begin{document}&#10;</xsl:text>
	</xsl:variable>

	<xsl:param name="admon.graphics.path">./img/admon</xsl:param>

	<xsl:variable name="latex.hyperref.preamble"/>
	<xsl:variable name="latex.use.fancyvrb">1</xsl:variable>
	<xsl:variable name="latex.use.fancybox">1</xsl:variable>
	<xsl:variable name="latex.use.fancyhdr">1</xsl:variable>
	<xsl:variable name="latex.use.subfigure">1</xsl:variable>
	<xsl:variable name="latex.use.rotating">1</xsl:variable>
	<xsl:variable name="latex.pdf.support">1</xsl:variable>
	<xsl:variable name="latex.math.support">1</xsl:variable>

	<xsl:param name="latex.hyphenation.tttricks">1</xsl:param>

	<!-- Mögliche fonts:
		helvetic
		palatino
		charter		funktioniert nicht
		avant
		courier
		lucida		funktioniert nicht
		courier
		bookman		sehr hässlich
		palatcm		funktioniert nicht
		newcent
	-->
	<xsl:variable name="latex.document.font">default</xsl:variable>
	<xsl:param name="latex.fontenc">T1</xsl:param>

	<xsl:variable name="latex.figure.position">[hbt]</xsl:variable>

	<xsl:variable name="latex.book.preamblestart">
		\hyphenation{Daten-manage-ment-Prinzip}
		\hyphenation{ver-ge-ben}
		\hyphenation{Block-dia-gramm}
		
		\documentclass[a4paper, twoside, 12pt, draft]{book}
		
		\usepackage[latin1]{inputenc}
		
		\hyphenation{Daten-manage-ment-Prinzip}
		\hyphenation{ver-ge-ben}
		\hyphenation{Block-dia-gramm}
	</xsl:variable>

	<!-- Keine Striche über und unter Tabellen und Figuren -->
	<xsl:template name="latex.float.preamble"/>

</xsl:stylesheet>
