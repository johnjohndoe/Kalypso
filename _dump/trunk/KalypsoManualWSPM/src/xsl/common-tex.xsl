<?xml version='1.0' encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	version="1.0">

	<xsl:import href="latex-docbook.xsl" />

	<xsl:output method="text" encoding="UTF-8" indent="yes" />

	<xsl:template name="latex.float.preamble" />

	<xsl:variable name="latex.inputenc">utf8</xsl:variable>
	<xsl:variable name="latex.document.font">times</xsl:variable>
	<xsl:variable name="latex.fontenc">T1</xsl:variable>
	<xsl:param name="unicode.mapping.languages">de</xsl:param>

	<xsl:variable name="latex.babel.language">germanb</xsl:variable>
	<xsl:variable name="latex.use.babel">1</xsl:variable>

	<xsl:variable name="latex.is.draft">0</xsl:variable>

	<xsl:variable name="latex.hyperref.param.pdftex">
		colorlinks, linkcolor=blue, urlcolor=blue,
		bookmarks=true,bookmarksnumbered=true
	</xsl:variable>
	<xsl:variable name="latex.use.hyperref">1</xsl:variable>

	<xsl:variable name="latex.book.begindocument">
		<xsl:text>\usepackage{hypcap}&#10;</xsl:text>
		<xsl:text>\usepackage{german}&#10;</xsl:text>
		<xsl:text>\begin{document}&#10;</xsl:text>
		<xsl:text>\hyphenation{Block-dia-gramm}&#10;</xsl:text>
		<xsl:text>\hyphenation{Da-ten-ma-nage-ment--Prinzip}&#10;</xsl:text>
		<xsl:text>\hyphenation{ver-ge-ben}&#10;</xsl:text>
		<xsl:text>\hyphenation{Ab-schnitt}&#10;</xsl:text>
		<xsl:text>\hyphenation{Vor-her-sa-ge--Assis-ten-ten}&#10;</xsl:text>
		<xsl:text>\hyphenation{Ta-bel-len-wer-ten}&#10;</xsl:text>
		<xsl:text>\hyphenation{In-for-ma-tions--Ma-nage-ment--Sys-tems}&#10;</xsl:text>
	</xsl:variable>

	<xsl:param name="admon.graphics.path">./img/admon</xsl:param>

	<xsl:variable name="latex.hyperref.preamble" />
	<xsl:variable name="latex.use.fancyvrb">1</xsl:variable>
	<xsl:variable name="latex.use.fancybox">1</xsl:variable>
	<xsl:variable name="latex.use.fancyhdr">1</xsl:variable>
	<xsl:variable name="latex.fancyhdr.style">natural</xsl:variable>
	<xsl:variable name="latex.use.subfigure">0</xsl:variable>
	<xsl:variable name="latex.use.rotating">0</xsl:variable>
	<xsl:variable name="latex.pdf.support">1</xsl:variable>
	<xsl:variable name="latex.math.support">1</xsl:variable>

	<xsl:param name="latex.url.quotation">0</xsl:param>
	<xsl:param name="latex.hyphenation.tttricks">1</xsl:param>
	<xsl:param name="latex.generate.indexterm">1</xsl:param>
	<xsl:param name="latex.biblio.output">all</xsl:param>
	<xsl:param name="latex.style">docbook</xsl:param>

	<xsl:param name="toc.section.depth">2</xsl:param>
	<xsl:param name="ignore.image.scaling" select="0"></xsl:param>

	<xsl:variable name="ignore.image.scaling">0</xsl:variable>
	<!-- 
	<xsl:param name="latex.figure.position">[htbp]</xsl:param>
	 -->
	<xsl:param name="latex.figure.boxed">0</xsl:param>
	<xsl:param name="latex.documentclass.pdftex"/>
	<!-- 
		<xsl:variable name="contentwidth" />
		<xsl:variable name="contentdepth" />
		<xsl:variable name="scalefit">1</xsl:variable>
	-->
	<xsl:variable name="latex.book.preamblestart">
		\documentclass[a4paper, oneside, 12pt]{book}
		\usepackage[pdftex]{graphicx}
	</xsl:variable>

</xsl:stylesheet>