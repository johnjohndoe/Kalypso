<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version='1.0'>

<xsl:import href="../../latex-docbook.xsl"/>

<xsl:output method="text" encoding="ISO-8859-1" indent="yes"/>

<xsl:variable name="latex.babel.language">german</xsl:variable>
<xsl:variable name="latex.use.babel">1</xsl:variable>
<xsl:variable name="latex.use.isolatin1">0</xsl:variable>

<xsl:variable name="latex.hyperref.param.pdftex">colorlinks, linkcolor=blue, urlcolor=blue, bookmarks=true,bookmarksnumbered=true</xsl:variable>
<xsl:variable name="latex.use.hyperref">1</xsl:variable>

<xsl:variable name="latex.use.fancyvrb">1</xsl:variable>
<xsl:variable name="latex.use.fancybox">1</xsl:variable>
<xsl:variable name="latex.use.fancyhdr">0</xsl:variable>
<xsl:variable name="latex.use.subfigure">1</xsl:variable>
<xsl:variable name="latex.use.rotating">1</xsl:variable>
<xsl:variable name="latex.pdf.support">1</xsl:variable>
<xsl:variable name="latex.math.support">1</xsl:variable>
<xsl:variable name="latex.document.font">default</xsl:variable>
<xsl:variable name="latex.figure.position">[htbp]</xsl:variable>

<!-- <xsl:variable name="latex.titlepage.file">./tex/titlepage.tex</xsl:variable> -->

<xsl:variable name="latex.book.preamblestart">
	\documentclass[a4paper, oneside, 12pt]{book}
	\usepackage[latin1]{inputenc}
</xsl:variable>


<xsl:param name="latex.admonition.path">./metadata/admonition</xsl:param>

</xsl:stylesheet>
