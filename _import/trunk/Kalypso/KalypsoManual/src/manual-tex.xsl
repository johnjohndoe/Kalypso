<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">


	<xsl:import href="../latex-docbook-08pre.xsl"/>


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

	
	<xsl:variable name="latex.hyperref.preamble">
	</xsl:variable>
	
	<xsl:param name="latex.admonition.path">./icons</xsl:param>

	<xsl:variable name="latex.use.fancyvrb">1</xsl:variable>
	<xsl:variable name="latex.use.fancybox">1</xsl:variable>
	<xsl:variable name="latex.use.fancyhdr">1</xsl:variable>
	<xsl:variable name="latex.use.subfigure">1</xsl:variable>
	<xsl:variable name="latex.use.rotating">1</xsl:variable>
	<xsl:variable name="latex.pdf.support">1</xsl:variable>
	<xsl:variable name="latex.math.support">1</xsl:variable>
	<xsl:variable name="latex.document.font">helvet</xsl:variable>
	<xsl:variable name="latex.figure.position">[hbt]</xsl:variable>
	<!--xsl:template match="figure">
	<xsl:variable name="placement">
				<xsl:call-template name="generate.formal.title.placement">
					<xsl:with-param name="object" select="local-name(.)"/>
				</xsl:call-template>
			</xsl:variable>
	<xsl:variable name="position">
				<xsl:call-template name="generate.latex.float.position">
					<xsl:with-param name="default" select="'hbt'"/>
				</xsl:call-template>
			</xsl:variable>
	</xsl:template-->

	<xsl:variable name="latex.book.preamblestart">
		\documentclass[a4paper, twoside, 10pt]{book}
		\usepackage[latin1]{inputenc}
	</xsl:variable>

</xsl:stylesheet>
