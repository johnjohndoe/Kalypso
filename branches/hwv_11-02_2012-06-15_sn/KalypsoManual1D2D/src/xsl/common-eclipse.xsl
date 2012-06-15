<?xml version='1.0' encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
	<xsl:import href="eclipse-ex.xsl"/>

	<xsl:param name="suppress.navigation" select="0"/>
	<xsl:param name="html.stylesheet" select="'xhstyle.css'"/>
	<!--xsl:param name="generate.toc" select="0"/-->
	
	<xsl:param name="generate.toc">
	appendix  toc,title
	article/appendix  nop
	article   toc,title
	book      toc,title,figure,table,example,equation
	chapter   toc,title
	part      toc,title
	preface   toc,title
	qandadiv  toc
	qandaset  toc
	reference toc,title
	sect1     toc
	sect2     toc
	sect3     toc
	sect4     toc
	sect5     toc
	section   toc
	set       toc,title
        </xsl:param>
	
	<xsl:param name="generate.index" select="1"/>
	<xsl:param name="chunk.section.depth" select="2"/>
	<xsl:param name="chunk.first.sections" select="1"/>
	<xsl:param name="chunk.quietly" select="1"/>
    <xsl:variable name="ignore.image.scaling">0</xsl:variable>
	<xsl:variable name="scalefit">1</xsl:variable>

</xsl:stylesheet>
