<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet id="grafik-vorlage" version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	
	<xsl:output method="text" media-type="text/plain"/>

	<xsl:strip-space elements="*" />

	<xsl:template match="/" xmlns:xlink="http://www.w3.org/1999/xlink">
		<xsl:text>/* Grafik-Vorlage</xsl:text>
		<xsl:text>&#10;</xsl:text>
		<xsl:for-each select="//*[name()='curve']">
			<xsl:value-of select="position()"/>
			<xsl:text>- "</xsl:text>
			
			<!-- if href type is relative, make it absolute -->
			<xsl:if test="substring-before( substring-after( @xlink:href, 'TYPE='), '#LOCATION=') = 'relative'">
			    <xsl:text>_XXXX_/</xsl:text>
			</xsl:if>
			
			<xsl:value-of select="substring-after(@xlink:href, 'LOCATION=')"/>
			<xsl:text>" J L 1 </xsl:text>
			<xsl:value-of select="@name"/>
			<xsl:text>&#10;</xsl:text>
		</xsl:for-each>
	</xsl:template>
	
</xsl:stylesheet>
