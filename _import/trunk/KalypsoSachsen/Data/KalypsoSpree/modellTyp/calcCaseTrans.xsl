<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet version="1.0"  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:gml="http://www.opengis.net/gml">

<!-- standardmässig alles einfach kopieren -->
<xsl:template match="*">
	<xsl:copy><xsl:apply-templates/></xsl:copy>
</xsl:template>

<!-- auch die Kommentare mitkopieren -->
<xsl:template match="comment()">
	<xsl:copy></xsl:copy>
</xsl:template>

<!-- Überall wo ein link auf eine Zeitreihe ist, hier ein neues Element erzeugen, welches eine lokale Kopie darauf enthält -->
<xsl:template match="Einzugsgebiet/Wasserstand">
	<xsl:copy-of select="."/>

<wasserstand-local/>
</xsl:template>

</xsl:stylesheet>