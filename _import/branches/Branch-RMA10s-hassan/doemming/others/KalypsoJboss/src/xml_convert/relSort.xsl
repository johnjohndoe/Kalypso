<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="xml"/>
  <xsl:template match="/">
    <xsl:element name="theme">
      <xsl:attribute name="key"><xsl:value-of select="@key"/></xsl:attribute>
      <!-- filter all object-tables -->
      <xsl:apply-templates select="/theme/table[not(boolean(o/rel))]" mode="table"/>
      <!-- filter all relation-tables -->
      <xsl:apply-templates select="/theme/table[boolean(o/rel)]" mode="table"/>
    </xsl:element>
  </xsl:template>
  
  <xsl:template mode="table" match="*">
    <xsl:copy-of select="."/>
  </xsl:template>
</xsl:stylesheet>
