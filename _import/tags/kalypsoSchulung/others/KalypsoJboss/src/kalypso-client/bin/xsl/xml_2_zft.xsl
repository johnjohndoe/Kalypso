<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0">
  <xsl:import href="xsl/functions.xsl"/>
  <xsl:output method="text"/>
  <xsl:template match="/">
    <xsl:for-each select="//TEILGEBNR[not(.=../preceding::TEILGEBNR)]">
      <xsl:sort data-type="number" />
      <xsl:call-template name="zft">
        <xsl:with-param name="num" select="."/>
      </xsl:call-template>
    </xsl:for-each>

    <xsl:for-each select="//table[@key='rb']/o/sp/@m_rbNumber">
      <xsl:sort data-type="number" />
      <xsl:call-template name="zft">
        <xsl:with-param name="num" select="."/>
      </xsl:call-template>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="zft">
    <xsl:param name="num"/>
    <xsl:call-template name="format">
      <xsl:with-param name="value" select="$num"/>
      <xsl:with-param name="digits" select="8"/>
      <xsl:with-param name="format" select="'0'"/>
    </xsl:call-template>
    <xsl:text>&#xA;   4  0.25       2&#xA;</xsl:text>
    <xsl:text>    0.2500    0.150000&#xA;</xsl:text>
    <xsl:text>    0.5000    0.400000&#xA;</xsl:text>
    <xsl:text>    0.7500    0.300000&#xA;</xsl:text>
    <xsl:text>    1.0000    0.150000&#xA;</xsl:text>
  </xsl:template>
</xsl:stylesheet>
  