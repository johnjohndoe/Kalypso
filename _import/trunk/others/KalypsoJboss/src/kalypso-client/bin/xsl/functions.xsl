<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:decimal-format name="kalypso" decimal-separator="." grouping-separator=";"/>
  <xsl:output method="text"/>
  
  <xsl:template name="format">
    <xsl:param name="value"/>
    <xsl:param name="digits"/>
    <xsl:param name="format"/>
    <xsl:variable name="out" select="format-number($value,$format)"/>
    <xsl:variable name="missingdigits" select="number($digits)-(string-length($out))"/>
    <xsl:choose>
      <xsl:when test="$missingdigits &gt;= 0">
        <xsl:value-of select="substring('                       ',1,number($missingdigits))"/>
        <xsl:value-of select="$out"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$out"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="text_format">
    <xsl:param name="value"/>
    <xsl:param name="digits"/>
    <xsl:variable name="missingdigits" select="number($digits)-(string-length($value))"/>
    <xsl:choose>
      <xsl:when test="$missingdigits &gt;= 0">
        <xsl:value-of select="substring('                       ',1,number($missingdigits))"/>
        <xsl:value-of select="$value"/>
      </xsl:when>
      <xsl:when test="$missingdigits='0'">
        <xsl:value-of select="$value"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:message>
          <xsl:text>
            Fehler in Formatierung!
          </xsl:text>
        </xsl:message>
        <xsl:value-of select="$value"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template mode="sumArea" match="*">
    <xsl:param name="sum"/>
    <xsl:param name="num"/>
    <xsl:variable name="val">
      <xsl:choose>
        <xsl:when test="sp[@m_rbNumber=$num]">
          <xsl:value-of select="sp/@m_areaHy * sp/@m_sealFactor * sp/@m_sealFactorCor"/>
        </xsl:when>
        <xsl:otherwise>0</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <!--
         needed Rb: <xsl:value-of select="$num"/>
         local Rb:<xsl:value-of select="sp/@m_rbNumber"/>
         sealedArea:<xsl:value-of select="$val"/>
         sum: <xsl:value-of select="$sum"/>
         -->
    <xsl:choose>
      <xsl:when test="following::table[@key='hydro'][1]/o[1]">
        <xsl:apply-templates mode="sumArea" select="following::table[@key='hydro'][1]/o[1]">
          <xsl:with-param name="sum" select="$sum + $val"/>
          <xsl:with-param name="num" select="$num"/>
        </xsl:apply-templates>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$sum + $val"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
</xsl:stylesheet>
