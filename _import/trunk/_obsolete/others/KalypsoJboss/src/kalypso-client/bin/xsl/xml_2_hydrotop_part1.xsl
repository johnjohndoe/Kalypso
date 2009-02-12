<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0">
  <xsl:output method="xml"/>
  <xsl:template match="/">
    <theme>
      <xsl:apply-templates/>
    </theme>
  </xsl:template>

  <xsl:template match="table[@key='hydro']/o">
    <xsl:element name="table">
      <xsl:attribute name="key">hydro</xsl:attribute>
      <xsl:element name="o">
        <xsl:attribute name="ID"><xsl:value-of select="@ID"/></xsl:attribute>
        <xsl:copy-of select="*"/>         
        <xsl:element name="cal_sealed">
          <xsl:value-of select="sp/@m_areaHy * sp/@m_sealFactor * sp/@m_sealFactorCor"/>
        </xsl:element>
      </xsl:element>
    </xsl:element>              
  </xsl:template>
  <xsl:template match="table[@key!='hydro']">
    <xsl:copy-of select=".">
    </xsl:copy-of>  
  </xsl:template>
</xsl:stylesheet>
