<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="xml"/>

  <xsl:template match="/">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="/theme">
    <xsl:copy>
      <xsl:call-template name="tableSort">
        <xsl:with-param name="tableKey">node</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="tableSort">
        <xsl:with-param name="tableKey">channel</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="tableSort">
        <xsl:with-param name="tableKey">nullStrand</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="tableSort">
        <xsl:with-param name="tableKey">rb</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="tableSort">
        <xsl:with-param name="tableKey">rhb</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="tableSort">
        <xsl:with-param name="tableKey">rht</xsl:with-param>
      </xsl:call-template>


      <xsl:call-template name="tableSort">
        <xsl:with-param name="tableKey">wc</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="tableSort">
        <xsl:with-param name="tableKey">wc2rb</xsl:with-param>
      </xsl:call-template>  
      <xsl:call-template name="tableSort">
        <xsl:with-param name="tableKey">wc2objects</xsl:with-param>
      </xsl:call-template> 
      <xsl:call-template name="tableSort">
        <xsl:with-param name="tableKey">wc2node</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="tableSort">
        <xsl:with-param name="tableKey">node2strand</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="tableSort">
        <xsl:with-param name="tableKey">strand2node</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="tableSort">
        <xsl:with-param name="tableKey">rb2strand</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="tableSort">
        <xsl:with-param name="tableKey">rb2rb</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="tableSort">
        <xsl:with-param name="tableKey">node2nodeOverflow</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="tableSort">
        <xsl:with-param name="tableKey">node2nodeFunctOut</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="tableSort">
        <xsl:with-param name="tableKey">node2nodeConstOut</xsl:with-param>
      </xsl:call-template>
    </xsl:copy>
  </xsl:template>

  <xsl:template name="tableSort">
    <xsl:param name="tableKey"/>
    <xsl:element name="table">
      <xsl:attribute name="key">
        <xsl:value-of select="$tableKey"/>
      </xsl:attribute>
      <xsl:for-each select="table[@key=$tableKey]/o">
        <xsl:copy-of select="."/>
      </xsl:for-each>
    </xsl:element>
  </xsl:template>
    


  <!--
  <xsl:template match="table[@key='node']/o">
    <table key="node">
      <xsl:copy-of select="."/>
    </table>
  </xsl:template>
-->
</xsl:stylesheet>
