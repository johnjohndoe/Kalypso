<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:gml="http://www.opengis.net/gml">
  <xsl:import href="xsl/functions.xsl"/>

  <xsl:decimal-format name="kalypso" decimal-separator="." grouping-separator=";"/>

  <xsl:output method="xml"/>
  

  <xsl:template match="/">
    <theme>
      <xsl:for-each select="//TEILGEBNR[not(.=../preceding::TEILGEBNR)]">
        <xsl:sort data-type="number" />
        <xsl:variable name="num" select="."/>

        <xsl:variable name="typanz" select="count(//TEILGEBNR[.=$num])"/>
        <xsl:variable name="areaTG" select="sum(  //TEILGEBNR[.=$num]/../FLAECHE)"/>
        <xsl:variable name="pos" select="..//gml:coordinates"/>
        <xsl:variable name="cs" select="..//gml:coordinates/@cs"/>
        <xsl:variable name="ts" select="..//gml:coordinates/@ts"/>
        <xsl:variable name="xyPos" select="substring-before($pos,$ts)"/>
        <xsl:variable name="xPos" select="substring-before($xyPos,$cs)"/>
        <xsl:variable name="yPos" select="substring-after($xyPos,$cs)"/>

        <!--
             POSITION: <xsl:value-of select="$pos"/>
             CS:<xsl:value-of select="$cs"/>
             TS:<xsl:value-of select="$ts"/>
             XYPOS:<xsl:value-of select="$xyPos"/>
             XPOS:<xsl:value-of select="$xPos"/>
             YPOS:<xsl:value-of select="$yPos"/>
             -->
        
        <table key="rb">
          <xsl:element name="o">
            <xsl:attribute name="ID">
              <xsl:value-of select="$num"/>
            </xsl:attribute>
            <xsl:element name="bp">
              <xsl:attribute name="x">
                <xsl:value-of select="$xPos"/>
              </xsl:attribute>
              <xsl:attribute name="y">
                <xsl:value-of select="$yPos"/>
              </xsl:attribute>
            </xsl:element>
            <xsl:element name="sp">
              <xsl:attribute name="m_rbNumber">
                <xsl:call-template name="format">
                  <xsl:with-param name="value" select="$num"/>
                  <xsl:with-param name="digits" select="1"/>
                  <xsl:with-param name="format" select="'0'"/>
                </xsl:call-template>
              </xsl:attribute>
            </xsl:element>
          </xsl:element>
        </table>
        
        <xsl:for-each select="//TEILGEBNR[.=$num]/..">
          <xsl:sort select="FLAECHE" order="descending" data-type="number"/>
          <xsl:variable name="areaHYDRO" select="FLAECHE"/>
          <xsl:variable name="tmp" select="translate(NUTZUNGALB,'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ')"/>
          <xsl:variable name="nutz">
            <xsl:choose>
              <xsl:when test="starts-with($tmp,'ALB_')">
                <xsl:value-of select="$tmp"/>                      
              </xsl:when>
              <xsl:otherwise>
                <xsl:text>ALB_</xsl:text>
                <xsl:call-template name="format">
                  <xsl:with-param name="value" select="number($tmp)"/>
                  <xsl:with-param name="digits" select="1"/>
                  <xsl:with-param name="format" select="'0'"/>
                </xsl:call-template>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:variable>
          

          <xsl:variable name="boden" select="translate(BODENTYP,' ','_')"/>
          <xsl:variable name="maxPERKOL" select="MAXPERRATE"/>
          <xsl:variable name="anteilGW" select="GWFAKTOR"/>

          
          <!--          <xsl:variable name="versigGrad" select="VERSIEGGR"/> -->

          <xsl:variable name="versigGrad">
            <xsl:call-template name="get_seal_factor">
              <xsl:with-param name="versigGrad" select="VERSIEGGR"/>
              <xsl:with-param name="alb_key" select="$nutz"/>
            </xsl:call-template>
          </xsl:variable>
          
          <xsl:variable name="Gebietskenn" select="GEBKZAHL"/>
          <xsl:variable name="pos" select="..//gml:coordinates"/>
          <xsl:variable name="cs" select="..//gml:coordinates/@cs"/>
          <xsl:variable name="ts" select="..//gml:coordinates/@ts"/>
          <xsl:variable name="xyPos" select="substring-before($pos,$ts)"/>
          <xsl:variable name="xPos" select="substring-before($xyPos,$cs)"/>
          <xsl:variable name="yPos" select="substring-after($xyPos,$cs)"/>
          <xsl:variable name="versiegkor">
            <xsl:choose>
              <xsl:when test="VERSIEGKOR">
                <xsl:value-of select="VERSIEGKOR"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="1.0"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:variable>

          
          <table key="hydro">
            <xsl:element name="o">
              <xsl:attribute name="ID">
                <xsl:value-of select="generate-id()"/>
              </xsl:attribute>
              <xsl:element name="bp">
                <xsl:attribute name="x">
                  <xsl:value-of select="$xPos"/>
                </xsl:attribute>
                <xsl:attribute name="y">
                  <xsl:value-of select="$yPos"/>
                </xsl:attribute>
              </xsl:element>
              <xsl:element name="sp">
                <!-- 
                     <xsl:attribute name="m_hydroNumber">
                       <xsl:value-of select="$Gebietskenn"/>
                     </xsl:attribute>
                     -->
                     <xsl:attribute name="m_rbNumber">
                       <xsl:call-template name="format">
                         <xsl:with-param name="value" select="$num"/>
                         <xsl:with-param name="digits" select="1"/>
                         <xsl:with-param name="format" select="'0'"/>
                       </xsl:call-template>
                     </xsl:attribute>

                <xsl:attribute name="m_areaHy">
                       <xsl:call-template name="format">
                         <xsl:with-param name="value" select="$areaHYDRO"/>
                         <xsl:with-param name="digits" select="1"/>
                         <xsl:with-param name="format" select="'0'"/>
                       </xsl:call-template>
                     </xsl:attribute>
                     <xsl:attribute name="m_sealFactor">
                       <xsl:value-of select="$versigGrad"/>
                     </xsl:attribute>
                <xsl:attribute name="m_sealFactorCor">
                  <xsl:value-of select="$versiegkor"/>
                </xsl:attribute>

                <xsl:attribute name="m_landuseALB">
                  <xsl:value-of select="$nutz"/>
                </xsl:attribute>

                <xsl:attribute name="m_soiltype">
                  <xsl:value-of select="$boden"/>
                </xsl:attribute>

                <xsl:attribute name="m_maxPerk">
                  <xsl:value-of select="$maxPERKOL"/>
                </xsl:attribute>
                <xsl:attribute name="m_aquiferRate">
                  <xsl:value-of select="$anteilGW"/>
                </xsl:attribute>
                <!--
                     <xsl:attribute name="m_lowAquiferRate">
                       <xsl:value-of select="1-$anteilGW"/>
                     </xsl:attribute>
                     -->
              </xsl:element>
            </xsl:element>
          </table>
        </xsl:for-each>
      </xsl:for-each>
    </theme>
  </xsl:template>
</xsl:stylesheet>
