<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:decimal-format name="kalypso" decimal-separator="." grouping-separator=";"/>
  <xsl:output method="text"/>
  
  <xsl:template match="/">
    <xsl:text>Hydrotope NAM TISZA</xsl:text>
    <xsl:for-each select="//table[@key='rb']">

      <xsl:variable name="num" select="o/sp/@m_rbNumber"/>
      
      <xsl:variable name="areaTG" select="sum(/theme/table[@key='hydro']/o/sp[@m_rbNumber=$num]/@m_areaHy)"/>
      <xsl:variable name="areaSealed">
        <xsl:apply-templates mode="sumArea" select="/theme/table[@key='hydro'][1]/o[1]">
          <xsl:with-param name="sum" select="0"/>
          <xsl:with-param name="num" select="$num"/>
        </xsl:apply-templates>
      </xsl:variable>
      <xsl:variable name="areaNat" select="$areaTG - $areaSealed"/>

      <xsl:variable name="anz" select="count(/theme/table[@key='hydro']/o/sp[@m_rbNumber=$num])"/>

      <xsl:text>&#xA;</xsl:text>
      <xsl:call-template name="format">
        <xsl:with-param name="value" select="$num"/>
        <xsl:with-param name="digits" select="10"/>
        <xsl:with-param name="format" select="'0'"/>
      </xsl:call-template>
      <xsl:text> </xsl:text>
      <xsl:call-template name="format">
        <xsl:with-param name="value" select="$anz"/>
        <xsl:with-param name="digits" select="10"/>
        <xsl:with-param name="format" select="'0'"/>
      </xsl:call-template>
      <xsl:text> </xsl:text>
      <xsl:call-template name="format">
        <xsl:with-param name="value" select="$areaSealed"/>
        <xsl:with-param name="digits" select="15"/>
        <xsl:with-param name="format" select="'0'"/>
      </xsl:call-template>
      <xsl:text> </xsl:text>
      <xsl:call-template name="format">
        <xsl:with-param name="value" select="$areaNat"/>
        <xsl:with-param name="digits" select="15"/>
        <xsl:with-param name="format" select="'0'"/>
      </xsl:call-template>
      <xsl:text> </xsl:text>
      <xsl:call-template name="format">
        <xsl:with-param name="value" select="$areaTG"/>
        <xsl:with-param name="digits" select="15"/>
        <xsl:with-param name="format" select="'0'"/>
      </xsl:call-template>

      <xsl:for-each select="//table[@key='hydro']/o/sp[@m_rbNumber=$num]">
        <xsl:variable name="areaHyd" select="./@m_areaHy"/>
        <xsl:variable name="nutz" select="./@m_landuseALB"/>
        <xsl:variable name="boden" select="./@m_soiltype"/>
        <xsl:variable name="perkMax" select="./@m_maxPerk"/>
        <xsl:variable name="zuflussGW" select="./@m_aquiferRate"/>
        <xsl:variable name="zuflussTGW" select="1-$zuflussGW"/>
        <xsl:variable name="hydroNumber" select="position()"/>
        <xsl:variable name="versGrad" select="./@m_sealFactor * ./@m_sealFactorCor"/>
        
        <xsl:text>&#xA;</xsl:text>
        <xsl:call-template name="format">
          <xsl:with-param name="value" select="$areaHyd"/>
          <xsl:with-param name="digits" select="10"/>
          <xsl:with-param name="format" select="'0'"/>
        </xsl:call-template>
        <xsl:text> </xsl:text>
        
        <xsl:call-template name="text_format">
          <xsl:with-param name="value" select="$nutz"/>
          <xsl:with-param name="digits" select="10"/>
        </xsl:call-template>
        <xsl:text> </xsl:text>
        
        <xsl:call-template name="text_format">
          <xsl:with-param name="value" select="$boden"/>
          <xsl:with-param name="digits" select="10"/>
        </xsl:call-template>
        <xsl:text> </xsl:text>
        
           <xsl:call-template name="text_format">
             <xsl:with-param name="value" select="$perkMax"/>
             <xsl:with-param name="digits" select="12"/>
             
           </xsl:call-template>
           <xsl:text> </xsl:text>

           <xsl:call-template name="format">
             <xsl:with-param name="value" select="$zuflussGW"/>
             <xsl:with-param name="digits" select="5"/>
             <xsl:with-param name="format" select="'0.000'"/>
           </xsl:call-template>
           <xsl:text> </xsl:text>
           
           <xsl:call-template name="format">
             <xsl:with-param name="value" select="$zuflussTGW"/>
             <xsl:with-param name="digits" select="5"/>
             <xsl:with-param name="format" select="'0.000'"/>
           </xsl:call-template>
           <xsl:text> 10.000 0.27 0.000</xsl:text>
           <!-- werden nicht verwendet:
                a) 10.000 :Hoehe des grundwasserspeichers ueber der Bodenoberflaeche
                b) 0.27   :Porositaet des Grundwasserspeichers
                c) 0.000  :dummy
                -->
           <xsl:call-template name="format">
             <xsl:with-param name="value" select="$hydroNumber"/>
             <xsl:with-param name="digits" select="15"/>
             <xsl:with-param name="format" select="'0'"/>
           </xsl:call-template>
           <xsl:text> </xsl:text>
           <xsl:call-template name="format">
             <xsl:with-param name="value" select="$versGrad"/>
             <xsl:with-param name="digits" select="5"/>
             <xsl:with-param name="format" select="'0.000'"/>
           </xsl:call-template>
           <xsl:text> </xsl:text>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>

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
        <xsl:message>
          <xsl:text>
            Fehler in Formatierung!
          </xsl:text>
        </xsl:message>
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
