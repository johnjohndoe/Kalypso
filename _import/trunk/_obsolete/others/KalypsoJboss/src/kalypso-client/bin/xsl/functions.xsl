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

  <xsl:template name="get_seal_factor">
    <xsl:param name="versigGrad" />
    <xsl:param name="alb_key"/>
    <xsl:value-of select="x"/>


    
    <xsl:choose>
      <xsl:when test="$versigGrad &gt; 0 and $versigGrad &lt; 1">
        <xsl:value-of select="$versigGrad"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="perlekamp">
          <xsl:choose>
            <xsl:when test="$alb_key='ALB_111'">7</xsl:when>
            <xsl:when test="$alb_key='ALB_112'">6</xsl:when>
            <xsl:when test="$alb_key='ALB_113'">6</xsl:when>
            <xsl:when test="$alb_key='ALB_114'">1</xsl:when>
            <xsl:when test="$alb_key='ALB_115'">7</xsl:when>
            <xsl:when test="$alb_key='ALB_116'">2</xsl:when>
            <xsl:when test="$alb_key='ALB_117'">7</xsl:when>
            <xsl:when test="$alb_key='ALB_139'">1</xsl:when>
            <xsl:when test="$alb_key='ALB_140'">9</xsl:when>
            <xsl:when test="$alb_key='ALB_141'">9</xsl:when>
            <xsl:when test="$alb_key='ALB_142'">9</xsl:when>
            <xsl:when test="$alb_key='ALB_143'">9</xsl:when>
            <xsl:when test="$alb_key='ALB_144'">9</xsl:when>
            <xsl:when test="$alb_key='ALB_145'">9</xsl:when>
            <xsl:when test="$alb_key='ALB_146'">9</xsl:when>
            <xsl:when test="$alb_key='ALB_147'">9</xsl:when>
            <xsl:when test="$alb_key='ALB_148'">9</xsl:when>
            <xsl:when test="$alb_key='ALB_149'">9</xsl:when>
            <xsl:when test="$alb_key='ALB_170'">8</xsl:when>
            <xsl:when test="$alb_key='ALB_171'">8</xsl:when>
            <xsl:when test="$alb_key='ALB_172'">5</xsl:when>
            <xsl:when test="$alb_key='ALB_172'">6</xsl:when>
            <xsl:when test="$alb_key='ALB_172'">8</xsl:when>
            <xsl:when test="$alb_key='ALB_173'">8</xsl:when>
            <xsl:when test="$alb_key='ALB_174'">8</xsl:when>
            <xsl:when test="$alb_key='ALB_175'">8</xsl:when>
            <xsl:when test="$alb_key='ALB_176'">8</xsl:when>
            <xsl:when test="$alb_key='ALB_179'">8</xsl:when>
            <xsl:when test="$alb_key='ALB_211'">4</xsl:when>
            <xsl:when test="$alb_key='ALB_212'">9</xsl:when>
            <xsl:when test="$alb_key='ALB_213'">9</xsl:when>
            <xsl:when test="$alb_key='ALB_216'">9</xsl:when>
            <xsl:when test="$alb_key='ALB_219'">5</xsl:when>
            <xsl:when test="$alb_key='ALB_231'">19</xsl:when>
            <xsl:when test="$alb_key='ALB_232'">11</xsl:when>
            <xsl:when test="$alb_key='ALB_236'">19</xsl:when>
            <xsl:when test="$alb_key='ALB_251'">17</xsl:when>
            <xsl:when test="$alb_key='ALB_252'">8</xsl:when>
            <xsl:when test="$alb_key='ALB_254'">8</xsl:when>
            <xsl:when test="$alb_key='ALB_259'">8</xsl:when>
            <xsl:when test="$alb_key='ALB_261'">8</xsl:when>
            <xsl:when test="$alb_key='ALB_262'">8</xsl:when>
            <xsl:when test="$alb_key='ALB_269'">8</xsl:when>
            <xsl:when test="$alb_key='ALB_272'">12</xsl:when>
            <xsl:when test="$alb_key='ALB_273'">12</xsl:when>
            <xsl:when test="$alb_key='ALB_274'">9</xsl:when>
            <xsl:when test="$alb_key='ALB_281'">10</xsl:when>
            <xsl:when test="$alb_key='ALB_282'">10</xsl:when>
            <xsl:when test="$alb_key='ALB_283'">10</xsl:when>
            <xsl:when test="$alb_key='ALB_284'">6</xsl:when>
            <xsl:when test="$alb_key='ALB_286'">12</xsl:when>
            <xsl:when test="$alb_key='ALB_287'">12</xsl:when>
            <xsl:when test="$alb_key='ALB_291'">9</xsl:when>
            <xsl:when test="$alb_key='ALB_299'">12</xsl:when>
            <xsl:when test="$alb_key='ALB_311'">14</xsl:when>
            <xsl:when test="$alb_key='ALB_321'">14</xsl:when>
            <xsl:when test="$alb_key='ALB_339'">8</xsl:when>
            <xsl:when test="$alb_key='ALB_341'">16</xsl:when>
            <xsl:when test="$alb_key='ALB_351'">13</xsl:when>
            <xsl:when test="$alb_key='ALB_361'">9</xsl:when>
            <xsl:when test="$alb_key='ALB_410'">10</xsl:when>
            <xsl:when test="$alb_key='ALB_411'">10</xsl:when>
            <xsl:when test="$alb_key='ALB_413'">10</xsl:when>
            <xsl:when test="$alb_key='ALB_416'">10</xsl:when>
            <xsl:when test="$alb_key='ALB_417'">10</xsl:when>
            <xsl:when test="$alb_key='ALB_420'">12</xsl:when>
            <xsl:when test="$alb_key='ALB_421'">9</xsl:when>
            <xsl:when test="$alb_key='ALB_422'">2</xsl:when>
            <xsl:when test="$alb_key='ALB_423'">12</xsl:when>
            <xsl:when test="$alb_key='ALB_424'">12</xsl:when>
            <xsl:when test="$alb_key='ALB_426'">13</xsl:when>
            <xsl:when test="$alb_key='ALB_427'">13</xsl:when>
            <xsl:when test="$alb_key='ALB_428'">12</xsl:when>
            <xsl:when test="$alb_key='ALB_429'">12</xsl:when>
            <xsl:when test="$alb_key='ALB_431'">12</xsl:when>
            <xsl:when test="$alb_key='ALB_500'">19</xsl:when>
            <xsl:when test="$alb_key='ALB_510'">19</xsl:when>
            <xsl:when test="$alb_key='ALB_511'">19</xsl:when>
            <xsl:when test="$alb_key='ALB_512'">19</xsl:when>
            <xsl:when test="$alb_key='ALB_513'">19</xsl:when>
            <xsl:when test="$alb_key='ALB_520'">21</xsl:when>
            <xsl:when test="$alb_key='ALB_521'">21</xsl:when>
            <xsl:when test="$alb_key='ALB_522'">21</xsl:when>
            <xsl:when test="$alb_key='ALB_531'">21</xsl:when>
            <xsl:when test="$alb_key='ALB_533'">21</xsl:when>
            <xsl:when test="$alb_key='ALB_534'">21</xsl:when>
            <xsl:when test="$alb_key='ALB_541'">11</xsl:when>
            <xsl:when test="$alb_key='ALB_548'">11</xsl:when>
            <xsl:when test="$alb_key='ALB_591'">12</xsl:when>
            <xsl:when test="$alb_key='ALB_594'">12</xsl:when>
            <xsl:when test="$alb_key='ALB_611'">14</xsl:when>
            <xsl:when test="$alb_key='ALB_620'">15</xsl:when>
            <xsl:when test="$alb_key='ALB_621'">15</xsl:when>
            <xsl:when test="$alb_key='ALB_631'">14</xsl:when>
            <xsl:when test="$alb_key='ALB_661'">15</xsl:when>
            <xsl:when test="$alb_key='ALB_681'">9</xsl:when>
            <xsl:when test="$alb_key='ALB_692'">15</xsl:when>
            <xsl:when test="$alb_key='ALB_710'">16</xsl:when>
            <xsl:when test="$alb_key='ALB_711'">16</xsl:when>
            <xsl:when test="$alb_key='ALB_731'">17</xsl:when>
            <xsl:when test="$alb_key='ALB_741'">16</xsl:when>
            <xsl:when test="$alb_key='ALB_841'">18</xsl:when>
            <xsl:when test="$alb_key='ALB_851'">18</xsl:when>
            <xsl:when test="$alb_key='ALB_861'">18</xsl:when>
            <xsl:when test="$alb_key='ALB_881'">18</xsl:when>
            <xsl:when test="$alb_key='ALB_922'">16</xsl:when>
            <xsl:when test="$alb_key='ALB_923'">15</xsl:when>
            <xsl:when test="$alb_key='ALB_933'">16</xsl:when>
            <xsl:when test="$alb_key='ALB_941'">12</xsl:when>
            <xsl:when test="$alb_key='ALB_999'">20</xsl:when>
            <xsl:otherwise>20</xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <xsl:choose>
          <xsl:when test="$perlekamp='1'">0.278</xsl:when>
          <xsl:when test="$perlekamp='2'">0.408</xsl:when>
          <xsl:when test="$perlekamp='3'">0.503</xsl:when>
          <xsl:when test="$perlekamp='4'">0.606</xsl:when>
          <xsl:when test="$perlekamp='5'">0.808</xsl:when>
          <xsl:when test="$perlekamp='6'">0.590</xsl:when>
          <xsl:when test="$perlekamp='7'">0.782</xsl:when>
          <xsl:when test="$perlekamp='8'">0.708</xsl:when>
          <xsl:when test="$perlekamp='9'">0.729</xsl:when>
          <xsl:when test="$perlekamp='10'">0.356</xsl:when>
          <xsl:when test="$perlekamp='11'">0.137</xsl:when>
          <xsl:when test="$perlekamp='12'">0.196</xsl:when>
          <xsl:when test="$perlekamp='13'">0.029</xsl:when>
          <xsl:when test="$perlekamp='14'">0.0</xsl:when>
          <xsl:when test="$perlekamp='15'">0.0</xsl:when>
          <xsl:when test="$perlekamp='16'">0.0</xsl:when>
          <xsl:when test="$perlekamp='17'">0.0</xsl:when>
          <xsl:when test="$perlekamp='18'">0.0</xsl:when>
          <xsl:when test="$perlekamp='19'">0.950</xsl:when>
          <xsl:when test="$perlekamp='20'">0.561</xsl:when>
          <xsl:when test="$perlekamp='21'">0.0</xsl:when>
          <xsl:otherwise>0.0</xsl:otherwise>
        </xsl:choose>

      </xsl:otherwise>
    </xsl:choose>

  </xsl:template>
</xsl:stylesheet>













