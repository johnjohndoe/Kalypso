<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:opt="optimizer.kalypso.org">
	<xsl:decimal-format name="kalypso" decimal-separator="." grouping-separator=";"/>
	<xsl:output method="text"/>
	<xsl:template match="/">
		
  <!--<xsl:text>debug:SCEIN.dat</xsl:text>
		-->
		<xsl:for-each select="//opt:optParameter">
			<xsl:variable name="maxn" select="opt:maxN"/>
			<xsl:variable name="kStop" select="opt:kStop"/>
			<xsl:variable name="pcento" select="opt:pcento"/>
			
			<xsl:variable name="iseed" select="opt:iseed"/>
			<xsl:variable name="ideflt" select="opt:ideflt"/>
			<xsl:variable name="iniflg" select="opt:iniflg"/>
			<xsl:variable name="iPrint" select="opt:iPrint"/>
			
			<xsl:variable name="ngs" select="opt:ngs"/>
			<xsl:variable name="npg" select="opt:npg"/>
			<xsl:variable name="nps" select="opt:nps"/>
			<xsl:variable name="nspl" select="opt:nspl"/>
			<xsl:variable name="mings" select="opt:mings"/>
			
			<!--<xsl:text>&#xA;</xsl:text>-->
			
			<xsl:call-template name="format">
				<xsl:with-param name="value" select="$maxn"/>
				<xsl:with-param name="digits" select="5"/>
				<xsl:with-param name="format" select="'0'"/>
			</xsl:call-template>
			<xsl:call-template name="format">
				<xsl:with-param name="value" select="$kStop"/>
				<xsl:with-param name="digits" select="5"/>
				<xsl:with-param name="format" select="'0'"/>
			</xsl:call-template>
			<xsl:call-template name="format">
				<xsl:with-param name="value" select="$pcento"/>
				<xsl:with-param name="digits" select="5"/>
				<xsl:with-param name="format" select="'0.00'"/>
			</xsl:call-template>
			<xsl:call-template name="format">
				<xsl:with-param name="value" select="$ngs"/>
				<xsl:with-param name="digits" select="5"/>
				<xsl:with-param name="format" select="'0'"/>
			</xsl:call-template>
			<xsl:call-template name="format">
				<xsl:with-param name="value" select="$iseed"/>
				<xsl:with-param name="digits" select="5"/>
				<xsl:with-param name="format" select="'0'"/>
			</xsl:call-template>
			<xsl:call-template name="format">
				<xsl:with-param name="value" select="$ideflt"/>
				<xsl:with-param name="digits" select="5"/>
				<xsl:with-param name="format" select="'0'"/>
			</xsl:call-template>
			
			<xsl:text>&#xA;</xsl:text>
			
			<xsl:call-template name="format">
				<xsl:with-param name="value" select="$npg"/>
				<xsl:with-param name="digits" select="5"/>
				<xsl:with-param name="format" select="'0'"/>
			</xsl:call-template>
			<xsl:call-template name="format">
				<xsl:with-param name="value" select="$nps"/>
				<xsl:with-param name="digits" select="5"/>
				<xsl:with-param name="format" select="'0'"/>
			</xsl:call-template>
			<xsl:call-template name="format">
				<xsl:with-param name="value" select="$nspl"/>
				<xsl:with-param name="digits" select="5"/>
				<xsl:with-param name="format" select="'0'"/>
			</xsl:call-template>
			<xsl:call-template name="format">
				<xsl:with-param name="value" select="$mings"/>
				<xsl:with-param name="digits" select="5"/>
				<xsl:with-param name="format" select="'0'"/>
			</xsl:call-template>
			<xsl:call-template name="format">
				<xsl:with-param name="value" select="$iniflg"/>
				<xsl:with-param name="digits" select="5"/>
				<xsl:with-param name="format" select="'0'"/>
			</xsl:call-template>
			<xsl:call-template name="format">
				<xsl:with-param name="value" select="$iPrint"/>
				<xsl:with-param name="digits" select="5"/>
				<xsl:with-param name="format" select="'0'"/>
			</xsl:call-template>
			
			<xsl:text>&#xA;</xsl:text>
		</xsl:for-each>
		
		<xsl:for-each select="//opt:parameterlist/opt:parameter">
			<xsl:variable name="iniVal" select="opt:initialValue"/>
			<xsl:variable name="upBound" select="opt:upperBound"/>
			<xsl:variable name="loBound" select="opt:lowerBound"/>
			<xsl:variable name="zaehler" select="number($iniVal) - number($loBound)"/>
			<xsl:variable name="nenner" select="number($upBound) - number($loBound)"/>
			<xsl:variable name="iniVal_trans" select="$zaehler div $nenner"/>
			
			<xsl:call-template name="format">
				<xsl:with-param name="value" select="$iniVal_trans"/><!--(number($iniVal)-number($loBound))/(number($upBound)-number($loBound))"/>-->
				<xsl:with-param name="digits" select="10"/>
				<xsl:with-param name="format" select="'0.000000'"/>
			</xsl:call-template>
			<xsl:call-template name="format">
				<xsl:with-param name="value" select="0"/>
				<xsl:with-param name="digits" select="10"/>
				<xsl:with-param name="format" select="'0.0'"/>
			</xsl:call-template>
			<xsl:call-template name="format">
				<xsl:with-param name="value" select="1"/>
				<xsl:with-param name="digits" select="10"/>
				<xsl:with-param name="format" select="'0.0'"/>
			</xsl:call-template>
			<xsl:text>&#xA;</xsl:text>
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
				<xsl:value-of select="substring('                         ',1,number($missingdigits))"/>
				<xsl:value-of select="$out"/>
			</xsl:when>
			<xsl:otherwise>
				<xsl:message>
					<xsl:text>Fehler in Formatierung!</xsl:text>
				</xsl:message>
				<xsl:value-of select="$out"/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
</xsl:stylesheet>
