<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<xsl:decimal-format name="kalypso" decimal-separator="." grouping-separator=";"/>
	<xsl:output method="text"/>
	<xsl:template match="/">
		
		<!--<xsl:text>SCEIN.dat</xsl:text>-->
		
		<xsl:for-each select="//optParameter">
			<xsl:variable name="maxn" select="maxN"/>
			<xsl:variable name="kStop" select="kStop"/>
			<xsl:variable name="pcento" select="pcento"/>
			
			<xsl:variable name="iseed" select="iseed"/>
			<xsl:variable name="ideflt" select="ideflt"/>
			<xsl:variable name="iniflg" select="iniflg"/>
			<xsl:variable name="iPrint" select="iPrint"/>
			
			<xsl:variable name="ngs" select="ngs"/>
			<xsl:variable name="npg" select="npg"/>
			<xsl:variable name="nps" select="nps"/>
			<xsl:variable name="nspl" select="nspl"/>
			<xsl:variable name="mings" select="mings"/>
			
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
		
		<xsl:for-each select="//parameterlist/parameter">
			<xsl:variable name="iniVal" select="initialValue"/>
			<xsl:variable name="upBound" select="upperBound"/>
			<xsl:variable name="loBound" select="lowerBound"/>
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
