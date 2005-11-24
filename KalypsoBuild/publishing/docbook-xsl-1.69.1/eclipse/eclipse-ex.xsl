<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
	
	<xsl:import href="eclipse.xsl"/>
	
	<xsl:template match="/">
		<!-- Call original code from the imported stylesheet -->
		<xsl:apply-imports/>
		
		<!-- Call custom templates for the contexts.xml -->
		<xsl:call-template name="contexts.xml"/>
	</xsl:template>
	
	<!-- Template for creating auxiliary contexts.xml file -->
	<xsl:template name="contexts.xml">
		<xsl:call-template name="write.chunk">
			<xsl:with-param name="filename" select="'contexts.xml'"/>
			<xsl:with-param name="method" select="'xml'"/>
			<xsl:with-param name="encoding" select="'utf-8'"/>
			<xsl:with-param name="indent" select="'yes'"/>
			<xsl:with-param name="content">

				<!-- TODO this should also be in the output -->
				<!--?NLS TYPE="org.eclipse.help.contexts"?-->
				<contexts>
					
				<xsl:if test="(@id)">
					<!-- Get the title of the root element -->
					<xsl:variable name="title">
						<xsl:apply-templates select="/*" mode="title.markup"/>
					</xsl:variable>
					
					<!-- Get HTML filename for the root element -->
					<xsl:variable name="href">
						<xsl:call-template name="href.target.with.base.dir">
							<xsl:with-param name="object" select="/*"/>
						</xsl:call-template>
					</xsl:variable>
					
					<context id="{@id}">
						<description><xsl:copy-of select="$title"/>
						</description>
						<topic label="{$title}" href="{$href}"/>
					</context>
				</xsl:if>
					
				<!-- Get context for all children of the root element -->
				<xsl:apply-templates select="/*/*" mode="contexts.xml"/>
					
				</contexts>
				
			</xsl:with-param>
		</xsl:call-template>
	</xsl:template>
	
	<!-- Template which converts all DocBook containers into 
	one entry in the contexts file -->
	<xsl:template
		match="part|reference|preface|chapter|
                                 bibliography|appendix|article|
                                 glossary|section|sect1|sect2|
                                 sect3|sect4|sect5|refentry|
                                 colophon|bibliodiv|index"
		mode="contexts.xml">
		
		<xsl:if test="(@id)">
			
			<!-- Get the title of the current element -->
			<xsl:variable name="title">
				<xsl:apply-templates select="." mode="title.markup"/>
			</xsl:variable>
			
			<!-- Get HTML filename for the current element -->
			<xsl:variable name="href">
				<xsl:call-template name="href.target.with.base.dir"/>
			</xsl:variable>
			
			<!-- Create ToC entry for the current node and process its 
			container-type children further -->
			<context id="{@id}">
				<description><xsl:copy-of select="$title"/></description>
				<topic label="{$title}" href="{$href}"/>
			</context>
		</xsl:if>
		
		<xsl:apply-templates
			select="part|reference|preface|chapter|
                                 bibliography|appendix|article|
                                 glossary|section|sect1|sect2|
                                 sect3|sect4|sect5|refentry|
                                 colophon|bibliodiv|index"
			mode="contexts.xml"/>
		
	</xsl:template>
	
	<!-- Default processing in the contexts.xml mode is no processing -->
	<xsl:template match="text()" mode="contexts.xml"/>
	
</xsl:stylesheet>