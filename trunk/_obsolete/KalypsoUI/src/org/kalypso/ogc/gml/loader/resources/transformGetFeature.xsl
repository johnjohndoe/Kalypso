<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:fo="http://www.w3.org/1999/XSL/Format">
<xsl:output method="xml" version="1.0"
encoding="UTF-8" indent="no"
omit-xml-declaration="no"/>
<xsl:template match="ResultCollection">
<ResultCollection xmlns="http://www.tu-harburg.de/ofp" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:gml="http://www.opengis.net/gml">
  <xsl:copy-of select="*"/>
</ResultCollection>	
</xsl:template>
</xsl:stylesheet>
