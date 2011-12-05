/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.tuhh.schema.simulation;

import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.net.URL;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.math.NumberRange;
import org.kalypso.contribs.java.awt.ColorUtilities;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.tuhh.schema.i18n.Messages;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.PolygonColorMapEntry;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizerUtils;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.SurfacePolygonSymbolizer;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.xml.XMLParsingException;
import org.kalypsodeegree_impl.graphics.sld.PolygonColorMap;
import org.kalypsodeegree_impl.graphics.sld.SLDFactory;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.xml.sax.SAXException;

/**
 * @author Gernot Belger
 */
public class ResultLSTinSldFile extends AbstractResultLSFile
{
  private final BreakLinesWriter m_breakLines;

  public ResultLSTinSldFile( final File outDir, final String runoffName, final BreakLinesWriter breakLines )
  {
    super( outDir, runoffName );

    m_breakLines = breakLines;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.schema.simulation.IResultLSFile#getTitle()
   */
  @Override
  public String getTitle( )
  {
    return Messages.getString("ResultLSTinSldFile_0"); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.schema.simulation.IResultLSFile#getFilename()
   */
  @Override
  public String getFilename( )
  {
    return "wspTin" + getRunoffName() + ".sld"; //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.schema.simulation.IResultLSFile#getResultID()
   */
  @Override
  public String getResultID( )
  {
    return "WspTinSld"; //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.schema.simulation.AbstractResultLSFile#doWrite(java.io.File)
   */
  @Override
  protected void doWrite( final File outputFile ) throws IOException, XMLParsingException, SAXException, GMLSchemaException, GM_Exception
  {
    final NumberRange range = m_breakLines.getRange();
    if( range == null )
      return;

    /* Fetch template polygon symbolizer */
    final URL wspSldLocation = getClass().getResource( "resources/WspTin.sld" ); //$NON-NLS-1$
    final FeatureTypeStyle wspStyle = SLDFactory.createFeatureTypeStyle( null, wspSldLocation );
    final Rule[] rules = wspStyle.getRules();
    final SurfacePolygonSymbolizer polySymb = (SurfacePolygonSymbolizer) rules[0].getSymbolizers()[0];
    final PolygonColorMap colorMap = polySymb.getColorMap();

    final BigDecimal stepWidth = new BigDecimal( "0.01" ); //$NON-NLS-1$
    final BigDecimal minValue = new BigDecimal( range.getMinimumDouble() );
    final BigDecimal maxValue = new BigDecimal( range.getMaximumDouble() );

    final Color minFill = new Color( 0, 255, 0, 128 );
    final Color maxFill = new Color( 255, 0, 0, 128 );
    final Color minStroke = ColorUtilities.createTransparent( minFill, 255 );
    final Color maxStroke = ColorUtilities.createTransparent( maxFill, 255 );
    final PolygonColorMapEntry fromEntry = StyleFactory.createPolygonColorMapEntry( minFill, minStroke, minValue, minValue.add( stepWidth ) );
    final PolygonColorMapEntry toEntry = StyleFactory.createPolygonColorMapEntry( maxFill, maxStroke, maxValue.subtract( stepWidth ), maxValue );

    /* Create and replace new color map */
    final List<PolygonColorMapEntry> colorMapEntries = PolygonSymbolizerUtils.createColorMap( fromEntry, toEntry, stepWidth, minValue, maxValue, true );
    colorMap.replaceColorMap( colorMapEntries );

    /* Save as tin-sld */
    final String styleAsString = wspStyle.exportAsXML();
    FileUtils.writeStringToFile( outputFile, styleAsString, "UTF-8" ); //$NON-NLS-1$
  }
}
