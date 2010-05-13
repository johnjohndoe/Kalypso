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
import java.io.InputStream;
import java.math.BigDecimal;
import java.net.URL;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.math.NumberRange;
import org.apache.xmlbeans.XmlException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.net.UrlUtilities;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.awt.ColorUtilities;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhSegmentStationComparator;
import org.kalypso.model.wspm.tuhh.schema.KalypsoModelWspmTuhhSchemaPlugin;
import org.kalypso.model.wspm.tuhh.schema.i18n.Messages;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.PolygonColorMapEntry;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizerUtils;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.SurfacePolygonSymbolizer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.xml.XMLParsingException;
import org.kalypsodeegree_impl.graphics.sld.PolygonColorMap;
import org.kalypsodeegree_impl.graphics.sld.SLDFactory;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.xml.sax.SAXException;

import de.openali.odysseus.chart.factory.config.ChartConfigurationLoader;
import de.openali.odysseus.chartconfig.x020.AxisType;
import de.openali.odysseus.chartconfig.x020.ChartType;
import de.openali.odysseus.chartconfig.x020.AxisType.Direction;

public class LengthSectionProcessor
{
  private static final String TOKEN_GMLFILENAME = "%GMLFILENAME%";

  private static final String PATTERN_RUNOFF = "<runoff>";

  private static final String PATTERN_CALCNAME = "<calcname>";

  private final StringBuffer m_buffer = new StringBuffer();

  private final File m_outDir;

  private final String m_footer;

  private final String m_epsThinning;

  private File m_gmlFile;

  private File m_diagFile;

  private File m_tableFile;

  private File m_breaklineFile;

  private File m_tinFile;

  private File m_tinSldFile;

  private File m_boundaryFile;

  private File m_waterlevelFile;

  private IStatus m_result;

  private final File m_dataDir;

  private String m_titlePattern = "L‰ngsschnitt - <runoff> (<calcname>)";

  private String m_gmlFilePattern = "lengthSection_<runoff>.gml";

  private final TuhhCalculation m_calculation;

  public LengthSectionProcessor( final File outDir, final TuhhCalculation calculation, final String header, final String footer, final String epsThinning )
  {
    m_outDir = outDir;
    m_calculation = calculation;
    m_footer = footer;
    m_epsThinning = epsThinning;

    m_buffer.append( header );

    m_dataDir = new File( m_outDir, "Daten" ); //$NON-NLS-1$
  }

  public void setLsFilePattern( final String pattern )
  {
    m_gmlFilePattern = pattern;
  }

  public void setTitlePattern( final String titlePattern )
  {
    m_titlePattern = titlePattern;
  }

  public void close( final BigDecimal runoff )
  {
    try
    {
      m_buffer.append( m_footer );

      m_dataDir.mkdirs();

      final String fileName = String.format( m_gmlFilePattern, runoff ); //$NON-NLS-1$
      m_gmlFile = new File( m_dataDir, fileName );
      m_result = postProcess( m_gmlFile, runoff );
    }
    catch( final Throwable t )
    {
      t.printStackTrace();

      m_result = StatusUtilities.statusFromThrowable( t );
    }
  }

  public void addLine( final String line )
  {
    m_buffer.append( line );
    m_buffer.append( '\n' );
  }

  /** Create stuff which depends on the observation. */
  private IStatus postProcess( final File gmlFile, final BigDecimal runoff ) throws Exception
  {
    final String runoffName = runoff.toString();

    final String diagFilename = "L‰ngsschnitt" + runoffName + ".kod"; //$NON-NLS-1$ //$NON-NLS-2$
    final String tableFilename = "Tabelle" + runoffName + ".gft"; //$NON-NLS-1$ //$NON-NLS-2$
    final String breaklineFilename = "Bruchkanten" + runoffName + ".gml"; //$NON-NLS-1$ //$NON-NLS-2$
    final String tinFilename = "wspTin" + runoffName + ".gml"; //$NON-NLS-1$ //$NON-NLS-2$
    final String tinSldFilename = "wspTin" + runoffName + ".sld"; //$NON-NLS-1$ //$NON-NLS-2$
    final String boundaryFilename = "Modellgrenzen" + runoffName + ".gml"; //$NON-NLS-1$ //$NON-NLS-2$
    final String waterlevelFilename = "‹berschwemmungslinie" + runoffName + ".gml"; //$NON-NLS-1$ //$NON-NLS-2$

    m_diagFile = new File( m_outDir, diagFilename );
    m_tableFile = new File( m_outDir, tableFilename );
    m_breaklineFile = new File( m_dataDir, breaklineFilename );
    m_tinFile = new File( m_dataDir, tinFilename );
    m_tinSldFile = new File( m_dataDir, tinSldFilename );

    m_boundaryFile = new File( m_dataDir, boundaryFilename );
    m_waterlevelFile = new File( m_dataDir, waterlevelFilename );

    final MultiStatus multiStatus = new MultiStatus( PluginUtilities.id( KalypsoModelWspmTuhhSchemaPlugin.getDefault() ), -1, "", null ); //$NON-NLS-1$

    final String title = getTitle( runoff );
    final String description = String.format( "L‰ngsschnitt: %s", title );
    final IObservation<TupleResult> lengthSectionObs = createLengthSection( title, description );
    final TupleResult result = lengthSectionObs.getResult();
    if( !gmlFile.exists() )
      return StatusUtilities.createWarningStatus( Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.LengthSectionProcessor.5" ) ); //$NON-NLS-1$

    final TuhhReach reach = m_calculation.getReach();
    final TuhhReachProfileSegment[] reachProfileSegments = reach.getReachProfileSegments();

    /* sort the segments */
    final WspmWaterBody waterBody = reach.getWaterBody();
    final boolean isDirectionUpstreams = waterBody.isDirectionUpstreams();
    Arrays.sort( reachProfileSegments, new TuhhSegmentStationComparator( isDirectionUpstreams ) );

    //
    // Diagramm
    //
    try
    {
      createDiagram( m_diagFile, isDirectionUpstreams, title );
    }
    catch( final Exception e )
    {
      multiStatus.add( StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.LengthSectionProcessor.0" ) ) ); //$NON-NLS-1$
    }

    //
    // Table
    //
    try
    {
      final URL tableUrl = getClass().getResource( "resources/table.gft" ); //$NON-NLS-1$
      final String tableTemplate = FileUtilities.toString( tableUrl, "UTF-8" ); //$NON-NLS-1$
      final String table = tableTemplate.replaceAll( TOKEN_GMLFILENAME, m_gmlFile.getName() );
      FileUtils.writeStringToFile( m_tableFile, table, "UTF-8" ); //$NON-NLS-1$
    }
    catch( final Exception e )
    {
      multiStatus.add( StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.LengthSectionProcessor.1" ) ) ); //$NON-NLS-1$
    }

    //
    // Breaklines
    //
    try
    {
      final NumberRange wspRange = BreakLinesHelper.createBreaklines( reachProfileSegments, result, IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_STATION, IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_WATERLEVEL, Double.valueOf( m_epsThinning ), m_breaklineFile, m_tinFile );
      if( wspRange != null )
        createWspTinSld( wspRange );
    }
    catch( final Exception e )
    {
      multiStatus.add( StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.LengthSectionProcessor.2" ) ) ); //$NON-NLS-1$
    }

    //
    // Model-Boundaries
    //
    try
    {
      BreakLinesHelper.createModelBoundary( reachProfileSegments, result, IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_STATION, IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_WATERLEVEL, m_boundaryFile, false );
    }
    catch( final Exception e )
    {
      multiStatus.add( StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.LengthSectionProcessor.3" ) ) ); //$NON-NLS-1$
    }

    //
    // Waterlevel
    //
    try
    {
      BreakLinesHelper.createModelBoundary( reachProfileSegments, result, IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_STATION, IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_WATERLEVEL, m_waterlevelFile, true );
    }
    catch( final Exception e )
    {
      multiStatus.add( StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.LengthSectionProcessor.4" ) ) ); //$NON-NLS-1$
    }

    return multiStatus;
  }

  private String getTitle( final BigDecimal runoff )
  {
    final String calcname = m_calculation.getName();
    return m_titlePattern.replaceAll( PATTERN_RUNOFF, runoff.toString() ).replaceAll( PATTERN_CALCNAME, calcname );
  }

  private IObservation<TupleResult> createLengthSection( final String title, final String description ) throws Exception
  {
    // Read Length-Section GML
    final InputStream obsIs = IOUtils.toInputStream( m_buffer.toString(), "UTF-8" );
    m_buffer.delete( 0, m_buffer.length() - 1 );
    final GMLWorkspace obsWks = GmlSerializer.createGMLWorkspace( obsIs, null, null );
    final Feature rootFeature = obsWks.getRootFeature();
    rootFeature.setName( title );
    rootFeature.setDescription( description );

    GmlSerializer.serializeWorkspace( m_gmlFile, obsWks, "UTF-8" );

    return ObservationFeatureFactory.toObservation( rootFeature );
  }

  private void createWspTinSld( final NumberRange wspRange ) throws IOException, XMLParsingException, SAXException
  {
    /* Fetch template polygon symbolizer */
    final URL wspSldLocation = getClass().getResource( "resources/WspTin.sld" );
    final FeatureTypeStyle wspStyle = SLDFactory.createFeatureTypeStyle( null, wspSldLocation );
    final Rule[] rules = wspStyle.getRules();
    final SurfacePolygonSymbolizer polySymb = (SurfacePolygonSymbolizer) rules[0].getSymbolizers()[0];
    final PolygonColorMap colorMap = polySymb.getColorMap();

    final BigDecimal stepWidth = new BigDecimal( "0.01" );
    final BigDecimal minValue = new BigDecimal( wspRange.getMinimumDouble() );
    final BigDecimal maxValue = new BigDecimal( wspRange.getMaximumDouble() );

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
    FileUtils.writeStringToFile( m_tinSldFile, styleAsString, "UTF-8" );
  }

  public File getGmlFile( )
  {
    return m_gmlFile;
  }

  public File getDiagFile( )
  {
    return m_diagFile;
  }

  public File getTableFile( )
  {
    return m_tableFile;
  }

  public File getBreaklineFile( )
  {
    return m_breaklineFile;
  }

  public File getTinFile( )
  {
    return m_tinFile;
  }

  public File getTinSldFile( )
  {
    return m_tinSldFile;
  }

  public File getBoundaryFile( )
  {
    return m_boundaryFile;
  }

  public File getWaterlevelFile( )
  {
    return m_waterlevelFile;
  }

  public IStatus getResult( )
  {
    return m_result;
  }

  private void createDiagram( final File diagFile, final boolean isDirectionUpstreams, final String title ) throws IOException, XmlException
  {
    // Check if optional bundle is installed
    // They are no more optional... however the id has changed and this does not work any more...
    // TODO: probably its better to check per reflection if a certain class is present...
    // Or even beteer: catch the ClassNotFoundExcpetion (check if this is the right exception) and ignore it (or give a
    // warning message)
// if( Platform.getBundle( "org.kalypso.chart.factory" ) == null || Platform.getBundle( "org.kalypso.chart.framework" )
    // == null )
// return;

    /* We just load the template and tweak the direction of the station-axis */
    final URL kodResource = LengthSectionProcessor.class.getResource( "resources/lengthSection.kod" ); //$NON-NLS-1$
    final String kodContent = UrlUtilities.toString( kodResource, "UTF-8" );
    final String kodContentReplaced = kodContent.replaceAll( TOKEN_GMLFILENAME, m_gmlFile.getName() );
    final ChartConfigurationLoader ccl = new ChartConfigurationLoader( IOUtils.toInputStream( kodContentReplaced, "UTF-8" ) );

    final ChartType[] charts = ccl.getCharts();

    // only use first chart - there should only be one
    final ChartType chart = charts[0];

    chart.setTitle( String.format( "L‰ngsschnitt - %s", title ) );

    final AxisType[] axes = chart.getMappers().getAxisArray();
    for( final AxisType axis : axes )
    {
      if( axis.getLabel().equals( "Station_Axis" ) ) //$NON-NLS-1$
      {
        if( isDirectionUpstreams )
          axis.setDirection( Direction.NEGATIVE );
        else
          axis.setDirection( Direction.POSITIVE );
      }
    }

    ccl.getChartConfigurationDocument().save( diagFile );
  }
}