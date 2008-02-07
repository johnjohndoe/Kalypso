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

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.net.URL;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.io.FileUtils;
import org.apache.xmlbeans.XmlException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Platform;
import org.kalypso.chart.factory.configuration.ChartConfigurationLoader;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.performance.TimeLogger;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhSegmentStationComparator;
import org.kalypso.model.wspm.tuhh.schema.KalypsoModelWspmTuhhSchemaPlugin;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.ksp.chart.factory.AxisType;
import org.ksp.chart.factory.ChartConfigurationDocument;
import org.ksp.chart.factory.ChartType;
import org.ksp.chart.factory.AxisType.Direction;

public class LengthSectionProcessor
{
  private final StringBuffer m_buffer = new StringBuffer();

  private final File m_outDir;

  private final String m_footer;

  private final String m_runoffPattern;

  private final String m_epsThinning;

  private File m_gmlFile;

  private final TuhhReach m_reach;

  private File m_diagFile;

  private File m_tableFile;

  private File m_breaklineFile;

  private File m_boundaryFile;

  private File m_waterlevelFile;

  private IStatus m_result;

  private final File m_dataDir;

  private final String m_titlePattern;

  private final String m_gmlFilePattern;

  public LengthSectionProcessor( final File outDir, final TuhhReach reach, final String header, final String footer, final String epsThinning, final boolean addRunoffToFilename )
  {
    m_outDir = outDir;
    m_reach = reach;
    m_footer = footer;
    m_epsThinning = epsThinning;

    m_buffer.append( header );

    m_runoffPattern = addRunoffToFilename ? "_%.3f" : "";
    m_titlePattern = addRunoffToFilename ? "L‰ngsschnitt - %.3f" : "L‰ngsschnitt";
    m_gmlFilePattern = addRunoffToFilename ? "lengthSection_%.3f" : "L‰ngsschnitt";

    m_dataDir = new File( m_outDir, "Daten" );
  }

  public void close( final BigDecimal runoff )
  {
    try
    {
      m_buffer.append( m_footer );

      m_dataDir.mkdirs();

      final String fileName = String.format( m_gmlFilePattern + ".gml", runoff );
      m_gmlFile = new File( m_dataDir, fileName );

      /* Configure replace tokens for diagram/table template */
      final Map<String, String> replaceTokens = new LinkedHashMap<String, String>();
      replaceTokens.put( "%GMLFILENAME%", m_gmlFile.getName() );
      replaceTokens.put( "%TITLE%", String.format( m_titlePattern, runoff ) );

      // process lenghtsection to result observation (laengsschnitt.gml): concatenate new header + laengsschnitt
      // (without header) + new footer
      final String lsgml = replaceTokens( m_buffer.toString(), replaceTokens );
      FileUtils.writeStringToFile( m_gmlFile, lsgml, IWspmTuhhConstants.WSPMTUHH_CODEPAGE );

      /* Clear buffer to free resources as this object will not be dereferenced immediatly */
      m_buffer.delete( 0, m_buffer.length() - 1 );

      m_result = postProcess( m_gmlFile, runoff, replaceTokens );
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
  private IStatus postProcess( final File gmlFile, final BigDecimal runoff, final Map<String, String> replaceTokens ) throws Exception
  {
//    final TimeLogger timeLogger = new TimeLogger( "Post Processing Length-Section" );

    if( !gmlFile.exists() )
      return StatusUtilities.createWarningStatus( "L‰ngsschnitt GML wurde nicht erzeugt." );

    final String diagFilename = String.format( "L‰ngsschnitt" + m_runoffPattern + ".kod", runoff );
    final String tableFilename = String.format( "Tabelle" + m_runoffPattern + ".gft", runoff );
    final String breaklineFilename = String.format( "Bruchkanten" + m_runoffPattern + ".gml", runoff );
    final String boundaryFilename = String.format( "Modellgrenzen" + m_runoffPattern + ".gml", runoff );
    final String waterlevelFilename = String.format( "‹berschwemmungslinie" + m_runoffPattern + ".gml", runoff );

    m_diagFile = new File( m_outDir, diagFilename );
    m_tableFile = new File( m_outDir, tableFilename );
    m_breaklineFile = new File( m_dataDir, breaklineFilename );
    m_boundaryFile = new File( m_dataDir, boundaryFilename );
    m_waterlevelFile = new File( m_dataDir, waterlevelFilename );

    final MultiStatus multiStatus = new MultiStatus( PluginUtilities.id( KalypsoModelWspmTuhhSchemaPlugin.getDefault() ), -1, "", null );

    // Read Length-Section GML
//    timeLogger.takeInterimTime();
//    timeLogger.printCurrentInterim( "Start-Read LS: " );

    final GMLWorkspace obsWks = GmlSerializer.createGMLWorkspace( gmlFile.toURL(), null );
    final Feature rootFeature = obsWks.getRootFeature();

    final IObservation<TupleResult> lengthSectionObs = ObservationFeatureFactory.toObservation( rootFeature );
    final TupleResult result = lengthSectionObs.getResult();
    final String strStationierung = "urn:ogc:gml:dict:kalypso:model:wspm:components#LengthSectionStation";
    final String strWsp = "urn:ogc:gml:dict:kalypso:model:wspm:components#LengthSectionWaterlevel";

    final TuhhReachProfileSegment[] reachProfileSegments = m_reach.getReachProfileSegments();

    /* sort the segments */
//    timeLogger.takeInterimTime();
//    timeLogger.printCurrentInterim( "Sort Segments: " );

    final boolean isDirectionUpstreams = m_reach.getWaterBody().isDirectionUpstreams();
    Arrays.sort( reachProfileSegments, new TuhhSegmentStationComparator( isDirectionUpstreams ) );

    //
    // Diagramm
    //
    try
    {
//      timeLogger.takeInterimTime();
//      timeLogger.printCurrentInterim( "Start-Create Diagram " );

      final WspmWaterBody waterBody = m_reach.getWaterBody();
      createDiagram( m_diagFile, lengthSectionObs, waterBody.isDirectionUpstreams() );
      final String diagramTemplate = FileUtils.readFileToString( m_diagFile, "UTF8" );
      final String diagram = replaceTokens( diagramTemplate, replaceTokens );
      FileUtils.writeStringToFile( m_diagFile, diagram, "UTF8" );
    }
    catch( final Exception e )
    {
      multiStatus.add( StatusUtilities.statusFromThrowable( e, "Diagramm konnte nicht erzeugt werden" ) );
    }

    //
    // Table
    //
    try
    {
//      timeLogger.takeInterimTime();
//      timeLogger.printCurrentInterim( "Start-Create Table " );

      final URL tableUrl = getClass().getResource( "resources/table.gft" );
      final String tableTemplate = FileUtilities.toString( tableUrl, "UTF8" );
      final String table = replaceTokens( tableTemplate, replaceTokens );
      FileUtils.writeStringToFile( m_tableFile, table, "UTF8" );
    }
    catch( final Exception e )
    {
      multiStatus.add( StatusUtilities.statusFromThrowable( e, "Tabelle konnte nicht erzeugt werden" ) );
    }

    //
    // Breaklines
    //
    try
    {
//      timeLogger.takeInterimTime();
//      timeLogger.printCurrentInterim( "Start-Create Breaklines " );

      BreakLinesHelper.createBreaklines( reachProfileSegments, result, strStationierung, strWsp, Double.valueOf( m_epsThinning ), m_breaklineFile );
    }
    catch( final Exception e )
    {
      multiStatus.add( StatusUtilities.statusFromThrowable( e, "Bruchkanten konnten nicht erzeugt werden" ) );
    }

    //
    // Model-Boundaries
    //
    try
    {
//      timeLogger.takeInterimTime();
//      timeLogger.printCurrentInterim( "Start-Create Modelboundary " );

      BreakLinesHelper.createModelBoundary( reachProfileSegments, result, strStationierung, strWsp, m_boundaryFile, false );
    }
    catch( final Exception e )
    {
      multiStatus.add( StatusUtilities.statusFromThrowable( e, "Modellgrenzen konnten nicht erzeugt werden" ) );
    }

    //
    // Waterlevel
    //
    try
    {
//      timeLogger.takeInterimTime();
//      timeLogger.printCurrentInterim( "Start-Waterlevel " );

      BreakLinesHelper.createModelBoundary( reachProfileSegments, result, strStationierung, strWsp, m_waterlevelFile, true );
    }
    catch( final Exception e )
    {
      multiStatus.add( StatusUtilities.statusFromThrowable( e, "‹berschwemmungslinie konnte nicht erzeugt werden" ) );
    }

//    timeLogger.takeInterimTime();
//    timeLogger.printCurrentTotal( "Fertisch " );

    return multiStatus;
  }

  private String replaceTokens( final String template, final Map<String, String> tokenMap )
  {
    String result = template;
    for( final Entry<String, String> entry : tokenMap.entrySet() )
      result = result.replaceAll( entry.getKey(), entry.getValue() );

    return result;
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

  @SuppressWarnings("unchecked")
  public static void createDiagram( final File diagFile, final IObservation<TupleResult> lsObs, final boolean isDirectionUpstreams ) throws IOException, XmlException
  {
    // Check if optional bundle is installed
    if( Platform.getBundle( "org.kalypso.chart.factory" ) == null || Platform.getBundle( "org.kalypso.chart.framework" ) == null )
      return;

    /* We just load the template and tweak the direction of the station-axis */
    final URL kodResource = LengthSectionProcessor.class.getResource( "resources/lengthSection.kod" );
    final ChartConfigurationLoader ccl = new ChartConfigurationLoader( kodResource );
    final ChartConfigurationDocument ccd = ChartConfigurationDocument.Factory.parse( kodResource );

    if( ccd != null )
    {
      final ChartType[] charts = ccl.getCharts();

      // only use first chart - there should only be one
      final ChartType chart = charts[0];
      chart.setTitle( lsObs.getName() );
      chart.setDescription( lsObs.getDescription() );

      final AxisType[] axes = ccl.getAxes();
      for( final AxisType axis : axes )
      {
        if( axis.getLabel().equals( "Station_Axis" ) )
        {
          if( isDirectionUpstreams )
            axis.setDirection( Direction.NEGATIVE );
          else
            axis.setDirection( Direction.POSITIVE );
        }
      }

      ccd.save( diagFile );
    }
  }
}