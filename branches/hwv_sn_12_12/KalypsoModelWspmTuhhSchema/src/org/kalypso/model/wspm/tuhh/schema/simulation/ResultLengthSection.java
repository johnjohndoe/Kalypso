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
import java.io.InputStream;
import java.math.BigDecimal;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.wspm.core.IWspmConstants;
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
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * Represents a result (in form of a lengthsection/table) of WSPM.<br/>
 * 
 * @author Gernot Belger
 */
public class ResultLengthSection
{
  private static final String PATTERN_RUNOFF = "<runoff>"; //$NON-NLS-1$

  private static final String PATTERN_CALCNAME = "<calcname>"; //$NON-NLS-1$

  private final StringBuffer m_buffer = new StringBuffer();

  private final Collection<IResultLSFile> m_resultFiles = new ArrayList<IResultLSFile>();

  private final File m_outDir;

  private final String m_epsThinning;

  private IStatus m_result;

  private final File m_dataDir;

  private String m_titlePattern = Messages.getString( "ResultLengthSection.0" ); //$NON-NLS-1$

  private String m_gmlFilePattern = "lengthSection_<runoff>.gml"; //$NON-NLS-1$

  private final TuhhCalculation m_calculation;

  private final BigDecimal m_runoff;

  private final URL m_ovwMapURL;

  public ResultLengthSection( final BigDecimal runoff, final File outDir, final TuhhCalculation calculation, final String epsThinning, final URL ovwMapURL )
  {
    m_runoff = runoff;
    m_outDir = outDir;
    m_calculation = calculation;
    m_epsThinning = epsThinning;
    m_ovwMapURL = ovwMapURL;

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

  public void close( )
  {
    try
    {
      m_dataDir.mkdirs();
      m_result = postProcess();
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
  private IStatus postProcess( ) throws Exception
  {
    final String runoffName = m_runoff.toString();

    final String title = getTitle();

    final GMLWorkspace lengthSectionWorkspace = createLengthSection( title );
    final IObservation<TupleResult> lengthSectionObs = ObservationFeatureFactory.toObservation( lengthSectionWorkspace.getRootFeature() );
    final TupleResult result = lengthSectionObs.getResult();

    /* Some handlers need access to the reach/profiles, fetch 'em! */
    final TuhhReach reach = m_calculation.getReach();
    final TuhhReachProfileSegment[] reachProfileSegments = reach.getReachProfileSegments();

    /* sort the segments */
    final WspmWaterBody waterBody = reach.getWaterBody();
    final boolean isDirectionUpstreams = waterBody.isDirectionUpstreams();
    Arrays.sort( reachProfileSegments, new TuhhSegmentStationComparator( isDirectionUpstreams ) );

    /* Breaklines */
    final BreakLinesWriter breakLines = new BreakLinesWriter( reachProfileSegments, result, IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_STATION, IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_WATERLEVEL, Double.valueOf( m_epsThinning ) );

    final String dataFilename = m_gmlFilePattern.replaceAll( PATTERN_RUNOFF, runoffName );

    /* Create result file handlers */
    addResultFile( new ResultLSGmlFile( m_dataDir, dataFilename, lengthSectionWorkspace ) );
    addResultFile( new ResultLSChartFile( m_outDir, runoffName, isDirectionUpstreams, dataFilename, title ) );
    addResultFile( new ResultLSTableFile( m_outDir, runoffName, dataFilename ) );
    addResultFile( new ResultLSBreaklinesFile( m_outDir, runoffName, breakLines ) );
    addResultFile( new ResultLSTinFile( m_outDir, runoffName, breakLines ) );
    addResultFile( new ResultLSTinSldFile( m_outDir, runoffName, breakLines ) );
    addResultFile( new ResultLSModelBoundaryFile( m_outDir, runoffName, result, reachProfileSegments ) );
    addResultFile( new ResultLSWaterlevelFile( m_outDir, runoffName, result, reachProfileSegments ) );
    addResultFile( new ResultLSOverviewMapFile( m_outDir, reach, m_ovwMapURL ) );

    return writeResultFiles();
  }

  private IStatus writeResultFiles( )
  {
    final IStatusCollector statusCollector = new StatusCollector( KalypsoModelWspmTuhhSchemaPlugin.getID() );

    for( final IResultLSFile resultFile : m_resultFiles )
    {
      final IStatus status = resultFile.writeFile();
      if( !status.isOK() )
        statusCollector.add( status );
    }

    return statusCollector.asMultiStatusOrOK( Messages.getString( "ResultLengthSection.3" ) ); //$NON-NLS-1$
  }

  private void addResultFile( final IResultLSFile resultFile )
  {
    m_resultFiles.add( resultFile );
  }

  private String getTitle( )
  {
    final String calcname = m_calculation.getName();
    return m_titlePattern.replaceAll( PATTERN_RUNOFF, m_runoff.toString() ).replaceAll( PATTERN_CALCNAME, calcname );
  }

  private GMLWorkspace createLengthSection( final String title ) throws Exception
  {
    final String description = String.format( Messages.getString( "ResultLengthSection.1" ), title ); //$NON-NLS-1$

    // Read Length-Section GML
    final InputStream obsIs = IOUtils.toInputStream( m_buffer.toString(), "UTF-8" ); //$NON-NLS-1$
    m_buffer.delete( 0, m_buffer.length() - 1 );
    final GMLWorkspace obsWks = GmlSerializer.createGMLWorkspace( obsIs, null, null );
    final Feature rootFeature = obsWks.getRootFeature();

    final IObservation<TupleResult> lengthSectionObs = ObservationFeatureFactory.toObservation( rootFeature );

    /* Set title */
    lengthSectionObs.setName( title );
    lengthSectionObs.setDescription( description );

    /* Add additional columns that are not returned by the calculation core */
    final TupleResult result = lengthSectionObs.getResult();

    final ILengthSectionColumn[] columns = new ILengthSectionColumn[] { //
        new LengthSectionColumnAdd( IWspmConstants.LENGTH_SECTION_PROPERTY_F, IWspmConstants.LENGTH_SECTION_PROPERTY_F_LI, IWspmConstants.LENGTH_SECTION_PROPERTY_F_FL, IWspmConstants.LENGTH_SECTION_PROPERTY_F_RE ), //
        new LengthSectionColumnAdd( IWspmConstants.LENGTH_SECTION_PROPERTY_BR, IWspmConstants.LENGTH_SECTION_PROPERTY_BR_LI, IWspmConstants.LENGTH_SECTION_PROPERTY_BR_FL, IWspmConstants.LENGTH_SECTION_PROPERTY_BR_RE ), //
        new LengthSectionColumnDivide( IWspmConstants.LENGTH_SECTION_PROPERTY_V_LI, IWspmConstants.LENGTH_SECTION_PROPERTY_Q_LI, IWspmConstants.LENGTH_SECTION_PROPERTY_F_LI ), //
        new LengthSectionColumnDivide( IWspmConstants.LENGTH_SECTION_PROPERTY_V_FL, IWspmConstants.LENGTH_SECTION_PROPERTY_Q_FL, IWspmConstants.LENGTH_SECTION_PROPERTY_F_FL ), //
        new LengthSectionColumnDivide( IWspmConstants.LENGTH_SECTION_PROPERTY_V_RE, IWspmConstants.LENGTH_SECTION_PROPERTY_Q_RE, IWspmConstants.LENGTH_SECTION_PROPERTY_F_RE ), //
        new LengthSectionColumnFroude() //
    };
    for( final ILengthSectionColumn column : columns )
      column.addColumn( result );

    ObservationFeatureFactory.toFeature( lengthSectionObs, rootFeature );

    return obsWks;
  }

  public IStatus getResult( )
  {
    return m_result;
  }

  public IResultLSFile[] getResultFiles( )
  {
    return m_resultFiles.toArray( new IResultLSFile[m_resultFiles.size()] );
  }
}