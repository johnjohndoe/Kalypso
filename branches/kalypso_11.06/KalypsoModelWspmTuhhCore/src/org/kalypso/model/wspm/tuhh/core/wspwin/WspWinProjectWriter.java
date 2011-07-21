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
package org.kalypso.model.wspm.tuhh.core.wspwin;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.net.URL;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.eclipse.core.resources.IProject;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.model.wspm.core.gml.IObservationFeature;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.IRunOffEvent;
import org.kalypso.model.wspm.core.gml.WspmFixation;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhSegmentStationComparator;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.wspwin.core.RunOffEventBean;
import org.kalypso.wspwin.core.WspCfg;
import org.kalypso.wspwin.core.WspCfg.TYPE;
import org.kalypso.wspwin.core.WspWinFiles;
import org.kalypso.wspwin.core.WspWinZustand;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * Writes the state for calculation with kalypso-1d.exe (which is different from wspwin format!).
 * 
 * @author Gernot Belger
 */
public class WspWinProjectWriter
{
  private final WspCfg m_wspCfg;

  private final Map<Integer, IProfil> m_profiles = new HashMap<Integer, IProfil>();

  private final String m_roughnessType;

  private int m_prfCount = 1;

  private final File m_outputDir;

  public WspWinProjectWriter( final String roughnessType, final TYPE projectType, final File outputDir )
  {
    m_roughnessType = roughnessType;
    m_outputDir = outputDir;
    m_wspCfg = new WspCfg( projectType );
  }

  public void addReach( final TuhhReach reach )
  {
    final WspmWaterBody waterBody = reach.getWaterBody();
    final String waterName = waterBody.getName();
    final String zustandFilename = findZustandFilename( waterName );
    final String zustandName = reach.getName();

    final URL context = waterBody.getWorkspace().getContext();
    final IProject project = ResourceUtilities.findProjectFromURL( context );
    if( project != null )
      m_wspCfg.setProjectName( project.getName() );

    final WspWinZustand zustand = m_wspCfg.createZustand( zustandName, zustandFilename, waterName, new Date() );

    final TuhhReachProfileSegment[] segments = reach.getReachProfileSegments();
    final boolean directionUpstreams = waterBody.isDirectionUpstreams();
    final TuhhSegmentStationComparator stationComparator = new TuhhSegmentStationComparator( directionUpstreams );
    Arrays.sort( segments, stationComparator );

    for( final TuhhReachProfileSegment segment : segments )
      addProfile( segment, zustand );

    final IFeatureBindingCollection<IRunOffEvent> runoffEvents = waterBody.getRunoffEvents();
    for( final IRunOffEvent runoffEvent : runoffEvents )
      addRunoffEvent( runoffEvent, zustand );

    final IFeatureBindingCollection<WspmFixation> fixations = waterBody.getWspFixations();
    for( final WspmFixation fixation : fixations )
      addFixation( fixation, zustand );

    // TODO: losses + calculations
  }

  private void addRunoffEvent( final IRunOffEvent runoffEvent, final WspWinZustand zustand )
  {
    final RunOffEventBean bean = convertToRunoffBean( runoffEvent, IRunOffEvent.COMPONENT_RUNOFF );
    zustand.addRunoff( bean );
  }

  private void addFixation( final WspmFixation runoffEvent, final WspWinZustand zustand )
  {
    final RunOffEventBean bean = convertToRunoffBean( runoffEvent, WspmFixation.COMPONENT_WSP );
    zustand.addWspFix( bean );
  }

  private RunOffEventBean convertToRunoffBean( final IObservationFeature feature, final String valueComponent )
  {
    final RunOffEventBean bean = new RunOffEventBean( feature.getName() );

    final IObservation<TupleResult> obs = feature.toObservation();
    final TupleResult result = obs.getResult();

    final int stationIndex = result.indexOfComponent( IRunOffEvent.COMPONENT_STATION );
    final int runoffIndex = result.indexOfComponent( valueComponent );

    for( final IRecord record : result )
    {
      final BigDecimal station = (BigDecimal) record.getValue( stationIndex );
      final BigDecimal value = (BigDecimal) record.getValue( runoffIndex );
      bean.addEntry( station, value );
    }

    return bean;
  }

  private String findZustandFilename( final String waterName )
  {
    /* First two characters of water name */
    String baseName;
    if( StringUtils.isBlank( waterName ) || waterName.length() < 2 )
      baseName = "XX"; //$NON-NLS-1$
    else
      baseName = waterName.substring( 0, 2 );

    /* Make unique within project */
    final String[] zustandNames = getZustandNames();
    for( int count = 1; count < 10E6; count++ )
    {
      final String name = String.format( "%s%06d.str", baseName, count );
      if( !ArrayUtils.contains( zustandNames, name ) )
        return name;
    }

    throw new IllegalStateException();
  }

  private String[] getZustandNames( )
  {
    final Set<String> names = new HashSet<String>();

    final WspWinZustand[] zustaende = m_wspCfg.getZustaende();
    for( final WspWinZustand zustand : zustaende )
      names.add( zustand.getBean().getFileName() );

    return names.toArray( new String[names.size()] );
  }

  private void addProfile( final TuhhReachProfileSegment segment, final WspWinZustand zustand )
  {
    final BigDecimal station = segment.getStation();

    final int prfCount = m_prfCount++;
    final String prfName = formatPrfName( prfCount );
    zustand.addProfile( prfName, station );

    /* Add real profile */
    final IProfileFeature profileMember = segment.getProfileMember();
    final IProfil profil = profileMember.getProfil();
    profil.setStation( station.doubleValue() );
    m_profiles.put( prfCount, profil );
  }

  private String formatPrfName( final int prfCount )
  {
    return String.format( "prof%04d.prf", prfCount ); //$NON-NLS-1$
  }

  public void write( ) throws IOException
  {
    m_outputDir.mkdirs();
    new File( m_outputDir, WspWinFiles.DATH ).mkdir();
    final File profDir = new File( m_outputDir, WspWinFiles.PROF );
    profDir.mkdir();

    /* Profiles */
    final Set<Entry<Integer, IProfil>> entrySet = m_profiles.entrySet();
    for( final Entry<Integer, IProfil> entry : entrySet )
    {
      final Integer prfCount = entry.getKey();
      final String filename = formatPrfName( prfCount );
      final IProfil profil = entry.getValue();
      final File outPrfFile = new File( profDir, filename );

      final WspWinProfileWriter profileWriter = new WspWinProfileWriter( profil, m_roughnessType, prfCount );
      profileWriter.write( outPrfFile );
    }

    m_wspCfg.write( m_outputDir );
  }
}