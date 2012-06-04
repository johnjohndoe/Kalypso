/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.Assert;
import org.kalypso.model.wspm.core.gml.IObservationFeature;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.IRunOffEvent;
import org.kalypso.model.wspm.core.gml.WspmFixation;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhSegmentStationComparator;
import org.kalypso.model.wspm.tuhh.core.profile.energyloss.IEnergylossProfileObject;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.wspwin.core.LocalEnergyLossBean;
import org.kalypso.wspwin.core.LocalEnergyLossBean.LOSSKIND;
import org.kalypso.wspwin.core.ProfileBean;
import org.kalypso.wspwin.core.RunOffEventBean;
import org.kalypso.wspwin.core.WspCfg;
import org.kalypso.wspwin.core.WspCfg.TYPE;
import org.kalypso.wspwin.core.WspWinProject;
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

  public WspWinProjectWriter( final String roughnessType, final TYPE projectType, final File outputDir, final String projectName )
  {
    m_roughnessType = roughnessType;
    m_wspCfg = new WspCfg( new WspWinProject( outputDir ), projectType, projectName );
  }

  /**
   * Adds a water body and some of its reaches to this exporter.<br/>
   * All reaches MUST be part of the given water body.
   */
  public void addReaches( final WspmWaterBody waterBody, final TuhhReach[] reaches )
  {
    for( final TuhhReach tuhhReach : reaches )
      Assert.isTrue( tuhhReach.getWaterBody() == waterBody );

    /* Add all profiles of water body */
    final Map<IProfileFeature, ProfileBean> profileIndex = new HashMap<IProfileFeature, ProfileBean>();
    final IFeatureBindingCollection<IProfileFeature> profiles = waterBody.getProfiles();
    for( final IProfileFeature profileFeature : profiles )
    {
      final BigDecimal station = profileFeature.getBigStation();

      final int prfCount = m_prfCount++;
      final String prfName = formatPrfName( prfCount );

      final String waterName = waterBody.getName();
      final ProfileBean profileBean = m_wspCfg.createProfile( waterName, "export", station, prfName, ProfileBean.DEFAULT_MFB, ProfileBean.DEFAULT_VZK ); //$NON-NLS-1$
      profileIndex.put( profileFeature, profileBean );

      /* Add real profile */
      final IProfil profil = profileFeature.getProfil();
      m_profiles.put( prfCount, profil );
    }

    /* Add all reaches */
    for( final TuhhReach reach : reaches )
      addReach( reach, profileIndex );
  }

  private void addReach( final TuhhReach reach, final Map<IProfileFeature, ProfileBean> profileIndex )
  {
    final WspmWaterBody waterBody = reach.getWaterBody();

    // TODO: shorten name?
    final String waterName = waterBody.getName();

    final String zustandFilename = findZustandFilename( waterName );
    final String zustandName = reach.getName();

    final WspWinZustand zustand = m_wspCfg.createZustand( zustandName, zustandFilename, waterName, new Date() );

    final TuhhReachProfileSegment[] segments = reach.getReachProfileSegments();
    final boolean directionUpstreams = waterBody.isDirectionUpstreams();
    final TuhhSegmentStationComparator stationComparator = new TuhhSegmentStationComparator( directionUpstreams );
    Arrays.sort( segments, stationComparator );

    for( final TuhhReachProfileSegment segment : segments )
      addZustandProfile( segment, zustand, profileIndex );

    final IFeatureBindingCollection<IRunOffEvent> runoffEvents = waterBody.getRunoffEvents();
    for( final IRunOffEvent runoffEvent : runoffEvents )
      addRunoffEvent( runoffEvent, zustand );

    final IFeatureBindingCollection<WspmFixation> fixations = waterBody.getWspFixations();
    for( final WspmFixation fixation : fixations )
      addFixation( fixation, zustand );

    // TODO: calculations
    for( final TuhhReachProfileSegment segment : segments )
      addEnergyloss( segment, zustand );

  }

  private void addEnergyloss( final TuhhReachProfileSegment segment, final WspWinZustand zustand )
  {
    final IProfileFeature profileMember = segment.getProfileMember();
    final IProfil profile = profileMember.getProfil();
    final IEnergylossProfileObject[] losses = profile.getProfileObjects( IEnergylossProfileObject.class );
    final BigDecimal station = profileMember.getBigStation();
    for( int i = 0; i < losses.length; i++ )
    {
      final LocalEnergyLossBean bean = convertToEnergylossBean( station, losses[i] );
      zustand.addLoss( bean );
    }
  }

  final private LOSSKIND getLosskind( final TupleResult result, final int index )
  {
    if( index < result.size() )
    {
      final int iType = result.indexOfComponent( IEnergylossProfileObject.PROPERTY_TYPE );
      final IRecord rec = result.get( index );
      try
      {
        final String id = rec.getValue( iType ).toString();
        LOSSKIND lk = LOSSKIND.valueOf( id.replace( "�", "UE" ).toUpperCase() );
        return lk;
      }
      catch( IllegalArgumentException e )
      {
        // do nothing, just catch IllegalArgumentException
      }
    }
    return LOSSKIND.ZUSATZVERLUST;
  }

  private final LocalEnergyLossBean convertToEnergylossBean( final BigDecimal station, final IEnergylossProfileObject loss )
  {
    final Map<LocalEnergyLossBean.LOSSKIND, Double> lossMap = new HashMap<LocalEnergyLossBean.LOSSKIND, Double>();
    final TupleResult res = loss.getObservation().getResult();
    for( int i = 0; i < res.size(); i++ )
    {
      final LOSSKIND lk = getLosskind( res, i );
      if(lossMap.containsKey( lk  ))
      {
        lossMap.put( lk,lossMap.get( lk )+ loss.getValue( i ).doubleValue() );
      }
      else
      {
        lossMap.put( lk, loss.getValue( i ).doubleValue() );
      }
    }
    final LocalEnergyLossBean bean = new LocalEnergyLossBean( station, lossMap);
    return bean;
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
      final String name = String.format( "%s%06d.str", baseName, count ); //$NON-NLS-1$
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

  private void addZustandProfile( final TuhhReachProfileSegment segment, final WspWinZustand zustand, final Map<IProfileFeature, ProfileBean> profileIndex )
  {
    final IProfileFeature profileMember = segment.getProfileMember();

    final ProfileBean profileBean = profileIndex.get( profileMember );
    zustand.addProfile( profileBean );
  }

  private String formatPrfName( final int prfCount )
  {
    return String.format( "prof%04d.prf", prfCount ); //$NON-NLS-1$
  }

  public void write( ) throws IOException
  {
    final WspWinProject project = m_wspCfg.getProject();
    project.createDirs();

    final File profDir = project.getProfDir();

    /* Profiles */
    final Set<Entry<Integer, IProfil>> entrySet = m_profiles.entrySet();
    for( final Entry<Integer, IProfil> entry : entrySet )
    {
      final Integer prfCount = entry.getKey();
      final String filename = formatPrfName( prfCount );
      final IProfil profil = entry.getValue();
      final File outPrfFile = new File( profDir, filename );

      final WspWinProfileWriter profileWriter = new WspWinProfileWriter( profil, prfCount, m_roughnessType );
      profileWriter.write( outPrfFile );
    }

    m_wspCfg.updateSegmentInfo();

    m_wspCfg.write();
  }
}