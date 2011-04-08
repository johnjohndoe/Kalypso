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
package org.kalypso.model.wspm.tuhh.core.results;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.resources.IFile;
import org.kalypso.contribs.java.util.Arrays;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.result.TupleResultUtilities;
import org.kalypso.observation.util.TupleResultIndex;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathUtilities;

/**
 * @author Gernot Belger
 */
public class WspmResultLengthSection
{
  private static final String LS_TYPE_INTERPOLATED = "i"; //$NON-NLS-1$

  public static WspmResultLengthSection create( final IFile observationFile, final GMLXPath gmlxPath )
  {
    try
    {
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( observationFile );
      final Feature obsFeature = (Feature) GMLXPathUtilities.query( gmlxPath, workspace );
      final WspmResultLengthSection obs = create( obsFeature );
      workspace.dispose();
      return obs;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  public static WspmResultLengthSection create( final Feature fixation )
  {
    final IObservation<TupleResult> observation = ObservationFeatureFactory.toObservation( fixation );
    return new WspmResultLengthSection( observation );
  }

  private final IObservation<TupleResult> m_observation;

  private final TupleResultIndex m_stationIndex;

  public WspmResultLengthSection( final IObservation<TupleResult> observation )
  {
    m_observation = observation;
    final TupleResult result = m_observation.getResult();
    final IComponent stationComponent = TupleResultUtilities.findComponentById( result, IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_STATION );
    m_stationIndex = new TupleResultIndex( result, stationComponent );
  }

  public WspmResultLengthSectionColumn getColumn( final IComponent component )
  {
    final TupleResult result = m_observation.getResult();
    if( !result.hasComponent( component ) )
      return null;

    return new WspmResultLengthSectionColumn( m_observation, m_stationIndex, component );
  }

  public String getLabel( )
  {
    return m_observation.getName();
  }

  public boolean hasColumn( final IComponent component )
  {
    final TupleResult result = m_observation.getResult();
    return result.hasComponent( component );
  }

  public BigDecimal[] getStations( )
  {
    final Object[] keys = m_stationIndex.getKeys();
    return Arrays.castArray( keys, new BigDecimal[keys.length] );
  }

  public Object getValue( final BigDecimal station, final String componentIdentifier )
  {
    final int component = m_observation.getResult().indexOfComponent( componentIdentifier );
    if( component == -1 )
      return null;

    final IRecord record = m_stationIndex.getRecord( station );
    if( record == null )
      return null;

    return record.getValue( component );
  }

  public WspmResultInterpolationProfile[] findInterpolationStations( )
  {
    final Collection<WspmResultInterpolationProfile> interpolatedProfiles = new ArrayList<WspmResultInterpolationProfile>();

    final BigDecimal[] stations = getStations();
    // Find interpolated stations
    BigDecimal lastNormalStation = null;
    for( int i = 0; i < stations.length; i++ )
    {
      final BigDecimal station = stations[i];

      final String type = (String) getValue( station, IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_TYPE );
      if( LS_TYPE_INTERPOLATED.equals( type ) )
      {
        final BigDecimal nextNormalStation = findNextNormalStation( stations, i );
        if( lastNormalStation != null && nextNormalStation != null )
          interpolatedProfiles.add( new WspmResultInterpolationProfile( lastNormalStation, nextNormalStation, station ) );
      }
      else
        lastNormalStation = station;
    }

    return interpolatedProfiles.toArray( new WspmResultInterpolationProfile[interpolatedProfiles.size()] );
  }

  private BigDecimal findNextNormalStation( final BigDecimal[] stations, final int startIndex )
  {
    for( int i = startIndex; i < stations.length; i++ )
    {
      final BigDecimal station = stations[i];
      final String type = (String) getValue( station, IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_TYPE );
      if( !LS_TYPE_INTERPOLATED.equals( type ) )
        return station;
    }

    return null;
  }
}
