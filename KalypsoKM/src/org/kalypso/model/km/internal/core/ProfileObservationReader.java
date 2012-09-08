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
package org.kalypso.model.km.internal.core;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.kalypso.model.wspm.tuhh.schema.gml.QIntervallResult;
import org.kalypso.model.wspm.tuhh.schema.gml.QIntervallResultCollection;
import org.kalypso.model.wspm.tuhh.schema.schemata.IWspmTuhhQIntervallConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * This class is similiar to the {@link ProfileDataSet}, except that it uses observations.
 *
 * @author Holger Albert
 */
public class ProfileObservationReader implements IKMReader
{
  /**
   * The path of the gml file.
   */
  private final IPath m_path;

  /**
   * The constructor.
   *
   * @param path
   *          The path of the gml file.
   */
  public ProfileObservationReader( final IPath path )
  {
    m_path = path;
  }

  @Override
  public ProfileDataSet getDataSet( ) throws Exception
  {
    final ProfileDataSetFactory factory = new ProfileDataSetFactory();

    /* Load the workspace. */
    final IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile( m_path );
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( file );

    /* Get the root feature. */
    final QIntervallResultCollection qIntervalResultCollection = (QIntervallResultCollection) workspace.getRootFeature();

    /* Each feature contains one observation. */
    final IFeatureBindingCollection<QIntervallResult> qIntervalResults = qIntervalResultCollection.getQIntervalls();
    for( final QIntervallResult qIntervallResult : qIntervalResults )
    {
      /* Get the values. */
      final BigDecimal station = qIntervallResult.getStation();
      final BigDecimal slope = qIntervallResult.getSlope();
      final IObservation<TupleResult> observation = qIntervallResult.getOrCreatePointsObservation();

      /* Create the profile data. */
      final ProfileData profileData = createProfileData( station, slope, observation );

      /* Add the profile data. */
      factory.addProfileData( profileData );
    }

    return factory.createProfileDataSet();
  }

  /**
   * This function creates one profile data.
   *
   * @param station
   *          The station of the profile in kilometer.
   * @param slope
   *          The slope.
   * @param observation
   *          The source observation.
   * @return The profile data.
   */
  private ProfileData createProfileData( final BigDecimal station, final BigDecimal slope, final IObservation<TupleResult> observation )
  {
    /* Memory for the reows. */
    final List<Row> rows = new ArrayList<>();

    /* Each record represents one row. */
    final TupleResult result = observation.getResult();
    for( final IRecord record : result )
    {
      /* Find all indexes. */
      final int waterlevelIndex = record.indexOfComponent( IWspmTuhhQIntervallConstants.DICT_COMPONENT_WATERLEVEL );
      final int runoffChannelIndex = record.indexOfComponent( IWspmTuhhQIntervallConstants.DICT_COMPONENT_RUNOFF_CHANNEL );
      final int runoffFloodplainIndex = record.indexOfComponent( IWspmTuhhQIntervallConstants.DICT_COMPONENT_RUNOFF_FLOODPLAIN );
      final int areaChannelIndex = record.indexOfComponent( IWspmTuhhQIntervallConstants.DICT_COMPONENT_AREA_CHANNEL );
      final int areaFloodplainIndex = record.indexOfComponent( IWspmTuhhQIntervallConstants.DICT_COMPONENT_AREA_FLOODPLAIN );
      final int widthChannelIndex = record.indexOfComponent( IWspmTuhhQIntervallConstants.DICT_COMPONENT_WIDTH_CHANNEL );
      final int widthFloodplainIndex = record.indexOfComponent( IWspmTuhhQIntervallConstants.DICT_COMPONENT_WIDTH_FLOODPLAIN );

      // FIXME: array index out of bounds exception for polynomial results
      // -> throw exception and do not continue

      /* Get the values. */
      final BigDecimal hNN = (BigDecimal) record.getValue( waterlevelIndex );
      final BigDecimal q = (BigDecimal) record.getValue( runoffChannelIndex );
      final BigDecimal qf = (BigDecimal) record.getValue( runoffFloodplainIndex );
      final BigDecimal a = (BigDecimal) record.getValue( areaChannelIndex );
      final BigDecimal af = (BigDecimal) record.getValue( areaFloodplainIndex );
      final BigDecimal w = (BigDecimal) record.getValue( widthChannelIndex );
      final BigDecimal wf = (BigDecimal) record.getValue( widthFloodplainIndex );

      /* Create a new row. */
      final Row row = new Row( hNN.doubleValue(), q.doubleValue(), qf.doubleValue(), a.doubleValue(), af.doubleValue(), w.doubleValue(), wf.doubleValue(), slope.doubleValue() );

      /* Add the new row. */
      rows.add( row );
    }

    /* Create the profile data. */
    final ProfileData profileData = new ProfileData( m_path.toString(), station );
    profileData.set( rows.toArray( new Row[] {} ) );

    return profileData;
  }
}