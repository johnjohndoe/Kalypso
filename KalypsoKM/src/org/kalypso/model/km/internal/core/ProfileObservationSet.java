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

import java.io.File;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

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
public class ProfileObservationSet extends AbstractProfileDataSet
{
  /**
   * The gml file.
   */
  private File m_gmlFile;

  /**
   * The constructor.
   * 
   * @param gmlFile
   *          The gml file.
   * @param startPosition
   *          The start position (the first station). In meters!
   * @param endPosition
   *          The end position (the last station). In meters!
   */
  public ProfileObservationSet( File gmlFile, double startPosition, double endPosition )
  {
    super( startPosition, endPosition );

    m_gmlFile = gmlFile;
  }

  /**
   * @see org.kalypso.model.km.internal.core.AbstractProfileDataSet#createProfileData()
   */
  @Override
  protected void createProfileData( )
  {
    try
    {
      /* Load the workspace. */
      GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( m_gmlFile, null );

      /* Get the root feature. */
      QIntervallResultCollection qIntervalResultCollection = (QIntervallResultCollection) workspace.getRootFeature();

      /* Each feature contains one observation. */
      IFeatureBindingCollection<QIntervallResult> qIntervalResults = qIntervalResultCollection.getQIntervalls();
      for( QIntervallResult qIntervallResult : qIntervalResults )
      {
        /* Get the values. */
        BigDecimal station = qIntervallResult.getStation();
        BigDecimal slope = qIntervallResult.getSlope();
        IObservation<TupleResult> observation = qIntervallResult.getOrCreatePointsObservation();

        /* Create the profile data. */
        ProfileData profileData = createProfileData( station, slope, observation );

        /* Add the profile data. */
        addProfileData( profileData );
      }
    }
    catch( Exception ex )
    {
      ex.printStackTrace();
    }
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
  private ProfileData createProfileData( BigDecimal station, BigDecimal slope, IObservation<TupleResult> observation )
  {
    /* Memory for the reows. */
    List<Row> rows = new ArrayList<Row>();

    /* Each record represents one row. */
    TupleResult result = observation.getResult();
    for( IRecord record : result )
    {
      /* Find all indexes. */
      int waterlevelIndex = record.indexOfComponent( IWspmTuhhQIntervallConstants.DICT_COMPONENT_WATERLEVEL );
      int runoffChannelIndex = record.indexOfComponent( IWspmTuhhQIntervallConstants.DICT_COMPONENT_RUNOFF_CHANNEL );
      int runoffFloodplainIndex = record.indexOfComponent( IWspmTuhhQIntervallConstants.DICT_COMPONENT_RUNOFF_FLOODPLAIN );
      int areaChannelIndex = record.indexOfComponent( IWspmTuhhQIntervallConstants.DICT_COMPONENT_AREA_CHANNEL );
      int areaFloodplainIndex = record.indexOfComponent( IWspmTuhhQIntervallConstants.DICT_COMPONENT_AREA_FLOODPLAIN );
      int widthChannelIndex = record.indexOfComponent( IWspmTuhhQIntervallConstants.DICT_COMPONENT_WIDTH_CHANNEL );
      int widthFloodplainIndex = record.indexOfComponent( IWspmTuhhQIntervallConstants.DICT_COMPONENT_WIDTH_FLOODPLAIN );

      /* Get the values. */
      BigDecimal hNN = (BigDecimal) record.getValue( waterlevelIndex );
      BigDecimal q = (BigDecimal) record.getValue( runoffChannelIndex );
      BigDecimal qf = (BigDecimal) record.getValue( runoffFloodplainIndex );
      BigDecimal a = (BigDecimal) record.getValue( areaChannelIndex );
      BigDecimal af = (BigDecimal) record.getValue( areaFloodplainIndex );
      BigDecimal w = (BigDecimal) record.getValue( widthChannelIndex );
      BigDecimal wf = (BigDecimal) record.getValue( widthFloodplainIndex );

      /* Create a new row. */
      Row row = new Row( hNN.doubleValue(), q.doubleValue(), qf.doubleValue(), a.doubleValue(), af.doubleValue(), w.doubleValue(), wf.doubleValue(), slope.doubleValue() );

      /* Add the new row. */
      rows.add( row );
    }

    /* Create the profile data. */
    ProfileData profileData = new ProfileData( m_gmlFile, getStartPosition(), getEndPosition(), 1000d * station.doubleValue() );
    profileData.set( rows.toArray( new Row[] {} ) );

    return profileData;
  }
}