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

import org.eclipse.core.resources.IFile;
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
  public static WspmResultLengthSection create( final IFile observationFile, final GMLXPath gmlxPath )
  {
    try
    {
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( observationFile );
      final Feature obsFeature = (Feature) GMLXPathUtilities.query( gmlxPath, workspace );
      return create( obsFeature );
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

  private final int m_componentWaterlevel;

  private final int m_componentRunoff;

  public WspmResultLengthSection( final IObservation<TupleResult> observation )
  {
    m_observation = observation;
    final TupleResult result = m_observation.getResult();
    final IComponent stationComponent = TupleResultUtilities.findComponentById( result, IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_STATION );
    m_stationIndex = new TupleResultIndex( result, stationComponent );
    m_componentWaterlevel = result.indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_WATERLEVEL );
    m_componentRunoff = result.indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_RUNOFF );
  }

  private BigDecimal getValue( final BigDecimal station, final int componentIndex )
  {
    final IRecord record = m_stationIndex.getRecord( station );
    if( record == null )
      return null;

    return (BigDecimal) record.getValue( componentIndex );
  }

  /** The result value for the given station */
  public BigDecimal getWaterlevel( final BigDecimal station )
  {
    return getValue( station, m_componentWaterlevel );
  }

  public BigDecimal getRunoff( final BigDecimal station )
  {
    return getValue( station, m_componentRunoff );
  }

  public String getLabel( )
  {
    return m_observation.getName();
  }

}
