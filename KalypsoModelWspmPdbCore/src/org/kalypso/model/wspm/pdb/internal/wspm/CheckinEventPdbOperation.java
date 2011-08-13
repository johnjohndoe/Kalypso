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
package org.kalypso.model.wspm.pdb.internal.wspm;

import java.math.BigDecimal;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.hibernate.Session;
import org.kalypso.model.wspm.core.gml.WspmFixation;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.pdb.connect.IPdbOperation;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.db.mapping.WaterlevelFixation;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * @author Gernot Belger
 */
public class CheckinEventPdbOperation implements IPdbOperation
{
  static final String STR_FAILED_TO_CONVERT_GEOMETRY = "Failed to convert geometry";

  private final Map<String, WaterBody> m_waterBodies = new HashMap<String, WaterBody>();

  private final IProgressMonitor m_monitor;

  private final WspmFixation m_fixation;

  private final Event m_event;

  public CheckinEventPdbOperation( final Event event, final WaterBody[] waterBodies, final WspmFixation fixation, final IProgressMonitor monitor )
  {
    m_event = event;
    m_fixation = fixation;

    for( final WaterBody waterBody : waterBodies )
      m_waterBodies.put( waterBody.getName(), waterBody );

    m_monitor = monitor;
  }

  @Override
  public String getLabel( )
  {
    return "Upload cross sections into database";
  }

  @Override
  public void execute( final Session session )
  {
    m_monitor.beginTask( "Uploading new event into database", IProgressMonitor.UNKNOWN );

    m_monitor.subTask( "saving state..." );

    final Date now = new Date();
    m_event.setCreationDate( now );
    m_event.setEditingDate( now );
    m_event.setWaterBody( findWaterBody() );

    session.save( m_event );

    final IObservation<TupleResult> observation = m_fixation.toObservation();
    final TupleResult result = observation.getResult();

    final int stationIndex = result.indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_STATION );
    final int waterlevelIndex = result.indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_WATERLEVEL );
    final int runoffIndex = result.indexOfComponent( IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_RUNOFF );

    for( final IRecord record : result )
    {
      final BigDecimal station = asBigDecimal( stationIndex, record );
      final BigDecimal stationM = station == null ? null : station.movePointRight( 3 );
      final BigDecimal wsp = asBigDecimal( waterlevelIndex, record );
      final BigDecimal runoff = asBigDecimal( runoffIndex, record );

      final WaterlevelFixation element = new WaterlevelFixation();
      element.setCreationDate( m_event.getCreationDate() );
      element.setDescription( StringUtils.EMPTY );
      element.setDischarge( runoff );
      element.setEditingDate( m_event.getEditingDate() );
      element.setEditingUser( m_event.getEditingUser() );
      element.setEvent( m_event );
      element.setLocation( null );
      element.setMeasurementDate( m_event.getMeasurementDate() );
      element.setStation( stationM );
      element.setWaterlevel( wsp );

      session.save( element );
    }

    m_monitor.subTask( "transferring data into database..." );
  }

  protected BigDecimal asBigDecimal( final int componentIndex, final IRecord record )
  {
    if( componentIndex == -1 )
      return null;

    final Object value = record.getValue( componentIndex );
    if( value instanceof BigDecimal )
      return (BigDecimal) value;

    if( value instanceof Number )
      return new BigDecimal( ((Number) value).doubleValue() );

    return null;
  }

  private WaterBody findWaterBody( )
  {
    final WspmWaterBody wspmWaterBody = m_fixation.getParent();
    final String refNr = wspmWaterBody.getRefNr();
    return m_waterBodies.get( refNr );
  }
}