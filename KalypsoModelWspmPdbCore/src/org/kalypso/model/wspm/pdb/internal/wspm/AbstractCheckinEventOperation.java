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
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.hibernate.Session;
import org.kalypso.model.wspm.core.IWspmLengthSectionProperties;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.IPdbOperation;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.db.mapping.WaterlevelFixation;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.wspm.SaveEventOperation;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * @author Gernot Belger
 */
public abstract class AbstractCheckinEventOperation implements IPdbOperation
{
  static final String STR_FAILED_TO_CONVERT_GEOMETRY = Messages.getString( "AbstractCheckinEventOperation.0" ); //$NON-NLS-1$

  private final Map<String, WaterBody> m_waterBodies;

  private final IProgressMonitor m_monitor;

  private final Event m_event;

  private final WspmWaterBody m_wspmWaterBody;

  private final IPdbConnection m_connection;

  private IStatus m_log;

  public AbstractCheckinEventOperation( final IPdbConnection connection, final Map<String, WaterBody> waterHash, final WspmWaterBody wspmWaterBody, final Event event, final IProgressMonitor monitor )
  {
    m_connection = connection;
    m_wspmWaterBody = wspmWaterBody;
    m_event = event;
    m_waterBodies = waterHash;

    m_monitor = monitor;
  }

  public IStatus getLog( )
  {
    return m_log;
  }

  @Override
  public String getLabel( )
  {
    return Messages.getString( "AbstractCheckinEventOperation.1" ); //$NON-NLS-1$
  }

  @Override
  public void execute( final Session session ) throws PdbConnectException
  {
    m_monitor.beginTask( Messages.getString( "AbstractCheckinEventOperation.2" ), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$

    m_monitor.subTask( Messages.getString( "AbstractCheckinEventOperation.3" ) ); //$NON-NLS-1$

    // FIXME: we should clone the event object, else we get problems if the upload fails with an error

    /* set water body */
    m_event.setWaterBody( findWaterBody() );

    /* convert observation to fixations */
    final IObservation<TupleResult> observation = getObservation();

    final TupleResult result = observation.getResult();

    final int stationIndex = result.indexOfComponent( IWspmLengthSectionProperties.LENGTH_SECTION_PROPERTY_STATION );
    final int waterlevelIndex = result.indexOfComponent( IWspmLengthSectionProperties.LENGTH_SECTION_PROPERTY_WATERLEVEL );
    final int runoffIndex = result.indexOfComponent( IWspmLengthSectionProperties.LENGTH_SECTION_PROPERTY_RUNOFF );

    for( final IRecord record : result )
    {
      final BigDecimal station = asBigDecimal( stationIndex, record );
      final BigDecimal stationM = station == null ? null : station.movePointRight( 3 );
      final BigDecimal wsp = asBigDecimal( waterlevelIndex, record );
      final BigDecimal runoff = asBigDecimal( runoffIndex, record );

      final WaterlevelFixation element = new WaterlevelFixation();
      element.setDescription( StringUtils.EMPTY );
      element.setDischarge( runoff );
      // FIXME
      element.setLocation( null );
      element.setStation( stationM );
      element.setWaterlevel( wsp );

      element.setEvent( m_event );
      m_event.getWaterlevelFixations().add( element );
    }

    /* save event */
    final String username = m_connection.getSettings().getUsername();
    final int dbSRSID = m_connection.getInfo().getSRID();
    final SaveEventOperation operation = new SaveEventOperation( m_event, username, dbSRSID );
    operation.execute( session );
    m_log = operation.getLog();

    m_monitor.subTask( Messages.getString( "AbstractCheckinEventOperation.4" ) ); //$NON-NLS-1$
  }

  protected abstract IObservation<TupleResult> getObservation( );

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
    final String refNr = m_wspmWaterBody.getRefNr();
    return m_waterBodies.get( refNr );
  }
}