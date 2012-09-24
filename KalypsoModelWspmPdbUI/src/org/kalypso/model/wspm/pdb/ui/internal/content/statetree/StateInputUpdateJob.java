/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.pdb.ui.internal.content.statetree;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.criterion.Restrictions;
import org.kalypso.model.wspm.pdb.PdbUtils;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.db.utils.WaterBodyUtils;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;

/**
 * @author Gernot Belger
 */
public class StateInputUpdateJob extends Job
{
  private static final Object[] INPUT_NO_SUBELEMENTS = new Object[] { new Status( IStatus.INFO, WspmPdbUiPlugin.PLUGIN_ID, "The selected water body has no content" ) };

  private final long m_waterID;

  private Object[] m_input;

  private final IPdbConnection m_connection;

  public StateInputUpdateJob( final IPdbConnection connection, final long waterID )
  {
    super( "fetch water body children" ); //$NON-NLS-1$

    m_connection = connection;
    m_waterID = waterID;

    setUser( false );
    setSystem( true );
  }

  @Override
  protected IStatus run( final IProgressMonitor monitor )
  {
    if( monitor.isCanceled() )
      return Status.CANCEL_STATUS;

    Session session = null;
    try
    {
      session = m_connection.openSession();

      final State[] states = findStates( session );
      final Event[] events = findEvents( session );

      session.close();

      if( monitor.isCanceled() )
        return Status.CANCEL_STATUS;

      final Collection<Object> input = new ArrayList<Object>( Arrays.asList( states ) );
      input.addAll( Arrays.asList( events ) );

      if( monitor.isCanceled() )
        return Status.CANCEL_STATUS;

      if( input.isEmpty() )
        m_input = INPUT_NO_SUBELEMENTS;
      else
        m_input = input.toArray();
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
    }
    finally
    {
      PdbUtils.closeSessionQuietly( session );
    }

    return Status.OK_STATUS;
  }

  private State[] findStates( final Session session )
  {
    return WaterBodyUtils.getStatesForWaterByID( session, m_waterID );
  }

  private Event[] findEvents( final Session session )
  {
    final WaterBody waterBody = WaterBodyUtils.findWaterBodyByID( session, m_waterID );
    if( waterBody == null )
      return new Event[0];

    final Criteria criteria = session.createCriteria( Event.class );
    criteria.add( Restrictions.eq( Event.PROPERTY_WATER_BODY, waterBody ) );

    final List<Event> list = criteria.list();
    return list.toArray( new Event[list.size()] );
  }

  public Object[] getInput( )
  {
    return m_input;
  }
}