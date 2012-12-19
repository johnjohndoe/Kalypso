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
package org.kalypso.model.wspm.pdb.db.utils;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.apache.commons.lang3.ObjectUtils;
import org.hibernate.Criteria;
import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.criterion.Restrictions;
import org.kalypso.model.wspm.pdb.PdbUtils;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.GetPdbList;
import org.kalypso.model.wspm.pdb.db.mapping.State;

/**
 * @author Gernot Belger
 */
public final class StateUtils
{
  private StateUtils( )
  {
    throw new UnsupportedOperationException();
  }

  public static State findStateByName( final Collection<State> states, final String name )
  {
    for( final State state : states )
    {
      if( state != null && ObjectUtils.equals( state.getName(), name ) )
        return state;
    }
    return null;
  }

  public static State findStateByName( final State[] states, final String name )
  {
    return findStateByName( Arrays.asList( states ), name );
  }

  public static State[] getStates( final IPdbConnection connection ) throws PdbConnectException
  {
    Session session = null;
    try
    {
      session = connection.openSession();
      final List<State> list = GetPdbList.getList( session, State.class );
      final State[] states = list.toArray( new State[list.size()] );
      session.close();
      return states;
    }
    finally
    {
      PdbUtils.closeSessionQuietly( session );
    }
  }

  /**
   * Fetches the names of all existing states from the given session.
   */
  public static String[] getStateNames( final Session session )
  {
    final Query query = session.createQuery( String.format( "select %s from %s", State.PROPERTY_NAME, State.class.getName() ) ); //$NON-NLS-1$
    final List<String> list = query.list();
    return list.toArray( new String[list.size()] );
  }

  public static State findStateByName( final Session session, final String name )
  {
    final Criteria criteria = session.createCriteria( State.class );
    criteria.add( Restrictions.eq( State.PROPERTY_NAME, name ) );

    return (State)criteria.uniqueResult();
  }
}