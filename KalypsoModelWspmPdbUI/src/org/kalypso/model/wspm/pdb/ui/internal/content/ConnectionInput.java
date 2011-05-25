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
package org.kalypso.model.wspm.pdb.ui.internal.content;

import java.util.ArrayList;
import java.util.List;

import org.hibernate.Session;
import org.kalypso.model.wspm.pdb.connect.Executor;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.ListOperation;
import org.kalypso.model.wspm.pdb.db.mapping.States;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBodies;

/**
 * @author Gernot Belger
 */
public class ConnectionInput
{
  private final Session m_session;

  private final List<States> m_states;

  private final List<WaterBodies> m_waterBodies;

  public ConnectionInput( final Session session )
  {
    m_session = session;

    m_states = readStates();
    m_waterBodies = readWaterBodies();
  }

  private List<States> readStates( )
  {
    try
    {
      final ListOperation<States> operation = new ListOperation<States>( States.class );
      new Executor( m_session, operation ).execute();
      return operation.getList();
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      return new ArrayList<States>();
    }
  }

  private List<WaterBodies> readWaterBodies( )
  {
    try
    {
      final ListOperation<WaterBodies> operation = new ListOperation<WaterBodies>( WaterBodies.class );
      new Executor( m_session, operation ).execute();
      return operation.getList();
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      return new ArrayList<WaterBodies>();
    }
  }

  public void dispose( )
  {
    m_session.close();
  }

  public States[] getStates( )
  {
    return m_states.toArray( new States[m_states.size()] );
  }

  public WaterBodies[] getWaterBodies( )
  {
    return m_waterBodies.toArray( new WaterBodies[m_waterBodies.size()] );
  }
}