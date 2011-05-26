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
package org.kalypso.model.wspm.pdb.gaf.internal;

import java.util.Date;

import org.hibernate.Session;
import org.kalypso.model.wspm.pdb.connect.Executor;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.internal.AddObjectOperation;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;

import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.PrecisionModel;

/**
 * Writes a gaf profile into the database.
 * 
 * @author Gernot Belger
 */
public class Gaf2Db
{
  private final WaterBody m_waterBody;

  private final State m_state;

  private final GeometryFactory m_geometryFactory;

  private int m_profileCount = 0;

  private final Session m_session;

  public Gaf2Db( final Session session, final WaterBody waterBody, final State state, final int srid )
  {
    m_session = session;
    m_waterBody = waterBody;
    m_state = state;
    m_geometryFactory = new GeometryFactory( new PrecisionModel(), srid );
  }

  public void addState( ) throws PdbConnectException
  {
    final Date now = new Date();
    m_state.setCreationDate( now );
    m_state.setEditingDate( now );

    final AddObjectOperation addOperation = new AddObjectOperation( m_state );
    new Executor( m_session, addOperation ).execute();
  }

  public void commitProfile( final GafProfile profile ) throws PdbConnectException
  {
    final AddProfileCommand operation = new AddProfileCommand( m_profileCount++, profile, m_waterBody, m_state, m_geometryFactory );
    new Executor( m_session, operation ).execute();
  }

  public GeometryFactory getGeometryFactory( )
  {
    return m_geometryFactory;
  }
}