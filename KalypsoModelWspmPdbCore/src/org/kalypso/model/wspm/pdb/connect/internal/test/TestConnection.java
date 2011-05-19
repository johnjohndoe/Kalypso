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
package org.kalypso.model.wspm.pdb.connect.internal.test;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.db.PdbInfo;
import org.kalypso.model.wspm.pdb.db.mapping.Info;
import org.kalypso.model.wspm.pdb.db.mapping.Points;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBodies;

/**
 * @author Gernot Belger
 */
public class TestConnection implements IPdbConnection
{
  private boolean m_connected = false;

  private final PdbInfo m_info = new PdbInfo( new ArrayList<Info>() );

  private final List<Points> m_points = new ArrayList<Points>();

  private final List<WaterBodies> m_waterbodies = new ArrayList<WaterBodies>();

  private final TestSettings m_settings;

  public TestConnection( final TestSettings settings )
  {
    m_settings = settings;

    /* Add some data */
    m_waterbodies.add( new WaterBodies( "1234", "B‰chle" ) );
  }

  @Override
  public void connect( )
  {
    m_connected = true;
  }

  @Override
  public boolean isConnected( )
  {
    return m_connected;
  }

  @Override
  public void close( )
  {
    m_connected = false;
  }

  @Override
  public PdbInfo getInfo( )
  {
    return m_info;
  }

  @Override
  public void addPoint( final Points onePoint )
  {
    m_points.add( onePoint );
  }

  @Override
  public String getLabel( )
  {
    return m_settings.getName();
  }

  @Override
  public List<WaterBodies> getWaterBodies( )
  {
    return m_waterbodies;
  }

  @Override
  public void addWaterBody( final WaterBodies waterBody )
  {
    m_waterbodies.add( waterBody );
  }
}
