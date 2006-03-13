/*
 * --------------- Kalypso-Header --------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ------------------------------------------------------------------------------------
 */
package org.kalypso.services.sensor;

import java.io.Serializable;

import javax.activation.DataHandler;

/**
 * A simple DataBean which actually wraps a DataHandler. It also has an identifier in order to keep trace of it within
 * the scope of the Observation service.
 * 
 * @author schlienger (31.05.2005)
 */
public class DataBean implements Serializable
{
  private String m_id;

  private DataHandler m_dh;

  public DataBean()
  {
    this( null, null );
  }

  public DataBean( final String id, final DataHandler dh )
  {
    m_id = id;
    m_dh = dh;
  }

  public DataHandler getDataHandler()
  {
    return m_dh;
  }

  public void setDataHandler( final DataHandler dh )
  {
    m_dh = dh;
  }

  public String getId()
  {
    return m_id;
  }

  public void setId( final String id )
  {
    m_id = id;
  }
}
