/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.template;

import java.util.EventObject;

/**
 * @author schlienger
 */
public class ObsViewEvent extends EventObject
{
  public final static int TYPE_ADD = 1;

  public final static int TYPE_REMOVE = 2;

  public final static int TYPE_REMOVE_ALL = 4;

  public final static int TYPE_REFRESH = 8;

  private final Object m_obj;

  private final int m_type;

  public ObsViewEvent( final Object obj, final int type )
  {
    this( obj, obj, type );
  }

  public ObsViewEvent( final Object src, final Object obj, final int type )
  {
    super( src );
    m_obj = obj;
    m_type = type;
  }

  public Object getObject()
  {
    return m_obj;
  }

  public int getType()
  {
    return m_type;
  }

  public boolean isType( int type )
  {
    return ( m_type & type ) == m_type;
  }
}