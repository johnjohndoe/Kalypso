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
package org.kalypso.repository.beans;

import java.io.Serializable;

/**
 * A Repository Item Bean: an element of a repository.
 * 
 * @author schlienger
 */
public class ItemBean implements Serializable
{
  private String m_id;

  private String m_name;

  private String m_repId;

  public ItemBean()
  {
    this( "", "", "" );
  }

  /**
   * @param id
   *          identifier of this item
   * @param name
   *          name of this item
   * @param repId
   *          identifier of the repository this item belongs to
   */
  public ItemBean( final String id, final String name, final String repId )
  {
    m_id = id;
    m_name = name;
    m_repId = repId;
  }

  public String getId()
  {
    return m_id;
  }

  public void setId( final String id )
  {
    m_id = id;
  }

  public String getName()
  {
    return m_name;
  }

  public void setName( final String name )
  {
    m_name = name;
  }

  public String getRepId()
  {
    return m_repId;
  }

  public void setRepId( String repId )
  {
    m_repId = repId;
  }
  
  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  public boolean equals( Object obj )
  {
    if( obj == null || !(obj instanceof ItemBean) )
      return false;
    
    final ItemBean other = (ItemBean)obj;
    
    return m_id.equals( other.getId() );
  }
}