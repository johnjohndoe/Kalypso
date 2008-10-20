/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.project.database.sei.beans;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.kalypso.project.database.common.interfaces.IKalypsoProject;

/**
 * @author Dirk Kuch
 */

@Entity
@Table(name = "PROJECT")
public class KalypsoProjectBean implements IKalypsoProject
{
  // TODO FIXME map name and version as primary key!

  @Id
  @GeneratedValue
  private Integer m_id;

  @Column(name = "unixName")
  private String m_unixName;

  @Column(name = "projectName")
  private String m_name;

  @Column(name = "projectVersion")
  private Integer m_projectVersion;

  // TODO description

  @Transient
  KalypsoProjectBean[] m_children;

  public KalypsoProjectBean( )
  {
    // Needed in order to make this class a java bean
  }

  public KalypsoProjectBean( final String unixName, final String name, final Integer projectVersion )
  {
    m_unixName = unixName;
    m_name = name;
    m_projectVersion = projectVersion;
  }

  /**
   * @see org.kalypso.project.database.common.interfaces.IKalypsoProject#getName()
   */
  public String getName( )
  {
    return m_name;
  }

  public void setName( final String name )
  {
    m_name = name;
  }

  public String getUnixName( )
  {
    return m_unixName;
  }

  public void setUnixName( final String unixName )
  {
    m_unixName = unixName;
  }

  public Integer getProjectVersion( )
  {
    return m_projectVersion;
  }

  public void setProjectVersion( final Integer projectVersion )
  {
    m_projectVersion = projectVersion;
  }

  public void setChildren( final KalypsoProjectBean[] children )
  {
    m_children = children;
  }

  public KalypsoProjectBean[] getChildren( )
  {
    return m_children;
  }

  /**
   * @see org.kalypso.project.database.common.interfaces.IKalypsoProject#getUrl()
   */
  @Override
  public String getUrl( )
  {
    // TODO FIXME
    return null;
  }
}
