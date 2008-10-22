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
package org.kalypso.project.database.common.interfaces.implementation;

import org.kalypso.project.database.common.interfaces.IKalypsoProjectBeanSettings;

/**
 * @author kuch
 */
public class KalypsoProjectBeanSettings implements IKalypsoProjectBeanSettings
{
  private String m_unixName;

  private String m_projectName;

  private Integer m_projectVersion;

  private String m_projectType;

  private String m_description;

  public KalypsoProjectBeanSettings( )
  {

  }

  public KalypsoProjectBeanSettings( final String unixName, final String projectName, final String description, final Integer projectVersion, final String projectType )
  {
    m_unixName = unixName;
    m_projectName = projectName;
    m_description = description;
    m_projectVersion = projectVersion;
    m_projectType = projectType;
  }

  /**
   * internal project name, for handeling the project (no whitespace, special chars, etc)
   */
  public String getUnixName( )
  {
    return m_unixName;
  }

  public void setUnixName( final String unixName )
  {
    m_unixName = unixName;
  }

  /**
   * ui name
   */
  public String getName( )
  {
    return m_projectName;
  }

  public void setName( final String projectName )
  {
    m_projectName = projectName;
  }

  /**
   * version of this project (mapped by unixname)
   */
  public Integer getVersion( )
  {
    return m_projectVersion;
  }

  public void setVersion( final Integer version )
  {
    m_projectVersion = version;
  }

  /**
   * @return what kind of project
   */
  public String getProjectType( )
  {
    return m_projectType;
  }

  public void setProjectType( final String type )
  {
    m_projectType = type;
  }

  public String getDescription( )
  {
    return m_description;
  }

  public void setDescription( final String description )
  {
    m_description = description;
  }
}
