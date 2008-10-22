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
package org.kalypso.project.database.sei.beans;

import java.net.MalformedURLException;
import java.net.URL;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.kalypso.project.database.IProjectDataBaseServerConstant;
import org.kalypso.project.database.common.interfaces.IKalypsoProject;
import org.kalypso.project.database.common.interfaces.implementation.KalypsoProjectBeanSettings;
import org.kalypso.project.database.common.utils.ProjectModelUrlResolver;

/**
 * @author Dirk Kuch
 */

@Entity
@Table(name = "PROJECT")
public class KalypsoProjectBean implements IKalypsoProject
{
  // TODO FIXME map name and version as primary key!

  @Transient
  KalypsoProjectBean[] m_children;

  @Id
  @GeneratedValue
  private Integer m_id;

  @Column(name = "projectName")
  private String m_name;

  @Column(name = "projectType")
  private String m_projectType;

  @Column(name = "projectVersion")
  private Integer m_projectVersion;

  @Column(name = "unixName")
  private String m_unixName;

  @Column(name = "projectDescription")
  private String m_description;

  public KalypsoProjectBean( )
  {
    // Needed in order to make this class a java bean
  }

  /**
   * @param delegate
   *          *narf* interface won't work, so we have a duplicated settings class. makes extension of settings (and
   *          handling of calling functions) easier
   */
  public KalypsoProjectBean( final KalypsoProjectBeanSettings delegate )
  {
    m_unixName = delegate.getUnixName();
    m_name = delegate.getName();
    m_projectVersion = delegate.getVersion();
    m_projectType = delegate.getProjectType();
    m_description = delegate.getDescription();
  }

  public KalypsoProjectBean[] getChildren( )
  {
    return m_children;
  }

  /**
   * @see org.kalypso.project.database.common.interfaces.IKalypsoProject#getName()
   */
  public String getName( )
  {
    return m_name;
  }

  public String getProjectType( )
  {
    return m_projectType;
  }

  public Integer getProjectVersion( )
  {
    return m_projectVersion;
  }

  public String getUnixName( )
  {
    return m_unixName;
  }

  /**
   * @see org.kalypso.project.database.common.interfaces.IKalypsoProject#getUrl()
   */
  @Override
  public URL getUrl( ) throws MalformedURLException
  {
    /* destination of incoming file */
    final URL url = ProjectModelUrlResolver.getUrlAsHttp( new ProjectModelUrlResolver.IResolverInterface()
    {
      @Override
      public String getPath( )
      {
        return System.getProperty( IProjectDataBaseServerConstant.SERVER_READABLE_PATH );
      }
    }, String.format( "%s/%d/project.zip", getUnixName(), getProjectVersion() ) );

    return url;
  }

  public void setChildren( final KalypsoProjectBean[] children )
  {
    m_children = children;
  }

  public void setName( final String name )
  {
    m_name = name;
  }

  public void setProjectType( final String projectType )
  {
    m_projectType = projectType;
  }

  public void setProjectVersion( final Integer projectVersion )
  {
    m_projectVersion = projectVersion;
  }

  public void setUnixName( final String unixName )
  {
    m_unixName = unixName;
  }

  public String getDescription( )
  {
    return m_description;
  }

  public void setDescription( final String description )
  {
    m_description = description;
  }

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals( final Object obj )
  {
    if( obj instanceof KalypsoProjectBean )
    {
      final KalypsoProjectBean other = (KalypsoProjectBean) obj;

      return getUnixName().equals( other.getUnixName() );
    }

    return super.equals( obj );
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode( )
  {
    return getUnixName().hashCode();

  }

  /**
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  @Override
  public int compareTo( final KalypsoProjectBean o )
  {
    return getName().compareTo( o.getName() );
  }
}
