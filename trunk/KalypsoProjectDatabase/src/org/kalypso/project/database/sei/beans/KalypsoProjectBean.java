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
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.IdClass;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.kalypso.project.database.IProjectDataBaseServerConstant;
import org.kalypso.project.database.common.utils.ProjectModelUrlResolver;

/**
 * @author Dirk Kuch
 */

@Entity
@Table(name = "PROJECT")
@IdClass(KalypsoProjectBeanPrimaryKey.class)
public class KalypsoProjectBean implements Comparable<KalypsoProjectBean>
{
  // TODO FIXME map name and version as primary key!

  @Transient
  KalypsoProjectBean[] m_children;

  @Column(name = "project_name")
  private String m_name;

  @Column(name = "project_type", updatable = false, nullable = false)
  private String m_projectType;

  @Id
  @Column(name = "project_version")
  private Integer m_projectVersion;

  @Id
  @Column(name = "unix_name", updatable = false, nullable = false)
  private String m_unixName;

  @Column(name = "project_description")
  private String m_description;

  @Column(name = "creation_date", updatable = false, nullable = false)
  private Date m_creationDate;

  @Column(name = "edit_lock_ticket")
  private String m_editLockTicket;

  @Column(name = "project_changes")
  private String m_changes;

  /**
   * @return previous versions of this bean
   */
  @Transient
  public KalypsoProjectBean[] getChildren( )
  {
    return m_children;
  }

  /**
   * @param editLock
   *          set or release edit lock of project final
   */
  public void setEditLockTicket( final String editTicket )
  {
    m_editLockTicket = editTicket;
  }

  /**
   * @return is project locked for editing?
   */
  public String getEditLockTicket( )
  {
    return m_editLockTicket;
  }

  public Boolean isProjectLockedForEditing( )
  {
    final String ticket = getEditLockTicket();
    if( ticket == null || "".equals( ticket.trim() ) )
      return false;

    return true;
  }

  /**
   * @return label name of this bean
   */
  public String getName( )
  {
    return m_name;
  }

  /**
   * @return type of project
   */
  public String getProjectType( )
  {
    return m_projectType;
  }

  /**
   * @return version number of this bean (each project has its own version numbers, starting from 0 (intial commit))
   */
  public Integer getProjectVersion( )
  {
    return m_projectVersion;
  }

  /**
   * @return unique unix name of this project. use this name to handle project / beans!
   */
  public String getUnixName( )
  {
    return m_unixName;
  }

  /**
   * @return public url (http at the moment) of project data (zipped project)
   */
  @Transient
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

  /**
   * @param children
   *          previous version of this project
   */
  public void setChildren( final KalypsoProjectBean[] children )
  {
    m_children = children;
  }

  /**
   * @param name
   *          label name
   */
  public void setName( final String name )
  {
    m_name = name;
  }

  /**
   * @param projectType
   *          project model data project type
   */
  public void setProjectType( final String projectType )
  {
    m_projectType = projectType;
  }

  /**
   * @param projectVersion
   *          project version of this bean ( projectXyz[0,1,... n] )
   */
  public void setProjectVersion( final Integer projectVersion )
  {
    m_projectVersion = projectVersion;
  }

  /**
   * @param unixName
   *          unique project (unix) name
   */
  public void setUnixName( final String unixName )
  {
    m_unixName = unixName;
  }

  /**
   * @return description of project (can differ with each version of project!)
   */
  public String getDescription( )
  {
    return m_description;
  }

  /**
   * @param description
   *          description of project
   */
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

      final EqualsBuilder builder = new EqualsBuilder();
      builder.append( getUnixName(), other.getUnixName() );
      builder.append( getProjectVersion(), other.getProjectVersion() );

      return builder.isEquals();
    }

    return super.equals( obj );
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode( )
  {
    final HashCodeBuilder builder = new HashCodeBuilder();
    builder.append( getUnixName() );
    builder.append( getProjectVersion() );

    return builder.toHashCode();
  }

  /**
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  @Override
  public int compareTo( final KalypsoProjectBean o )
  {
    return getUnixName().compareTo( o.getUnixName() );
  }

  /**
   * @return date the project (here version!) was created
   */
  public Date getCreationDate( )
  {
    return m_creationDate;
  }

  /**
   * @param creationDate
   *          date of creation. will be automatically set by {@link org.kalypso.project.database.server.ProjectDatabase}
   *          .createProject()
   */
  public void setCreationDate( final Date creationDate )
  {
    m_creationDate = creationDate;
  }

  public void setChanges( final String changes )
  {
    m_changes = changes;
  }

  public String getChanges( )
  {
    return m_changes;
  }
}
