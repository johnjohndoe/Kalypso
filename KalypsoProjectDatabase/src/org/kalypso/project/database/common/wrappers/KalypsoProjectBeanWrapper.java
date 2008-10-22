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
package org.kalypso.project.database.common.wrappers;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.core.runtime.Assert;
import org.kalypso.project.database.common.interfaces.IKalypsoProject;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

/**
 * Handles client / server interactions of KalypsoProjectBeans
 * 
 * @author Dirk Kuch
 */
// TODO perhaps extends from KalypsoProjectBean
public class KalypsoProjectBeanWrapper implements IKalypsoProject
{
  private final KalypsoProjectBean m_bean;

  public KalypsoProjectBeanWrapper( final KalypsoProjectBean bean )
  {
    Assert.isNotNull( bean );

    m_bean = bean;
  }

  /**
   * @see org.kalypso.project.database.common.interfaces.IKalypsoProject#getChildren()
   */
  @Override
  public KalypsoProjectBean[] getChildren( )
  {
    return m_bean.getChildren();
  }

  /**
   * @see org.kalypso.project.database.common.interfaces.IKalypsoProject#getName()
   */
  @Override
  public String getName( )
  {
    return m_bean.getName();
  }

  /**
   * @see org.kalypso.project.database.common.interfaces.IKalypsoProject#getProjectType()
   */
  @Override
  public String getProjectType( )
  {
    return m_bean.getProjectType();
  }

  /**
   * @see org.kalypso.project.database.common.interfaces.IKalypsoProject#getProjectVersion()
   */
  @Override
  public Integer getProjectVersion( )
  {
    return m_bean.getProjectVersion();
  }

  /**
   * @see org.kalypso.project.database.common.interfaces.IKalypsoProject#getUnixName()
   */
  @Override
  public String getUnixName( )
  {
    return m_bean.getUnixName();
  }

  /**
   * @see org.kalypso.project.database.common.interfaces.IKalypsoProject#getUrl()
   */
  @Override
  public URL getUrl( ) throws MalformedURLException
  {
    return m_bean.getUrl();
  }

  /**
   * @see org.kalypso.project.database.common.interfaces.IKalypsoProject#setChildren(org.kalypso.project.database.sei.beans.KalypsoProjectBean[])
   */
  @Override
  public void setChildren( final KalypsoProjectBean[] children )
  {
    m_bean.setChildren( children );
  }

  /**
   * @see org.kalypso.project.database.common.interfaces.IKalypsoProject#setName(java.lang.String)
   */
  @Override
  public void setName( final String name )
  {
    m_bean.setName( name );
  }

  /**
   * @see org.kalypso.project.database.common.interfaces.IKalypsoProject#setProjectType(java.lang.String)
   */
  @Override
  public void setProjectType( final String projectType )
  {
    m_bean.setProjectType( projectType );
  }

  /**
   * @see org.kalypso.project.database.common.interfaces.IKalypsoProject#setProjectVersion(java.lang.Integer)
   */
  @Override
  public void setProjectVersion( final Integer projectVersion )
  {
    m_bean.setProjectVersion( projectVersion );
  }

  /**
   * @see org.kalypso.project.database.common.interfaces.IKalypsoProject#setUnixName(java.lang.String)
   */
  @Override
  public void setUnixName( final String unixName )
  {
    m_bean.setUnixName( unixName );
  }

  /**
   * @see org.kalypso.project.database.common.interfaces.IKalypsoProject#getDescription()
   */
  @Override
  public String getDescription( )
  {
    return m_bean.getDescription();
  }

  /**
   * @see org.kalypso.project.database.common.interfaces.IKalypsoProject#setDescription(java.lang.String)
   */
  @Override
  public void setDescription( final String description )
  {
    m_bean.setDescription( description );

  }
}
