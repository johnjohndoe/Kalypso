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

import java.io.Serializable;

import javax.persistence.Embeddable;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

/**
 * @author Dirk Kuch
 */
@Embeddable
public class KalypsoProjectBeanPrimaryKey implements Serializable
{
  private Integer m_projectVersion;

  private String m_unixName;

  /**
   * @return version number of this bean (each project has its own version numbers, starting from 0 (intial commit))
   */
  public Integer getProjectVersion( )
  {
    return m_projectVersion;
  }

  public void setProjectVersion( final Integer projectVersion )
  {
    m_projectVersion = projectVersion;
  }

  /**
   * @return unique unix name of this project. use this name to handle project / beans!
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
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals( final Object obj )
  {
    if( obj instanceof KalypsoProjectBeanPrimaryKey )
    {
      final KalypsoProjectBeanPrimaryKey foreign = (KalypsoProjectBeanPrimaryKey) obj;
      final EqualsBuilder builder = new EqualsBuilder();
      builder.append( getUnixName(), foreign.getUnixName() );
      builder.append( getProjectVersion(), foreign.getProjectVersion() );

      return builder.isEquals();
    }

    return super.equals( obj );
  }

}
