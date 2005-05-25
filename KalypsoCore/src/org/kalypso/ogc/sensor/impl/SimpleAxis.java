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

package org.kalypso.ogc.sensor.impl;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;


/**
 * SimpleAxis, compared to DefaultAxis, this class mutable.
 * 
 * @author doemming
 */
public class SimpleAxis implements IAxis
{
  private String m_type;

  private String m_name;

  private final boolean m_isKey;

  private boolean m_isPersistable;

  public SimpleAxis( IAxis axis )
  {
    m_type = axis.getType();
    m_name = axis.getName();
    m_isKey = axis.isKey();
    m_isPersistable = axis.isPersistable();
  }

  public String getName()
  {
    return m_name;
  }

  public void setName( String name )
  {
    m_name = name;
  }

  public boolean isKey()
  {
    return m_isKey;
  }

  public boolean isPersistable()
  {
    return m_isPersistable;
  }

  public Class getDataClass()
  {
    return TimeserieUtils.getDataClass( m_type );
  }

  public String getType()
  {
    return m_type;
  }

  public void setType( String type )
  {
    m_type = type;
  }

  public String getUnit()
  {
    return TimeserieUtils.getUnit( m_type );
  }

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  public boolean equals( Object obj )
  {
    if( !( obj instanceof IAxis ) )
      return false;

    final IAxis other = (IAxis)obj;

    if( getDataClass() == other.getDataClass() && isKey() == other.isKey()
        && getType().equals( other.getType() ) && getUnit().equals( other.getUnit() ) )
      return true;

    return false;
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  public int hashCode()
  {
    final StringBuffer bf = new StringBuffer();

    bf.append( getDataClass().getName() );
    bf.append( isKey() );
    bf.append( getType() );
    bf.append( getUnit() );

    return bf.toString().hashCode();
  }

}