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
package org.kalypso.model.hydrology.binding.model.nodes.helper;

import java.util.Comparator;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * @author Dirk Kuch
 */
public class QqRelationRow
{
  public static final Comparator<QqRelationRow> COMPARATOR = new Comparator<QqRelationRow>()
  {
    @Override
    public int compare( final QqRelationRow o1, final QqRelationRow o2 )
    {
      return o1.getDischarge().compareTo( o2.getDischarge() );
    }
  };

  Double m_discharge;

  Double m_dischargeRetentionBasin;

  public QqRelationRow( final double discharge, final double dischargeRetentionBasin )
  {
    m_discharge = discharge;
    m_dischargeRetentionBasin = dischargeRetentionBasin;
  }

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals( final Object obj )
  {
    if( obj instanceof QqRelationRow )
    {
      final QqRelationRow other = (QqRelationRow) obj;

      final EqualsBuilder builder = new EqualsBuilder();
      builder.append( getDischarge(), other.getDischarge() );

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
    builder.append( getClass().getName() );
    builder.append( getDischarge() );

    return builder.toHashCode();
  }

  public Double getDischarge( )
  {
    return m_discharge;
  }

  public Double getDischargeRetentionBasin( )
  {
    return m_dischargeRetentionBasin;
  }

  public void setDischarge( final Double discharge )
  {
    m_discharge = discharge;
  }

  public void setDischargeRetentionBasin( final Double dischargeRetentionBasin )
  {
    m_dischargeRetentionBasin = dischargeRetentionBasin;
  }

}
