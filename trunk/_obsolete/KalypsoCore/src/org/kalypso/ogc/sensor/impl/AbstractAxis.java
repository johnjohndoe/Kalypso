/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.ogc.sensor.impl;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;

/**
 * Provides the default implementation for equals() and hashCode() and toString().
 * 
 * @author schlienger
 */
public abstract class AbstractAxis implements IAxis
{
  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals( final Object obj )
  {
    if( !( obj instanceof IAxis ) )
      return false;

    if( this == obj )
      return true;

    final IAxis other = (IAxis)obj;
    final EqualsBuilder builder = new EqualsBuilder();
    builder.append( getDataClass(), other.getDataClass() ).append( isKey(), other.isKey() ).append( getType(),
        other.getType() ).append( getUnit(), other.getUnit() );

    // TRICK: hässlich, aber notwendig: der Label muss auch berücksichtigt werden wenn es sich um kalypso-status
    // Achsen handelt, sonst sind sie alle gleich.
    // Es ist sicherlich nicht schön dass plötzlich DefaultAxis von KalypsoStatusUtils abhängig ist, aber
    // so ist das Leben. Hier besteht ein großes Refaktoring Bedarf.
    if( KalypsoStatusUtils.isStatusAxis( this ) )
      builder.append( getName(), other.getName() );

    return builder.isEquals();
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode()
  {
    final HashCodeBuilder builder = new HashCodeBuilder( 27, 13 );
    builder.append( getDataClass() ).append( isKey() ).append( getType() ).append( getUnit() );

    // TRICK: hässlich, aber notwendig: der Label muss auch berücksichtigt werden wenn es sich um kalypso-status
    // Achsen handelt, sonst sind sie alle gleich.
    // Es ist sicherlich nicht schön dass plötzlich DefaultAxis von KalypsoStatusUtils abhängig ist, aber
    // so ist das Leben. Hier besteht ein großes Refaktoring Bedarf.
    if( KalypsoStatusUtils.isStatusAxis( this ) )
      builder.append( getName() );

    return builder.toHashCode();
  }
  
  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString()
  {
    if( getUnit().length() == 0 )
      return getName();

    return getName() + " - " + getUnit(); //$NON-NLS-1$
  }
}
