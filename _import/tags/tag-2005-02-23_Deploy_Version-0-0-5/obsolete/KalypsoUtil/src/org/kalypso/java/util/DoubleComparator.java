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
package org.kalypso.java.util;

import java.util.Comparator;


/**
 * @author belger
 */
public class DoubleComparator implements Comparator
{
  private double m_delta = 0.0;

  public DoubleComparator( double delta )
  {
    m_delta = delta;
  }

  /**
   * @see java.util.Comparator#compare(Object, Object)
   */
  public int compare( Object o1, Object o2 )
  {
    double d1 = ( (Number)o1 ).doubleValue(  );
    double d2 = ( (Number)o2 ).doubleValue(  );

    return compare( d1, d2 );
  }

  public int compare( double d1, double d2 )
  {
    if( d1 < d2 - m_delta )
      return -1;
    else if( d1 > d2 + m_delta )
      return 1;
    else

      return 0;
  }
}
