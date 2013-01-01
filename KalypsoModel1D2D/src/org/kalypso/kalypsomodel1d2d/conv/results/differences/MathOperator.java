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
package org.kalypso.kalypsomodel1d2d.conv.results.differences;

import java.math.BigDecimal;

/**
 * @author Thomas Jung
 */
public enum MathOperator
{
  ePlus( "plus" )
  {
    @Override
    public BigDecimal getResult( final BigDecimal o1, final BigDecimal o2 )
    {
      return o1.add( o2 );
    }
  },

  eMinus( "minus" )
  {
    @Override
    public BigDecimal getResult( final BigDecimal o1, final BigDecimal o2 )
    {
      return o1.subtract( o2 );
    }
  };

  private final String m_label;

  private MathOperator( final String label )
  {
    m_label = label;
  }

  @Override
  public String toString( )
  {
    return m_label;
  }

  public abstract BigDecimal getResult( BigDecimal o1, BigDecimal o2 );
}
