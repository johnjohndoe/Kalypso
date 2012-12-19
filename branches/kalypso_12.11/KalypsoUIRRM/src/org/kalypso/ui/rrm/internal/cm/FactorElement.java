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
package org.kalypso.ui.rrm.internal.cm;

/**
 * A factor element.
 * 
 * @author Holger Albert
 */
public class FactorElement implements Comparable<FactorElement>
{
  /**
   * The factor.
   */
  private int m_factor;

  /**
   * The index of the original order.
   */
  private final int m_index;

  /**
   * The constructor.
   * 
   * @param factor
   *          The factor.
   * @param index
   *          The index of the original order.
   */
  public FactorElement( final int factor, final int index )
  {
    m_factor = factor;
    m_index = index;
  }

  /**
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  @Override
  public int compareTo( final FactorElement o )
  {
    return Integer.compare( m_index, o.getIndex() );
  }

  /**
   * This function returns the factor.
   * 
   * @return The factor.
   */
  public int getFactor( )
  {
    return m_factor;
  }

  /**
   * This function returns the index of the original order.
   * 
   * @return The index of the original order.
   */
  public int getIndex( )
  {
    return m_index;
  }

  /**
   * This function sets the factor.
   * 
   * @param factor
   *          The factor.
   */
  public void setFactor( final int factor )
  {
    m_factor = factor;
  }
}