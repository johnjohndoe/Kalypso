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
package org.kalypso.model.km.internal.core;

/**
 * Just represents a discharge value, used to sorting.
 */
class DummyKMValue extends AbstractKMValue
{
  private final double m_q;

  public DummyKMValue( final double q )
  {
    m_q = q;
  }

  @Override
  public double getLength( )
  {
    return 0;
  }

  @Override
  public double getAlpha( )
  {
    return 0;
  }

  @Override
  public double getK( )
  {
    return 0;
  }

  @Override
  public double getN( )
  {
    return 0;
  }

  @Override
  public double getKForeland( )
  {
    return 0;
  }

  @Override
  public double getNForeland( )
  {
    return 0;
  }

  @Override
  public double getQ( )
  {
    return 0;
  }

  @Override
  public double getQForeland( )
  {
    return 0;
  }

  @Override
  public double getQSum( )
  {
    return m_q;
  }

}