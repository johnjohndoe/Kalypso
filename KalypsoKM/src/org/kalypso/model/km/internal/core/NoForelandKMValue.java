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
 * @author Gernot Belger
 */
public class NoForelandKMValue extends AbstractKMValue
{
  private final IKMValue m_delegate;

  public NoForelandKMValue( final IKMValue delegate )
  {
    m_delegate = delegate;
  }

  @Override
  public double getLength( )
  {
    return m_delegate.getLength();
  }

  @Override
  public double getAlpha( )
  {
    return 1.0;
  }

  @Override
  public double getK( )
  {
    return m_delegate.getK();
  }

  @Override
  public double getN( )
  {
    return m_delegate.getN();
  }

  @Override
  public double getKForeland( )
  {
    return 0.0;
  }

  @Override
  public double getNForeland( )
  {
    return 0.0;
  }

  @Override
  public double getLowerQchannel( )
  {
    return m_delegate.getLowerQ();
  }

  @Override
  public double getUpperQchannel( )
  {
    return m_delegate.getUpperQ();
  }

  @Override
  public double getLowerQforeland( )
  {
    return 0.0;
  }

  @Override
  public double getUpperQforeland( )
  {
    return 0.0;
  }
}