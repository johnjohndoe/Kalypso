/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.convert.namodel.net.visitors;

import java.util.List;

import org.kalypso.convert.namodel.net.NetElement;

/**
 * @author doemming
 */
public class UpStreamVisitor extends NetElementVisitor
{
  private final NetElementVisitor m_innerVisitor;

  public UpStreamVisitor( final NetElementVisitor innerVisitor )
  {
    m_innerVisitor = innerVisitor;
  }

  /**
   * @throws Exception
   * @see org.kalypso.convert.namodel.net.visitors.NetElementVisitor#visit(org.kalypso.convert.namodel.net.NetElement)
   */
  @Override
  public boolean visit( final NetElement netElement ) throws Exception
  {
    if( m_innerVisitor.visit( netElement ) )
    {
      final List<NetElement> upStreamNetElements = netElement.getUpStreamNetElements();
      for( final NetElement netElement2 : upStreamNetElements )
      {
        final NetElement element = netElement2;
        visit( element );
      }
    }
    return false;
  }
}