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
package org.kalypso.convert.namodel.net.visitors;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.kalypso.convert.namodel.net.NetElement;
import org.kalypso.convert.namodel.net.NetElementCircleFinder;
import org.kalypso.model.hydrology.internal.i18n.Messages;

/**
 * @author doemming
 */
public class SimulationVisitor extends NetElementVisitor
{
  private final WriteAsciiVisitor m_elementWriter;

  private final List<NetElement> m_simulated = new ArrayList<NetElement>();

  private final Set<NetElement> m_cycleTest = new HashSet<NetElement>();

  public SimulationVisitor( final WriteAsciiVisitor elementWriter )
  {
    m_elementWriter = elementWriter;
  }

  /**
   * @throws Exception
   * @see org.kalypso.convert.namodel.net.visitors.NetElementVisitor#visit(org.kalypso.convert.namodel.net.NetElement)
   */
  @Override
  public boolean visit( final NetElement netElement ) throws Exception
  {
    if( m_simulated.contains( netElement ) )
      return false;

    // check cycle
    if( m_cycleTest.contains( netElement ) )
    {
      final String warning = Messages.getString( "org.kalypso.convert.namodel.net.visitors.SimulationVisitor.0" ); //$NON-NLS-1$
      final StringBuffer b = new StringBuffer( warning );
      System.out.println( warning + netElement ); //$NON-NLS-1$

      // check circle (shortest connection)
      // TODO: Better output handling, in this way it is not easy to find the circle
      final NetElementCircleFinder circlefinder = new NetElementCircleFinder( netElement );
      final List<NetElement>[] circleList = circlefinder.findCircle();
      b.append( Messages.getString( "org.kalypso.convert.namodel.net.visitors.SimulationVisitor.2", netElement ) + ":\n" ); //$NON-NLS-1$ //$NON-NLS-2$

      for( final List<NetElement> element : circleList )
      {
        b.append( Messages.getString( "org.kalypso.convert.namodel.net.visitors.SimulationVisitor.4", element ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
      }
      log( b.toString() );
      throw new Exception( b.toString() );
    }

    m_cycleTest.add( netElement );

    // first calculate upstream

    final List<NetElement> upStreamNetElements = netElement.getUpStreamNetElements();
    for( final NetElement netElement2 : upStreamNetElements )
    {
      final NetElement element = netElement2;
      visit( element );
    }

    // then calculate current
    m_elementWriter.visit( netElement );
    m_simulated.add( netElement );
    return true;
  }
}