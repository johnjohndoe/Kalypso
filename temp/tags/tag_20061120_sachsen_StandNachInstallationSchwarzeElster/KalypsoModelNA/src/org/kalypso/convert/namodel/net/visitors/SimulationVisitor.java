package org.kalypso.convert.namodel.net.visitors;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.kalypso.convert.namodel.net.NetElement;
import org.kalypso.convert.namodel.net.NetElementCircleFinder;

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

public class SimulationVisitor extends NetElementVisitor
{
  private final NetElementVisitor m_innerVisitor;

  private final List m_simulated;

  private final List m_cycleTest = new ArrayList();

  /*
   * 
   * @author doemming
   */
  public SimulationVisitor( NetElementVisitor innerVisitor )
  {
    m_innerVisitor = innerVisitor;
    m_simulated = new ArrayList();

  }

  /**
   * 
   * @throws Exception
   * @see org.kalypso.convert.namodel.net.visitors.NetElementVisitor#visit(org.kalypso.convert.namodel.net.NetElement)
   */
  public boolean visit( NetElement netElement ) throws Exception
  {
    if( m_simulated.contains( netElement ) )
      return false;
    // check cycle
    if( m_cycleTest.contains( netElement ) )
    {
      StringBuffer b = new StringBuffer( "Netzplan ist fehlerhaft - Wasser flieﬂt im Kreis\n" );
      System.out.println( "Netzplan ist fehlerhaft - Wasser flieﬂt im Kreis\n" + netElement );

      // check circle (shortest connection)
      //TODO: Better output handling, in this way it is not easy to find the circle
      final NetElementCircleFinder circlefinder = new NetElementCircleFinder( netElement );
      List[] circleList = circlefinder.findCircle();
      b.append( "circle for : " + netElement + ":\n" );

      for( int i = 0; i < circleList.length; i++ )
      {
        b.append( "StrangID: " + circleList[i] + "\n" );
      }
      log( b.toString() );
      throw new Exception( b.toString() );
    }
    
    m_cycleTest.add( netElement );

    // first calculate upstream

    final List upStreamNetElements = netElement.getUpStreamNetElements();
    for( Iterator iter = upStreamNetElements.iterator(); iter.hasNext(); )
    {
      NetElement element = (NetElement)iter.next();
      visit( element );
    }
    // then calculate current
    m_innerVisitor.visit( netElement );
    m_simulated.add( netElement );
    return true;
  }
}