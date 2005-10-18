/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.convert.namodel.net;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.kalypso.contribs.java.util.Arrays;

/**
 * 
 * 
 * @author doemming
 */
public class NetElementCircleFinder
{

  private final NetElement m_testNetElement;

  /**
   *  
   */
  public NetElementCircleFinder( final NetElement testNetElement )
  {
    m_testNetElement = testNetElement;
  }

  public List[] findCircle()
  {
    return findCircle( m_testNetElement, new ArrayList() );
  }

  private List[] findCircle( NetElement netElement, final List listToHere )
  {
    final List result = new ArrayList();
    listToHere.add( netElement );
    final List downStreamNetElements = netElement.getDownStreamNetElements();
    final NetElement[] downStreamElements = (NetElement[])downStreamNetElements
        .toArray( new NetElement[downStreamNetElements.size()] );
    for( int i = 0; i < downStreamElements.length; i++ )
    {
      final List copyOfListToHere = new ArrayList();
      // make copy
      for( Iterator iter = listToHere.iterator(); iter.hasNext(); )
        copyOfListToHere.add( iter.next() );

      final NetElement linkNetElement = downStreamElements[i];
      if( linkNetElement == m_testNetElement )
      {
        copyOfListToHere.add( linkNetElement );
        result.add( copyOfListToHere );
      }
      else if( listToHere.contains( linkNetElement ) )
      {
        // an other circle
        System.out.println( "found an other circle:\n" + listToHere.toString() + " : " + linkNetElement );
      }
      else
      {
        final List[] lists = findCircle( linkNetElement, copyOfListToHere);
        if(lists.length>0)
        result.addAll( Arrays.asList( lists ) );
      }
    }
    return (List[])result.toArray( new List[result.size()] );
  }

}
