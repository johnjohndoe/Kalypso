/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.io.rtree;

import java.util.Hashtable;

/**
 * Implementierung eine PageFile im Speicher. Die PageFile ist als Hashtable realisiert, wobei die Schlüssel die
 * PageFileNummern darstellen.
 * 
 * @version 1.0
 * @author Wolfgang Bär
 */
public class MemoryPageFile extends PageFile
{
  private Hashtable file = new Hashtable();

  /**
   * Konstruktor MemoryPageFile.
   */
  public MemoryPageFile()
  {
    super();
    file.clear();
  }

  /**
   * @see PageFile#readNode(int)
   */
  public Node readNode( int pageFile ) //throws PageFileException
  {
    return (Node)file.get( new Integer( pageFile ) );
  }

  /**
   * @see PageFile#writeNode(Node)
   */
  public int writeNode( Node node )
  {
    int i = 0;

    if( node.getPageNumber() < 0 )
    {
      while( true )
      {
        if( !file.containsKey( new Integer( i ) ) )
        {
          break;
        }

        i++;
      }

      node.setPageNumber( i );
    }
    else
    {
      i = node.getPageNumber();
    }

    file.put( new Integer( i ), node );

    return i;
  }

  public Node deleteNode( int pageNumber )
  {
    return (Node)file.remove( new Integer( pageNumber ) );
  }

  /**
   * @see PageFile#close()
   */
  public void close()
  {
  // nothing to do
  }
}