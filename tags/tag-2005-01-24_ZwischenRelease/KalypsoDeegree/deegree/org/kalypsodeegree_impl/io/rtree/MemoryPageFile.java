/*----------------    FILE HEADER  ------------------------------------------

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

 Copyright (C) 2002 Wolfgang Baer - WBaer@gmx.de
 
 Adapted May 2003 by IDgis, The Netherlands - www.idgis.nl
 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.io.rtree;

import java.util.Hashtable;

/**
 * Implementierung eine PageFile im Speicher. Die PageFile ist als Hashtable
 * realisiert, wobei die Schlüssel die PageFileNummern darstellen.
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
  public int writeNode( Node node ) throws PageFileException
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

  /**
   * 
   * 
   * @param pageNumber
   * 
   * @return
   */
  public Node deleteNode( int pageNumber )
  {
    return (Node)file.remove( new Integer( pageNumber ) );
  }

  /**
   * @see PageFile#close()
   */
  public void close() throws PageFileException
  {
  // nothing to do
  }
}