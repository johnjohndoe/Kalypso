package org.kalypso.java.xml;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

/**
 * @author doemming
 */
public class XMLUtilities
{
  public static String removeXMLHeader( String xmlString )
  {
    return xmlString.replaceFirst( "<\\?.+?\\?>", "" );
  }

  /**
   * TODO Andreas, diese Methode wird scheinbar nur von
   * WQFilterUtilities.createWQFilterInline() und
   * UpdateHelper.createInterpolationFilter() aufgerufen, und beinhaltet
   * ausserdem 'filter'-spezifische Tags. Sollte es nicht besser in
   * WQFilterUtilities verschoben werden?
   * 
   * @param xmlString
   * @return
   */
  public static String prepareInLine( String xmlString )
  {
    String result = xmlString.replaceAll( "\n", "" );
    result = removeXMLHeader( result );
    return "<filter>" + result + "</filter>";
  }

  public static void setTextNode( Document dom, Node node, String value )
  {
    NodeList cn = node.getChildNodes();
    for( int _n = 0; _n < cn.getLength(); _n++ )
    {
      Node cnode = cn.item( _n );
      short nodeType = cnode.getNodeType();
      if( nodeType == Node.TEXT_NODE )
        cnode.setNodeValue( value );
    }
    if( cn.getLength() == 0 ) // text node does not exist
    {
      Text text = dom.createTextNode( value );
      node.appendChild( text );
    }
  
  }

}
