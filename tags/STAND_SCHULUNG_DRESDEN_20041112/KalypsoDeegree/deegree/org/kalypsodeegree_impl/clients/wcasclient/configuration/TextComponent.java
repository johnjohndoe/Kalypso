/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

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

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.clients.wcasclient.configuration;

import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;

import org.deegree.xml.XMLParsingException;
import org.deegree.xml.XMLTools;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.StringExtend;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

/**
 * 
 * @author Andreas Poth
 * @version 11.3.2003
 */
public class TextComponent
{
  private static String LATLONNS = "http://www.lat-lon.de";

  private Document doc = null;

  /** Creates a new instance of TextComponent */
  public TextComponent( String urlName ) throws Exception
  {
    URL url = new URL( urlName );
    Reader reader = new InputStreamReader( url.openStream() );
    doc = XMLTools.parse( reader );
  }

  /**
   * returns the message specified by the submitted path expression (e.g.
   * text/download/successPattern )
   */
  public String getMessage( String path ) throws XMLParsingException
  {
    Debug.debugMethodBegin( this, "getMessage" );

    String[] nodes = StringExtend.toArray( path, "/", false );
    Node node = doc;

    for( int i = 0; i < nodes.length; i++ )
    {
      node = XMLTools.getRequiredChildByName( nodes[i], LATLONNS, node );
    }

    Debug.debugMethodEnd();
    return XMLTools.getStringValue( node );
  }
}