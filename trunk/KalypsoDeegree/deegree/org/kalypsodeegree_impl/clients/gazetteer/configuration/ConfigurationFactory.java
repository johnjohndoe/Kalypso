// $Header:
// /var/lib/cvs/backupdeegree/deegree/org/deegree_impl/clients/gazetteer/configuration/ConfigurationFactory.java,v
// 1.1.1.1 2004/05/11 16:43:27 doemming Exp $

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
package org.deegree_impl.clients.gazetteer.configuration;

import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;

import org.deegree.xml.ElementList;
import org.deegree.xml.XMLTools;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author last edited by: $Author$
 * 
 * @version 1.0. $Revision$, $Date$
 * 
 * @since 1.1
 */
public class ConfigurationFactory
{
  private static final String GZNS = "http://www.deegree.org/gazetteerclient";

  /**
   * 
   * 
   * @param confFile
   * 
   * @return @throws
   *         SAXException
   * @throws IOException
   * @throws Exception
   */
  public static GazetteerClientConfiguration createConfiguration( String confFile )
      throws SAXException, IOException, Exception
  {
    Debug.debugMethodBegin();

    Reader reader = new FileReader( confFile );
    GazetteerClientConfiguration conf = createConfiguration( reader );
    reader.close();
    Debug.debugMethodEnd();
    return conf;
  }

  /**
   * 
   * 
   * @param reader
   * 
   * @return @throws
   *         SAXException
   * @throws IOException
   * @throws Exception
   */
  public static GazetteerClientConfiguration createConfiguration( Reader reader )
      throws SAXException, IOException, Exception
  {
    Debug.debugMethodBegin();

    Document doc = XMLTools.parse( reader );

    // gazetteer descriptions
    Element element = doc.getDocumentElement();
    ElementList el = XMLTools.getChildElementsByName( "gazetteer", GZNS, element );
    HashMap gaze = createGazetteerDesc( el );

    GazetteerClientConfiguration conf = new GazetteerClientConfiguration( gaze );

    Debug.debugMethodEnd();
    return conf;
  }

  /**
   * creates a map of thesauri names and associated addresses
   */
  private static HashMap createGazetteerDesc( ElementList nl ) throws MalformedURLException
  {
    Debug.debugMethodBegin();

    HashMap thes = new HashMap();

    for( int i = 0; i < nl.getLength(); i++ )
    {
      Element element = (Element)nl.item( i );
      Node node = element.getElementsByTagNameNS( GZNS, "name" ).item( 0 );
      String name = node.getFirstChild().getNodeValue();
      node = element.getElementsByTagNameNS( GZNS, "onlineResource" ).item( 0 );

      String tmp = XMLTools.getStringValue( node );
      thes.put( name, new URL( tmp ) );
    }

    Debug.debugMethodEnd();
    return thes;
  }

}
/*******************************************************************************
 * Changes to this class. What the people have been up to: $Log:
 * ConfigurationFactory.java,v $ Revision 1.1.1.1 2004/05/11 16:43:27 doemming
 * backup of local modified deegree sources
 * 
 * Revision 1.2 2004/03/24 12:36:22 poth no message
 * 
 * Revision 1.1 2004/03/15 07:38:05 poth no message
 * 
 * 
 *  
 ******************************************************************************/