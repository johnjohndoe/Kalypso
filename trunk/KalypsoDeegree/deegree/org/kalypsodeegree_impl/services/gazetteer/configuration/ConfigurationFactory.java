// $Header:
// /var/lib/cvs/backupdeegree/deegree/org/deegree_impl/services/gazetteer/configuration/ConfigurationFactory.java,v
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
package org.deegree_impl.services.gazetteer.configuration;

import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;

import org.deegree.xml.ElementList;
import org.deegree.xml.XMLParsingException;
import org.deegree.xml.XMLTools;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Element;

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
  private static String GNS = "http://www.deegree.org/gazetteer";

  /**
   * 
   * 
   * @param url
   * 
   * @return @throws
   *         XMLParsingException
   */
  public static GazetteerConfiguration createGazetteerConfiguration( URL url )
      throws XMLParsingException
  {
    Debug.debugMethodBegin();

    Element root = null;

    try
    {
      Reader reader = new InputStreamReader( url.openStream(), "UTF-8" );
      root = XMLTools.parse( reader ).getDocumentElement();
    }
    catch( Exception e )
    {
      throw new XMLParsingException( "could not parse gazetteer configuration", e );
    }

    Debug.debugMethodEnd();
    return createGazetteerConfiguration( root );
  }

  /**
   * 
   * 
   * @param root
   * 
   * @return @throws
   *         XMLParsingException
   */
  public static GazetteerConfiguration createGazetteerConfiguration( Element root )
      throws XMLParsingException
  {
    Debug.debugMethodBegin();

    Element element = XMLTools.getChildByName( "Gazetteer", GNS, root );
    String name = XMLTools.getRequiredStringValue( "Name", GNS, element );

    ElementList el = XMLTools.getChildElementsByName( "Relation", GNS, root );
    Relation[] relations = new Relation[el.getLength()];

    for( int i = 0; i < relations.length; i++ )
    {
      relations[i] = createRelation( el.item( i ) );
    }

    GazetteerConfiguration gc = new GazetteerConfiguration( name, relations );

    Debug.debugMethodEnd();
    return gc;
  }

  /**
   * 
   * 
   * @param element
   * 
   * @return @throws
   *         XMLParsingException
   */
  private static Relation createRelation( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    Element elem = XMLTools.getChildByName( "Source", GNS, element );
    String sourceLocationType = XMLTools.getRequiredStringValue( "LocationType", GNS, elem );
    String sourceProperty = XMLTools.getRequiredStringValue( "Property", GNS, elem );
    elem = XMLTools.getChildByName( "Target", GNS, element );
    String targetLocationType = XMLTools.getRequiredStringValue( "LocationType", GNS, elem );
    String targetProperty = XMLTools.getRequiredStringValue( "Property", GNS, elem );

    String operation = XMLTools.getRequiredStringValue( "Operation", GNS, element );
    Relation relation = new Relation( sourceLocationType, sourceProperty, targetLocationType,
        targetProperty, operation );

    Debug.debugMethodEnd();
    return relation;
  }
}
/*******************************************************************************
 * Changes to this class. What the people have been up to: $Log:
 * ConfigurationFactory.java,v $ Revision 1.1.1.1 2004/05/11 16:43:27 doemming
 * backup of local modified deegree sources
 * 
 * Revision 1.2 2004/03/26 11:19:29 poth no message
 * 
 * 
 *  
 ******************************************************************************/