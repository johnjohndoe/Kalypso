/*----------------    FILE HEADER  ------------------------------------------
 
 This file is part of deegree (Java Framework for Geospatial Solutions).
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
package org.deegree_impl.services.wcs.capabilities;

import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;

import org.deegree.model.coverage.CoverageLayer;
import org.deegree.services.capabilities.Service;
import org.deegree.services.wcs.capabilities.WCSCapabilities;
import org.deegree.xml.XMLTools;
import org.deegree_impl.model.cv.CVDescriptorFactory;
import org.deegree_impl.services.capabilities.Service_Impl;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.StringExtend;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

/**
 * 
 * @author Administrator
 */
public class WCSCapabilitiesFactory
{

  private static String NSWCS = "http://www.opengis.net/wcs";

  /**
   * factory method for creating a <tt>WCSCapabilities</tt> object from a file
   * that contains a OGC WCS 0.7 conform XML capabilities document
   */
  public static synchronized WCSCapabilities createCapabilities( URL url ) throws IOException,
      SAXException, Exception
  {
    Debug.debugMethodBegin( "WCSCapabilitiesFactory", "createCapabilities" );

    InputStreamReader isr = new InputStreamReader( url.openStream() );
    Document doc = XMLTools.parse( isr );
    WCSCapabilities capabilities = createCapabilities( doc );

    Debug.debugMethodEnd();

    return capabilities;
  }

  /**
   * factory method for creating a <tt>WCSCapabilities</tt> object from a OGC
   * WCS 0.7 conform XML capabilities document
   */
  public static synchronized WCSCapabilities createCapabilities( Document doc ) throws Exception
  {
    Debug.debugMethodBegin( "WCSCapabilitiesFactory", "createCapabilities" );

    Element root = doc.getDocumentElement();

    //get general service informations
    String version = XMLTools.getAttrValue( root, "version" );
    String updateSequence = XMLTools.getAttrValue( root, "updateSequence" );

    // get service section
    Element element = (Element)root.getElementsByTagName( "Service" ).item( 0 );
    Service service = createService( element );

    // get coverage layer list
    element = (Element)root.getElementsByTagNameNS( NSWCS, "CoverageLayerList" ).item( 0 );
    CoverageLayer[] cv = CVDescriptorFactory.createCoverageLayer( element );

    // create capabilities object
    WCSCapabilities capabilities = new WCSCapabilities_Impl( version, updateSequence, service, cv );

    Debug.debugMethodEnd();

    return capabilities;
  }

  /**
   * returns an instance of an object that capsulates the service element of the
   * WFS capabilities.
   */
  private static Service createService( Element serviceElement )
  {
    Debug.debugMethodBegin( "WCSCapabilitiesFactory", "createService" );

    // get service name
    Element element = XMLTools.getNamedChild( serviceElement, "Name" );
    String name = element.getFirstChild().getNodeValue();

    // get service title
    element = XMLTools.getNamedChild( serviceElement, "Title" );
    String title = element.getFirstChild().getNodeValue();

    // get service abstract
    element = XMLTools.getNamedChild( serviceElement, "Abstract" );
    String abstract_ = "";
    if( element != null )
    {
      abstract_ = element.getFirstChild().getNodeValue();
    }

    // get service keywords
    element = XMLTools.getNamedChild( serviceElement, "Keywords" );
    String[] keywords = null;
    if( element != null )
    {
      keywords = createKeywords( element );
    }

    // get service online resource
    element = XMLTools.getNamedChild( serviceElement, "OnlineResource" );
    URL onlineResource = null;
    try
    {
      onlineResource = new URL( element.getFirstChild().getNodeValue() );
    }
    catch( Exception ex )
    {
      System.out.println( "getService: " + ex );
    }

    // get service fees
    element = XMLTools.getNamedChild( serviceElement, "Fees" );
    String fees = null;
    if( element != null )
    {
      fees = element.getFirstChild().getNodeValue();
    }

    // get service access constraints
    element = XMLTools.getNamedChild( serviceElement, "AccessConstraints" );
    String accessConstraints = null;
    if( element != null )
    {
      accessConstraints = element.getFirstChild().getNodeValue();
    }

    Service service = new Service_Impl( name, title, abstract_, keywords, onlineResource, null,
        fees, accessConstraints );

    Debug.debugMethodEnd();
    return service;
  }

  /**
   * returns the keywords associated with the service
   */
  private static String[] createKeywords( Element keywordsElement )
  {
    Debug.debugMethodBegin( "WCSCapabilitiesFactory", "createKeywords" );

    String[] kw = null;

    Node node = keywordsElement.getFirstChild();
    if( node != null )
    {
      String keywords = node.getNodeValue();
      if( keywords != null )
      {
        kw = StringExtend.toArray( keywords, ",;", true );
      }
    }

    Debug.debugMethodEnd();
    return kw;
  }

}