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
package org.deegree_impl.gml;

import java.io.*;

import java.net.*;

import java.util.*;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.deegree.gml.*;
import org.deegree.xml.*;

import org.deegree_impl.tools.*;

import org.w3c.dom.*;

import org.xml.sax.*;

/**
 * 
 * 
 * <p>
 * ----------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 07.02.2001
 *          <p>
 */
public class GMLDocument_Impl implements GMLDocument
{
  private org.w3c.dom.Document document = null;
      private List myNS=new ArrayList();

  /**
   * Creates a new GMLDocument_Impl object.
   */
  public GMLDocument_Impl()
  {
    Debug.debugMethodBegin( this, "GMLDocument_Impl()" );
    DocumentBuilder parser = null;

    try
    {
      parser = DocumentBuilderFactory.newInstance().newDocumentBuilder();
    }
    catch( ParserConfigurationException ex )
    {
      ex.printStackTrace();
    }

    document = parser.newDocument();

    Debug.debugMethodEnd();
  }

  /**
   * Creates a new GMLDocument_Impl object.
   * 
   * @param reader
   * 
   * @throws IOException
   * @throws SAXException
   */
  public GMLDocument_Impl( Reader reader ) throws IOException, SAXException
  {
    Debug.debugMethodBegin( this, "GMLDocument_Impl(Reader)" );
    document = XMLTools.parse( reader );
    Debug.debugMethodEnd();
  }

  /**
   * Creates a new GMLDocument_Impl object.
   * 
   * @param document
   */
  public GMLDocument_Impl( Document document )
  {
    setDocument( document );
  }

  /**
   * returns the dom document representing the GML document
   */
  public Document getDocument()
  {
    return document;
  }

  /**
   * @see org.deegree_impl.gml.GMLDocument_Impl#getDocument()
   */
  public void setDocument( Document document )
  {
    this.document = document;
  }

  /**
   * returns the location of the schema the document based on
   */
  public URL getSchemaLocation() throws MalformedURLException
  {
    Debug.debugMethodBegin( this, "getSchemaLocation" );

    String schemaL = XMLTools.getAttrValue( document.getDocumentElement(), "xsi:schemaLocation" );
    Debug.debugMethodEnd();
    return new URL( schemaL );
  }

  /**
   * sets the location of schema the document based on
   */
  public void setSchemaLocation( URL schema )
  {
    Debug.debugMethodBegin( this, "setSchemaLocation" );

    Element root = document.getDocumentElement();
    root.setAttribute( "xsi:schemaLocation", schema.toString() );
    Debug.debugMethodEnd();
  }

  /**
   * returns the name spaces used within the document
   */
  public GMLNameSpace[] getNameSpaces()
  {
    Debug.debugMethodBegin( this, "getNameSpace" );

    ArrayList list = new ArrayList();
    Element root = document.getDocumentElement();
    if( root != null )
    {
      NamedNodeMap nnm = root.getAttributes();

      for( int i = 0; i < nnm.getLength(); i++ )
      {
        if( nnm.item( i ).getNodeValue().indexOf( "xmlns" ) >= 0 )
        {
          GMLNameSpace gns = new GMLNameSpace_Impl( nnm.item( i ).getNodeName() + "="
              + nnm.item( i ).getNodeValue() );
          list.add( gns );
        }
      }
    }
    Debug.debugMethodEnd();

    return (GMLNameSpace[])list.toArray( new GMLNameSpace[list.size()] );
  }

  /**
   * helper method to get namespace key from qualified namespace example: ns=
   * xmlns:test="www.test.org" "test" = getNameSpaceKey("www.test.org")
   */
  public String getNameSpaceKey( String qualifiedNS )
  {
    GMLNameSpace[] nss = getNameSpaces();
    for( int i = 0; i < nss.length; i++ )
      if( qualifiedNS.equals( nss[i].getNameSpaceName() ) )
        return nss[i].getNameSpaceValue();
    return null;
  }

  /**
   * @see org.deegree_impl.gml.GMLDocument_Impl#getNameSpaces()
   */
  public void addNameSpace( GMLNameSpace nameSpace )
  {
    Debug.debugMethodBegin();
    if(!myNS.contains(nameSpace))
      myNS.add(nameSpace);
    Element root = document.getDocumentElement();

    if( nameSpace.getSubSpaceName() != null )
    {
      root.setAttribute( nameSpace.getNameSpaceName() + ":" + nameSpace.getSubSpaceName(),
          nameSpace.getNameSpaceValue() );
    }
    else
    {
      root.setAttribute( nameSpace.getNameSpaceName(), nameSpace.getNameSpaceValue() );
    }

    Debug.debugMethodEnd();
  }

  /**
   * returns the root element of the document as GMLFeatureCollection.
   */
  public GMLFeatureCollection getRoot()
  {
    return new GMLFeatureCollection_Impl( document.getDocumentElement() );
  }

  /**
   * @see org.deegree_impl.gml.GMLDocument_Impl#getRoot()
   */
  public void setRoot( GMLFeatureCollection root )
  {
    Debug.debugMethodBegin();
   
    Node node = document.getDocumentElement();

    // remove root node if it already exists
    if( node != null )
    {
      document.removeChild( node );
    }

    XMLTools.insertNodeInto( ( (GMLFeatureCollection_Impl)root ).getAsElement(), document );
    for( int i = 0; i < myNS.size(); i++ )
      addNameSpace( (GMLNameSpace)myNS.get(i) );
    Debug.debugMethodEnd();
  }

  /**
   * returns true if the document is valid against the referenced schemas
   */
  public boolean isValid()
  {
    return true;
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    return DOMPrinter.nodeToString( document, "" );
  }

  /* #GMLSchema lnkGMLSchema; */
}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.2  2004/08/11 11:20:16  doemming
 * *** empty log message ***
 * Revision 1.1.1.1 2004/05/11 16:43:24 doemming
 * backup of local modified deegree sources
 * 
 * Revision 1.7 2004/03/02 07:38:14 poth no message
 * 
 * Revision 1.6 2004/02/19 10:08:56 poth no message
 * 
 * Revision 1.5 2003/11/28 11:35:56 poth no message
 * 
 * Revision 1.4 2003/06/17 07:44:43 poth no message
 * 
 * Revision 1.3 2003/04/23 15:44:39 poth no message
 * 
 * Revision 1.2 2003/04/17 13:54:48 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:01 poth no message
 * 
 * Revision 1.4 2002/08/19 15:59:29 ap no message
 * 
 * Revision 1.3 2002/08/05 16:11:02 ap no message
 * 
 * Revision 1.2 2002/04/23 14:24:55 ap no message
 *  
 */
