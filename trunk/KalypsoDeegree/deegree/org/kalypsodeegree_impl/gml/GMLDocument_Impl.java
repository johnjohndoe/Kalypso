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

import java.io.IOException;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.deegree.gml.GMLDocument;
import org.deegree.gml.GMLFeature;
import org.deegree.gml.GMLFeatureCollection;
import org.deegree.gml.GMLNameSpace;
import org.deegree.xml.DOMPrinter;
import org.deegree.xml.XMLTools;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Attr;
import org.w3c.dom.CDATASection;
import org.w3c.dom.Comment;
import org.w3c.dom.DOMException;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.DocumentType;
import org.w3c.dom.Element;
import org.w3c.dom.EntityReference;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.ProcessingInstruction;
import org.w3c.dom.Text;
import org.xml.sax.SAXException;

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
public class GMLDocument_Impl implements GMLDocument, Document, Element
{
  private final org.w3c.dom.Document m_document;

  private Element getGMLElement()
  {
    return m_document.getDocumentElement();
  }

  private static final String XSI_NS = "http://www.w3.org/2001/XMLSchema-instance";

  private final HashMap m_nameSpaces = new HashMap();

  /**
   * Creates a new GMLDocument_Impl object.
   */
  public GMLDocument_Impl()
  {
    DocumentBuilder parser = null;
    try
    {
      DocumentBuilderFactory fac = DocumentBuilderFactory.newInstance();
      fac.setNamespaceAware( true );
      parser = fac.newDocumentBuilder();
    }
    catch( ParserConfigurationException ex )
    {
      ex.printStackTrace();
    }
    m_document = parser.newDocument();
    XMLTools.insertNodeInto( m_document.createElement( "fake" ), m_document );
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
    m_document = XMLTools.parse( reader );
  }

  /**
   * Creates a new GMLDocument_Impl object.
   * 
   * @param document
   */
  public GMLDocument_Impl( Document document )
  {
    m_document = document;
  }

  /**
   * returns the location of the schema the document based on
   */
  public URL getSchemaLocation() throws MalformedURLException
  {
    Debug.debugMethodBegin( this, "getSchemaLocation" );
    try
    {
      final String schemaLocation = m_document.getDocumentElement().getAttributeNS(
          "http://www.w3.org/2001/XMLSchema-instance", "schemaLocation" );
      if( schemaLocation == null )
        return null;

      final String namespaceURI = m_document.getDocumentElement().getNamespaceURI();
      if( namespaceURI != null && schemaLocation.startsWith( namespaceURI ) )
      {
        final String path = schemaLocation.substring( namespaceURI.length() );
        return new URL( path );
      }
      return new URL( schemaLocation );
    }
    finally
    {
      Debug.debugMethodEnd();
    }
  }

  /**
   * returns the location of the schema the document based on
   */
  public String getSchemaLocationName()
  {
    Debug.debugMethodBegin( this, "getSchemaLocation" );

    try
    {
      final String schemaLocation = m_document.getDocumentElement().getAttributeNS( XSI_NS,
          "schemaLocation" );
      if( schemaLocation == null )
        return null;

      final String namespaceURI = m_document.getDocumentElement().getNamespaceURI();
      if( namespaceURI != null && schemaLocation.startsWith( namespaceURI ) )
      {
        final String path = schemaLocation.substring( namespaceURI.length() ).trim();
        return path;
      }

      return schemaLocation;
    }
    finally
    {
      Debug.debugMethodEnd();
    }
  }

  /**
   * sets the location of schema the document based on
   */
  public void setSchemaLocation( URL schemaLocation )
  {
    setSchemaLocation( schemaLocation.toString() );
  }

  public void setSchemaLocation( final String loc )
  {
    setAttribute( "xsi:schemaLocation", loc );
  }

  /**
   * @see org.deegree_impl.gml.GMLDocument_Impl#getNameSpaces()
   */
  public void addNameSpace( GMLNameSpace nameSpace )
  {
    if( m_nameSpaces.containsKey( nameSpace.getNameSpaceValue() ) )
      return;
    m_nameSpaces.put( nameSpace.getNameSpaceValue(), nameSpace );
  }

  public void applyNameSpaces()
  {
    GMLNameSpace[] nameSpaces = getNameSpaces();
    for( int i = 0; i < nameSpaces.length; i++ )
    {
      GMLNameSpace nameSpace = nameSpaces[i];
      Element root = m_document.getDocumentElement();
      if( nameSpace.getSubSpaceName() != null )
      {
        root.setAttribute( nameSpace.getNameSpaceName() + ":" + nameSpace.getSubSpaceName(),
            nameSpace.getNameSpaceValue() );
      }
      else
      {
        root.setAttribute( nameSpace.getNameSpaceName(), nameSpace.getNameSpaceValue() );
      }
    }
  }

  /**
   * returns the root element of the document as GMLFeatureCollection.
   * 
   * @deprecated
   */
  public GMLFeatureCollection getRoot()
  {
    return new GMLFeatureCollection_Impl( m_document.getDocumentElement() );
  }

  public GMLFeature getRootFeature()
  {
    return new GMLFeature_Impl( m_document.getDocumentElement() );
  }

  /**
   * @see org.deegree_impl.gml.GMLDocument_Impl#getRoot()
   * @deprecated
   */
  public void setRoot( GMLFeatureCollection root )
  {
    Node node = m_document.getDocumentElement();
    // remove root node if it already exists
    if( node != null )
    {
      m_document.removeChild( node );
    }
    XMLTools.insertNodeInto( ( (GMLFeatureCollection_Impl)root ).getAsElement(), m_document );
  }

  public void setRoot( GMLFeature rootFeature )
  {
    Debug.debugMethodBegin();

    Node node = m_document.getDocumentElement();

    // remove root node if it already exists
    if( node != null )
    {
      m_document.removeChild( node );
    }

    XMLTools.insertNodeInto( rootFeature.getAsElement(), m_document );
    //    
    applyNameSpaces();
    //
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
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    return DOMPrinter.nodeToString( m_document, "" );
  }

  /**
   * @see org.deegree.gml.GMLDocument#getNameSpaces()
   */
  public GMLNameSpace[] getNameSpaces()
  {
    return (GMLNameSpace[])m_nameSpaces.values().toArray( new GMLNameSpace[m_nameSpaces.size()] );
  }

  public Node appendChild( Node arg0 ) throws DOMException
  {
    return m_document.appendChild( arg0 );
  }

  public Node cloneNode( boolean arg0 )
  {
    return m_document.cloneNode( arg0 );
  }

  public Attr createAttribute( String arg0 ) throws DOMException
  {
    return m_document.createAttribute( arg0 );
  }

  public Attr createAttributeNS( String arg0, String arg1 ) throws DOMException
  {
    return m_document.createAttributeNS( arg0, arg1 );
  }

  public CDATASection createCDATASection( String arg0 ) throws DOMException
  {
    return m_document.createCDATASection( arg0 );
  }

  public Comment createComment( String arg0 )
  {
    return m_document.createComment( arg0 );
  }

  public DocumentFragment createDocumentFragment()
  {
    return m_document.createDocumentFragment();
  }

  public Element createElement( String arg0 ) throws DOMException
  {
    return m_document.createElement( arg0 );
  }

  public Element createElementNS( String namespace, String propName ) throws DOMException
  {
    if( propName.indexOf( ":" ) > -1 )
    {
      // it seems that prefix is allready set
      return m_document.createElementNS( namespace, propName );
    }
    GMLNameSpace ns = (GMLNameSpace)m_nameSpaces.get( namespace );
    if( ns != null )
    {
      String prefix = ns.getSubSpaceName();
      if( prefix != null )
        return m_document.createElementNS( namespace, prefix + ":" + propName );
    }
    return m_document.createElementNS( namespace, propName );
  }

  public EntityReference createEntityReference( String arg0 ) throws DOMException
  {
    return m_document.createEntityReference( arg0 );
  }

  public ProcessingInstruction createProcessingInstruction( String arg0, String arg1 )
      throws DOMException
  {
    return m_document.createProcessingInstruction( arg0, arg1 );
  }

  public Text createTextNode( String arg0 )
  {
    return m_document.createTextNode( arg0 );
  }

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  public boolean equals( Object obj )
  {
    return m_document.equals( obj );
  }

  public NamedNodeMap getAttributes()
  {
    return m_document.getAttributes();
  }

  public NodeList getChildNodes()
  {
    return m_document.getChildNodes();
  }

  public DocumentType getDoctype()
  {
    return m_document.getDoctype();
  }

  public Element getDocumentElement()
  {
    return m_document.getDocumentElement();
  }

  public Element getElementById( String arg0 )
  {
    return m_document.getElementById( arg0 );
  }

  public NodeList getElementsByTagName( String arg0 )
  {
    return m_document.getElementsByTagName( arg0 );
  }

  public NodeList getElementsByTagNameNS( String arg0, String arg1 )
  {
    return m_document.getElementsByTagNameNS( arg0, arg1 );
  }

  public Node getFirstChild()
  {
    return m_document.getFirstChild();
  }

  public DOMImplementation getImplementation()
  {
    return m_document.getImplementation();
  }

  public Node getLastChild()
  {
    return m_document.getLastChild();
  }

  public String getLocalName()
  {
    return m_document.getLocalName();
  }

  public String getNamespaceURI()
  {
    return m_document.getNamespaceURI();
  }

  public Node getNextSibling()
  {
    return m_document.getNextSibling();
  }

  public String getNodeName()
  {
    return m_document.getNodeName();
  }

  public short getNodeType()
  {
    return m_document.getNodeType();
  }

  public String getNodeValue() throws DOMException
  {
    return m_document.getNodeValue();
  }

  public Document getOwnerDocument()
  {
    return m_document.getOwnerDocument();
  }

  public Node getParentNode()
  {
    return m_document.getParentNode();
  }

  public String getPrefix()
  {
    return m_document.getPrefix();
  }

  public Node getPreviousSibling()
  {
    return m_document.getPreviousSibling();
  }

  public boolean hasAttributes()
  {
    return m_document.hasAttributes();
  }

  public boolean hasChildNodes()
  {
    return m_document.hasChildNodes();
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  public int hashCode()
  {
    return m_document.hashCode();
  }

  public Node importNode( Node arg0, boolean arg1 ) throws DOMException
  {
    return m_document.importNode( arg0, arg1 );
  }

  public Node insertBefore( Node arg0, Node arg1 ) throws DOMException
  {
    return m_document.insertBefore( arg0, arg1 );
  }

  public boolean isSupported( String arg0, String arg1 )
  {
    return m_document.isSupported( arg0, arg1 );
  }

  public void normalize()
  {
    m_document.normalize();
  }

  public Node removeChild( Node arg0 ) throws DOMException
  {
    return m_document.removeChild( arg0 );
  }

  public Node replaceChild( Node arg0, Node arg1 ) throws DOMException
  {
    return m_document.replaceChild( arg0, arg1 );
  }

  public void setNodeValue( String arg0 ) throws DOMException
  {
    m_document.setNodeValue( arg0 );
  }

  public void setPrefix( String arg0 ) throws DOMException
  {
    m_document.setPrefix( arg0 );
  }

  public void removeAttribute( String arg0 ) throws DOMException
  {
    getGMLElement().removeAttribute( arg0 );
  }

  public Attr setAttributeNodeNS( Attr arg0 ) throws DOMException
  {
    return getGMLElement().setAttributeNodeNS( arg0 );
  }

  public void setAttributeNS( String arg0, String arg1, String arg2 ) throws DOMException
  {
    getGMLElement().setAttributeNS( arg0, arg1, arg2 );
  }

  public Attr getAttributeNode( String arg0 )
  {
    return getGMLElement().getAttributeNode( arg0 );
  }

  public void removeAttributeNS( String arg0, String arg1 ) throws DOMException
  {
    getGMLElement().removeAttributeNS( arg0, arg1 );
  }

  public Attr getAttributeNodeNS( String arg0, String arg1 )
  {
    return getGMLElement().getAttributeNodeNS( arg0, arg1 );
  }

  public boolean hasAttributeNS( String arg0, String arg1 )
  {
    return getGMLElement().hasAttributeNS( arg0, arg1 );
  }

  public void setAttribute( String arg0, String arg1 ) throws DOMException
  {
    getGMLElement().setAttribute( arg0, arg1 );
  }

  public Attr removeAttributeNode( Attr arg0 ) throws DOMException
  {
    return getGMLElement().removeAttributeNode( arg0 );
  }

  public boolean hasAttribute( String arg0 )
  {
    return getGMLElement().hasAttribute( arg0 );
  }

  public Attr setAttributeNode( Attr arg0 ) throws DOMException
  {
    return getGMLElement().setAttributeNode( arg0 );
  }

  public String getTagName()
  {
    return getGMLElement().getTagName();
  }

  public String getAttribute( String arg0 )
  {
    return getGMLElement().getAttribute( arg0 );
  }

  public String getAttributeNS( String arg0, String arg1 )
  {
    return getGMLElement().getAttributeNS( arg0, arg1 );
  }

  public Document getDocument()
  {
    return m_document;
  }
}