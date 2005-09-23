/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

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

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 
 history:
 
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
 
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.gml;

import java.io.IOException;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.kalypsodeegree.gml.GMLDocument;
import org.kalypsodeegree.gml.GMLException;
import org.kalypsodeegree.gml.GMLFeature;
import org.kalypsodeegree.gml.GMLFeatureCollection;
import org.kalypsodeegree.gml.GMLGeometry;
import org.kalypsodeegree.gml.GMLNameSpace;
import org.kalypsodeegree.gml.GMLProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.ogcbasic.CommonNamespaces;
import org.kalypsodeegree.xml.DOMPrinter;
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.extension.IMarshallingTypeHandler;
import org.kalypsodeegree_impl.extension.MarshallingTypeRegistrySingleton;
import org.kalypsodeegree_impl.tools.Debug;
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
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 07.02.2001
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
      final String schemaLocation = m_document.getDocumentElement().getAttributeNS( XSI_NS, "schemaLocation" );
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

  /** Sollte mit dem Targetnamespace zusammen aufgerufen werden */
  public void setSchemaLocation( final String loc )
  {
    setAttribute( "xsi:schemaLocation", loc );
  }

  /**
   * @see org.kalypsodeegree_impl.gml.GMLDocument_Impl#getNameSpaces()
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
        root.setAttribute( nameSpace.getNameSpaceName() + ":" + nameSpace.getSubSpaceName(), nameSpace
            .getNameSpaceValue() );
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
   * @see org.kalypsodeegree_impl.gml.GMLDocument_Impl#getRoot()
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
   * @see org.kalypsodeegree.gml.GMLDocument#getNameSpaces()
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

  public ProcessingInstruction createProcessingInstruction( String arg0, String arg1 ) throws DOMException
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

  public GMLFeature createGMLFeature( final FeatureType featureType )
  {
    Debug.debugMethodBegin( "", "createGMLFeature(Document, FeatureType)" );

    final Element elem = createElementNS( featureType.getNamespace(), featureType.getName() );
    final GMLFeature feature = new GMLFeature_Impl( elem );

    Debug.debugMethodEnd();
    return feature;
  }

  /**
   * creates a GMLFeatureCollection that doesn't contain a property and that hasn't an id.
   */
  public GMLFeatureCollection createGMLFeatureCollection( final String collectionName )
  {
    Debug.debugMethodBegin();

    final Element elem = createElementNS( CommonNamespaces.GMLNS, collectionName );
    final Element el = createElementNS( CommonNamespaces.GMLNS, "boundedBy" );
    elem.appendChild( el );

    final GMLFeatureCollection feature = new GMLFeatureCollection_Impl( elem );

    Debug.debugMethodEnd();
    return feature;
  }

  /**
   * @see org.kalypsodeegree.gml.GMLDocument#createGMLFeature(org.kalypsodeegree.model.feature.FeatureType,
   *      java.lang.String, org.kalypsodeegree.gml.GMLProperty[])
   */
  public GMLFeature createGMLFeature( final FeatureType featureType, final String id, GMLProperty[] properties )
      throws GMLException
  {
    Debug.debugMethodBegin( "", "createGMLFeature(Document, String, String, GMLProperty[])" );

    final GMLFeature feature = createGMLFeature( featureType );
    for( int i = 0; i < properties.length; i++ )
      feature.addProperty( properties[i] );

    Debug.debugMethodEnd();
    return feature;
  }

  /**
   * factory method to create a GMLProperty. the property that will be return doesn't contain a value.
   */
  public GMLProperty createGMLProperty( final FeatureTypeProperty ftp )
  {
    final Element elem = createElementNS( ftp.getNamespace(), ftp.getName() );
    final GMLProperty ls = new GMLProperty_Impl( elem );
    return ls;
  }

  public GMLProperty createGMLProperty( final FeatureTypeProperty ftp, final Element propertyValue )
  {
    final GMLProperty ls = createGMLProperty( ftp );
    ls.setPropertyValue( propertyValue );
    return ls;
  }

  public GMLProperty createGMLProperty( final FeatureTypeProperty ftp, final String attributeValue )
  {
    final Element element = createElementNS( ftp.getNamespace(), ftp.getName() );
    GMLProperty gmlProp = new GMLProperty_Impl( ftp, element );
    gmlProp.setPropertyValue( attributeValue );
    return gmlProp;
  }

  public GMLProperty createGMLProperty( final FeatureTypeProperty ftp, final Object customObject ) throws GMLException
  {
    try
    {
      final Element element = createElementNS( ftp.getNamespace(), ftp.getName() );

      // marshalling
      final IMarshallingTypeHandler typeHandler = (IMarshallingTypeHandler)MarshallingTypeRegistrySingleton.getTypeRegistry().getTypeHandlerForClassName(
          ftp.getType() );
      // TODO give context not null
      typeHandler.marshall( customObject, element, null );
      GMLCustomProperty_Impl gmlProp = new GMLCustomProperty_Impl( ftp, element );

      Debug.debugMethodEnd();
      return gmlProp;
    }
    catch( final Exception e )
    {
      throw new GMLException( e.getLocalizedMessage() );
    }
  }

  /**
   * 
   * @throws GMLException
   * @see org.kalypsodeegree.gml.GMLDocument#createGMLGeoProperty(org.kalypsodeegree.model.feature.FeatureTypeProperty,
   *      org.kalypsodeegree.model.geometry.GM_Object)
   */
  public GMLProperty createGMLGeoProperty( FeatureTypeProperty ftp, GM_Object geom ) throws GMLException
  {
    final GMLProperty ls = createGMLProperty( ftp );
    GMLGeometry geometry = GMLFactory.createGMLGeometry( this, geom );
    ls.setPropertyValue( ( (GMLGeometry_Impl)geometry ).getAsElement() );
    return ls;
  }
}