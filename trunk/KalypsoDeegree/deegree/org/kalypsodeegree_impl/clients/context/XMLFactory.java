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

package org.deegree_impl.clients.context;

import java.awt.Rectangle;
import java.net.URL;

import javax.xml.parsers.ParserConfigurationException;

import org.deegree.clients.context.Frontend;
import org.deegree.clients.context.GUIArea;
import org.deegree.clients.context.Module;
import org.deegree.clients.context.ModuleConfiguration;
import org.deegree.model.geometry.GM_Point;
import org.deegree.ogcbasic.BaseURL;
import org.deegree.ogcbasic.CommonNamespaces;
import org.deegree.ogcbasic.ContactAddress;
import org.deegree.ogcbasic.ContactInformation;
import org.deegree.ogcbasic.ContactPersonPrimary;
import org.deegree.tools.Parameter;
import org.deegree.tools.ParameterList;
import org.deegree_impl.clients.wmsclient.configuration.MapOperationFactor;
import org.deegree_impl.ogcbasic.ImageURL;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;

/**
 * This is a factory class to export a <code>ViewContext</code> and a
 * <code>ViewContextCollection</code> as an xml
 * <code>org.w3c.dom.Document</code>.
 * 
 * @author <a href="mailto:taddei@lat-lon.de">Ugo Taddei </a>
 *  
 */
public class XMLFactory
{

  // Import and define constants
  private static String OGC_CONTEXT_NS = CommonNamespaces.CNTXTNS;

  private static String D_CONTEXT_NS = CommonNamespaces.DGCNTXTNS;

  private static String SLD_NS = CommonNamespaces.SLDNS;

  private static String XSI_NS = "http://www.w3.org/2001/XMLSchema-instance";

  private static String XLINK_NS = "http://www.w3.org/1999/xlink";

  // Common objects
  protected static javax.xml.parsers.DocumentBuilderFactory factory = null;

  protected static javax.xml.parsers.DocumentBuilder builder = null;

  protected static Document document = null;

  // Forbid instantiation
  private XMLFactory()
  {}

  /**
   * Convenience method for creating a common document builder. Implementation
   * copied from XmlDocument (by tf and ap).
   */
  protected static void initDocBuilder() throws ParserConfigurationException
  {

    Debug.debugMethodBegin();

    if( builder == null )
    {
      if( factory == null )
      {
        factory = javax.xml.parsers.DocumentBuilderFactory.newInstance();
        factory.setIgnoringElementContentWhitespace( true );
        factory.setNamespaceAware( false );
        factory.setExpandEntityReferences( false );
      }
      builder = factory.newDocumentBuilder();
    }
    Debug.debugMethodEnd();
  }

  /**
   * Creates a new <code>org.w3c.dom.Document</code> using the internal
   * document builder.
   * 
   * @return new <code>Document</code> instance
   */
  protected static Document createDocument() throws ParserConfigurationException
  {

    Debug.debugMethodBegin();

    initDocBuilder();

    Debug.debugMethodEnd();
    return builder.newDocument();
  }

  /**
   * Creates a new <code>org.w3c.dom.Element</code>.
   * 
   * @param namespace
   *          the element namespace
   * @param elemName
   *          the element name
   * @return new <code>Element</code> instance
   */
  public static Element createElement( String namespace, String elemName )
  {

    Debug.debugMethodBegin();

    String ns = "";
    if( namespace != null )
    {
      ns = namespace;
    }

    Debug.debugMethodEnd();
    return document.createElementNS( namespace, elemName );
  }

  /**
   * Creates a new <code>org.w3c.dom.Attr</code>.
   * 
   * @param attName
   *          the attribute name
   * @param value
   *          the attribute value
   * @return new <code>Attr</code> instance
   */
  public static Attr createAttribute( String attName, String value )
  {

    Debug.debugMethodBegin();

    Attr attr = document.createAttribute( attName );
    attr.setValue( value );

    Debug.debugMethodEnd();
    return attr;
  }

  /**
   * Creates a new <code>org.w3c.dom.Text</code>. This is the textual content
   * of an element.
   * 
   * @param text
   *          the attribute name (if <code>null</code>, then context stays
   *          empty)
   * @return new <code>Text</code> instance
   */
  public static Text createTextNode( String text )
  {

    Debug.debugMethodBegin();

    String t = "";
    if( text != null )
    {
      t = text;
    }

    Debug.debugMethodEnd();
    return document.createTextNode( t );
  }

  /**
   * Creates a new <code>org.w3c.dom.Document</code> describing a
   * <code>ViewContext</code>.
   * 
   * @param viewContext
   *          the <code>ViewContext</code> to be exported
   * 
   * @throws ParserConfigurationException
   *           if an XML parser couldn't be found
   */
  public static Document export( ViewContext viewContext ) throws ParserConfigurationException
  {

    Debug.debugMethodBegin();

    document = createDocument();
    // start appending nodes...
    appendViewContext( document, viewContext, D_CONTEXT_NS );

    Debug.debugMethodEnd();
    return document;
  }

  /**
   * Creates a new <code>org.w3c.dom.Document</code> describing a
   * <code>ViewContextCollection</code>.
   * 
   * @param viewContCollec
   *          the <code>ViewContextCollection</code> to be exported
   * 
   * @throws ParserConfigurationException
   *           if an XML parser couldn't be found
   *  
   */
  public static Document export( ViewContextCollection viewContCollec )
      throws ParserConfigurationException
  {

    Debug.debugMethodBegin();

    document = createDocument();
    // start appending nodes...
    appendViewContextCollection( document, viewContCollec, D_CONTEXT_NS );

    Debug.debugMethodEnd();
    return document;
  }

  /**
   * Appends the XML representation of a <code>ViewContext</code> to a
   * <code>Node</code> using the <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param viewContxt
   *          the <code>ViewContext</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   *  
   */
  protected static void appendViewContext( Node toNode, ViewContext viewContxt, String namespace )
  {

    Debug.debugMethodBegin();

    if( viewContxt != null )
    {
      Element e = createElement( namespace, "ViewContext" );
      e.setAttributeNode( createAttribute( "xmlns", OGC_CONTEXT_NS ) );
      e.setAttributeNode( createAttribute( "xmlns:sld", SLD_NS ) );
      e.setAttributeNode( createAttribute( "xmlns:xlink", XLINK_NS ) );
      e.setAttributeNode( createAttribute( "xmlns:xsi", XSI_NS ) );
      e.setAttributeNode( createAttribute( "xmlns:xsi", XSI_NS ) );
      e.setAttributeNode( createAttribute( "version", "1.0.0" ) );
      e.setAttributeNode( createAttribute( "id", "viewContext_id" ) );

      appendGeneral( e, viewContxt.getGeneral(), D_CONTEXT_NS );
      appendLayerList( e, viewContxt.getLayerList(), D_CONTEXT_NS );

      toNode.appendChild( e );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a <code>General</code> to a
   * <code>Node</code> using the <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param gen
   *          the <code>General</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   * 
   * contains illegal characters
   */
  protected static void appendGeneral( Node toNode, General gen, String namespace )
  {

    Debug.debugMethodBegin();

    if( gen != null )
    {
      Element e = createElement( namespace, "General" );
      appendWindow( e, gen.getWindow(), namespace );
      appendBoundingBox( e, gen.getBoundingBox(), namespace );
      appendTitle( e, gen.getTitle(), namespace );
      appendAbstract( e, gen.getAbstract(), namespace );
      appendKeywords( e, gen.getKeywords(), namespace );
      appendDescriptionURL( e, gen.getDescriptionURL(), namespace );
      appendLogoURL( e, gen.getLogoURL(), namespace );
      appendContactInformation( e, gen.getContactInformation(), namespace );
      // append deegree-specific extension
      appendGeneralExtension( e, (GeneralExtension)gen.getExtension(), D_CONTEXT_NS );

      toNode.appendChild( e );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a <code>Rectangle</code> to a
   * <code>Node</code> using the <code>namespace</code>. <p/>Note that the
   * XML representation of a <code>Rectangle</code> is given by a
   * <code>&lt;Window&gt;</code> element.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param r
   *          the <code>Rectangle</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   * 
   * contains illegal characters
   */
  protected static void appendWindow( Node toNode, Rectangle r, String namespace )
  {

    Debug.debugMethodBegin();

    if( r != null )
    {
      Element window = createElement( namespace, "Window" );

      window.setAttribute( "width", String.valueOf( r.width ) );
      window.setAttribute( "height", String.valueOf( r.height ) );

      toNode.appendChild( window );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a <code>GM_Point[]</code> to a
   * <code>Node</code> using the <code>namespace</code>. <p/>Note that the
   * XML representation of a <code>GM_Point[]</code> is given by a
   * <code>&lt;BoundingBox&gt;</code> element.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param points
   *          the <code>GM_Point[]</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   * 
   * contains illegal characters
   */
  protected static void appendBoundingBox( Node toNode, GM_Point[] points, String namespace )
  {
    Debug.debugMethodBegin();

    if( points != null && points.length == 2 )
    {
      Element bbox = createElement( namespace, "BoundingBox" );
      String srs = "UNKNOWN_SRS";
      try
      {
        srs = points[0].getCoordinateSystem().getName();
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }

      bbox.setAttributeNode( createAttribute( "SRS", srs ) );

      bbox.setAttribute( "minx", String.valueOf( points[0].getX() ) );
      bbox.setAttribute( "miny", String.valueOf( points[0].getY() ) );
      bbox.setAttribute( "maxx", String.valueOf( points[1].getX() ) );
      bbox.setAttribute( "maxy", String.valueOf( points[1].getY() ) );

      toNode.appendChild( bbox );

    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a <code>Title</code> to a
   * <code>Node</code> using the <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param title
   *          the <code>String</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   * 
   * contains illegal characters
   */
  protected static void appendTitle( Node toNode, String title, String namespace )
  {

    Debug.debugMethodBegin();

    String t = "";
    if( t != null )
    {
      t = title;
    }
    Element te = createElement( namespace, "Title" );
    te.appendChild( createTextNode( t ) );
    toNode.appendChild( te );

    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of an <code>Abstract</code> to a
   * <code>Node</code> using the <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param abstr
   *          the <code>String</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   *  
   */
  protected static void appendAbstract( Node toNode, String abstr, String namespace )
  {

    Debug.debugMethodBegin();

    if( abstr != null )
    {
      Element te = createElement( namespace, "Abstract" );
      te.appendChild( createTextNode( abstr ) );
      toNode.appendChild( te );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of an <code>ImageURL</code> to a
   * <code>Node</code> using the <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param logoURL
   *          the <code>ImageURL</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   * 
   * contains illegal characters
   */
  protected static void appendLogoURL( Node toNode, ImageURL logoURL, String namespace )
  {

    Debug.debugMethodBegin();

    if( logoURL != null && logoURL.getOnlineResource() != null )
    {
      Element e = createElement( namespace, "LogoURL" );
      appendOnlineResource( e, logoURL.getOnlineResource(), namespace );
      toNode.appendChild( e );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a keyword list as a <code>String[]</code>
   * to a <code>Node</code> using the <code>namespace</code>. <p/>Note that
   * the keywords are appended to a <code>&lt;KeywordList&gt;</code> element.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param keywords
   *          the <code>ImageURL</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   * 
   * contains illegal characters
   */
  protected static void appendKeywords( Node toNode, String[] keywords, String namespace )
  {

    Debug.debugMethodBegin();

    if( keywords != null )
    {
      Element kWordList = createElement( namespace, "KeywordList" );
      for( int i = 0; i < keywords.length; i++ )
      {
        Element kw = createElement( namespace, "Keyword" );
        kw.appendChild( createTextNode( keywords[i] ) );
        kWordList.appendChild( kw );
      }
      toNode.appendChild( kWordList );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a <code>BaseURL</code>, the
   * <code>DescriptionURL</code>, to a <code>Node</code> using the
   * <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param bURL
   *          the <code>BaseURL</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   * 
   * contains illegal characters
   */
  protected static void appendDescriptionURL( Node toNode, BaseURL bURL, String namespace )
  {

    Debug.debugMethodBegin();

    if( bURL != null )
    {
      Element du = createElement( namespace, "DescriptionURL" );
      String f = bURL.getFormat();
      if( f != null )
      {
        du.setAttribute( "format", f );
      }

      URL onlineRes = bURL.getOnlineResource();
      appendOnlineResource( du, onlineRes, namespace );

      toNode.appendChild( du );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a <code>URL</code> to a
   * <code>Node</code> as a <code>&lt;OnlineResource&gt;</code> using the
   * <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param onlineRes
   *          the <code>URL</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   * 
   * contains illegal characters
   */
  protected static void appendOnlineResource( Node toNode, URL onlineRes, String namespace )
  {

    Debug.debugMethodBegin();

    if( onlineRes != null )
    {
      Element or = createElement( namespace, "OnlineResource" );
      or.setAttribute( "xlink:type", "simple" );

      String href = onlineRes.toString();
      if( href != null )
      {
        or.setAttribute( "xlink:href", href );
      }
      toNode.appendChild( or );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a <code>ContactInformation</code> to a
   * <code>Node</code> using the <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param contInfo
   *          the <code>ContactInformation</code> to be appended as new
   *          element
   * @param namespace
   *          the namespace of the new element
   * 
   * contains illegal characters
   */
  protected static void appendContactInformation( Node toNode, ContactInformation contInfo,
      String namespace )
  {

    Debug.debugMethodBegin();

    if( contInfo != null )
    {
      Element ci = createElement( namespace, "ContactInformation" );

      appendContactPersonPrimary( ci, contInfo.getContactPersonPrimary(), namespace );

      Element pos = createElement( namespace, "ContactPosition" );
      pos.appendChild( createTextNode( contInfo.getContactPosition() ) );
      ci.appendChild( pos );
      appendContactAddress( ci, contInfo.getContactAddress(), namespace );

      Element e = createElement( namespace, "ContactVoiceTelephone" );
      e.appendChild( createTextNode( contInfo.getContactVoiceTelephone() ) );
      ci.appendChild( e );

      e = createElement( namespace, "ContactElectronicMailAddress" );
      e.appendChild( createTextNode( contInfo.getContactElectronicMailAddress() ) );
      ci.appendChild( e );

      toNode.appendChild( ci );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a <code>ContactPersonPrimary</code> to
   * a <code>Node</code> using the <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param contPersonPrim
   *          the <code>ContactPersonPrimary</code> to be appended as new
   *          element
   * @param namespace
   *          the namespace of the new element
   * 
   * contains illegal characters
   */
  protected static void appendContactPersonPrimary( Node toNode,
      ContactPersonPrimary contPersonPrim, String namespace )
  {

    Debug.debugMethodBegin();

    if( contPersonPrim != null )
    {
      Element cpp = createElement( namespace, "ContactPersonPrimary" );

      Element p = createElement( namespace, "ContactPerson" );
      p.appendChild( createTextNode( contPersonPrim.getContactPerson() ) );
      cpp.appendChild( p );

      Element org = createElement( namespace, "ContactOrganization" );
      org.appendChild( createTextNode( contPersonPrim.getContactOrganization() ) );
      cpp.appendChild( org );

      toNode.appendChild( cpp );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a <code>ContactAddress</code> to a
   * <code>Node</code> using the <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param address
   *          the <code>ContactAddress</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   *  
   */
  protected static void appendContactAddress( Node toNode, ContactAddress address, String namespace )
  {

    Debug.debugMethodBegin();

    if( address != null )
    {
      Element ca = createElement( namespace, "ContactAddress" );

      Element e = createElement( namespace, "AddressType" );
      e.appendChild( createTextNode( address.getAddressType() ) );
      ca.appendChild( e );

      e = createElement( namespace, "Address" );
      e.appendChild( createTextNode( address.getAddress() ) );
      ca.appendChild( e );

      e = createElement( namespace, "City" );
      e.appendChild( createTextNode( address.getCity() ) );
      ca.appendChild( e );

      e = createElement( namespace, "StateOrProvince" );
      e.appendChild( createTextNode( address.getStateOrProvince() ) );
      ca.appendChild( e );

      e = createElement( namespace, "PostCode" );
      e.appendChild( createTextNode( address.getPostCode() ) );
      ca.appendChild( e );

      e = createElement( namespace, "Country" );
      e.appendChild( createTextNode( address.getCountry() ) );
      ca.appendChild( e );

      toNode.appendChild( ca );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a <code>LayerList</code> to a
   * <code>Node</code> using the <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param lList
   *          the <code>LayerList</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   *  
   */
  protected static void appendLayerList( Node toNode, LayerList lList, String namespace )
  {

    Debug.debugMethodBegin();

    if( lList != null )
    {
      Element list = createElement( namespace, "LayerList" );

      Layer[] ls = lList.getLayers();
      if( ls != null )
      {
        for( int i = 0; i < ls.length; i++ )
        {
          appendLayer( list, ls[i], namespace );
        }
      }
      toNode.appendChild( list );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a <code>Layer</code> to a
   * <code>Node</code> using the <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param layer
   *          the <code>Layer</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   *  
   */
  protected static void appendLayer( Node toNode, Layer layer, String namespace )
  {

    Debug.debugMethodBegin();

    if( layer != null )
    {
      Element le = createElement( namespace, "Layer" );

      le.setAttribute( "queryable", stringValue01( layer.isQueryable() ) );
      le.setAttribute( "hidden", stringValue01( layer.isHidden() ) );

      appendServer( le, layer.getServer(), namespace );

      Element n = createElement( namespace, "Name" );
      n.appendChild( createTextNode( layer.getName() ) );
      le.appendChild( n );

      n = createElement( namespace, "Title" );
      n.appendChild( createTextNode( layer.getTitle() ) );
      le.appendChild( n );

      appendSrs( le, layer.getSrs(), namespace );
      appendFormatList( le, layer.getFormatList(), namespace );
      appendStyleList( le, layer.getStyleList(), namespace );

      appendLayerExtension( le, (LayerExtension)layer.getExtension(), namespace );

      toNode.appendChild( le );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a <code>Server</code> to a
   * <code>Node</code> using the <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param server
   *          the <code>Server</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   *  
   */
  protected static void appendServer( Node toNode, Server server, String namespace )
  {

    Debug.debugMethodBegin();

    if( server != null )
    {
      Element serv = createElement( namespace, "Server" );

      if( server.getService() != null )
      {
        serv.setAttribute( "service", server.getService() );
      }
      if( server.getService() != null )
      {
        serv.setAttribute( "version", server.getVersion() );
      }
      if( server.getService() != null )
      {
        serv.setAttribute( "title", server.getTitle() );
      }

      appendOnlineResource( serv, server.getOnlineResource(), namespace );

      toNode.appendChild( serv );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a list of SRSs as a <code>String[]</code>
   * to a <code>Node</code> using the <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param srsList
   *          the <code>String[]</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   *  
   */
  protected static void appendSrs( Node toNode, String[] srsList, String namespace )
  {

    Debug.debugMethodBegin();

    if( srsList != null )
    {
      StringBuffer sBuf = new StringBuffer( 100 );
      for( int i = 0; i < srsList.length; i++ )
      {
        sBuf.append( srsList[i] );
        if( i < srsList.length - 1 )
          sBuf.append( ";" );

      }
      Element e = createElement( namespace, "SRS" );
      e.appendChild( createTextNode( sBuf.toString() ) );
      toNode.appendChild( e );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a list of a <code>FormatList</code> to
   * a <code>Node</code> using the <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param formatList
   *          the <code>FormatList</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   * 
   * contains illegal characters
   */
  protected static void appendFormatList( Node toNode, FormatList formatList, String namespace )
  {

    Debug.debugMethodBegin();

    if( formatList != null )
    {

      Format[] formats = formatList.getFormats();
      if( formats != null )
      {
        Element e = createElement( namespace, "FormatList" );

        for( int i = 0; i < formats.length; i++ )
        {
          if( formats[i] != null )
          {
            Element f = createElement( namespace, "Format" );
            f.setAttribute( "current", stringValue01( formats[i].isCurrent() ) );
            if( formats[i].getName() != null )
              f.appendChild( createTextNode( formats[i].getName() ) );
            e.appendChild( f );
          }
        }
        toNode.appendChild( e );
      }
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a list of a <code>StyleList</code> to a
   * <code>Node</code> using the <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param styleList
   *          the <code>StyleList</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   *  
   */
  protected static void appendStyleList( Node toNode, StyleList styleList, String namespace )
  {

    Debug.debugMethodBegin();

    if( styleList != null )
    {

      Style[] styles = styleList.getStyles();
      if( styles != null )
      {
        Element e = createElement( namespace, "StyleList" );

        for( int i = 0; i < styles.length; i++ )
        {
          if( styles[i] != null )
          {
            Element s = createElement( namespace, "Style" );
            s.setAttribute( "current", stringValue01( styles[i].isCurrent() ) );

            if( styles[i].getName() != null )
            {
              Element ne = createElement( namespace, "Name" );
              ne.appendChild( createTextNode( styles[i].getName() ) );
              s.appendChild( ne );
            }
            if( styles[i].getTitle() != null )
            {
              Element ne = createElement( namespace, "Title" );
              ne.appendChild( createTextNode( styles[i].getTitle() ) );
              s.appendChild( ne );
            }
            e.appendChild( s );

          }
        }
        toNode.appendChild( e );
      }
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a list of a
   * <code>ViewContextCollection</code> to a <code>Node</code> using the
   * <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param vcc
   *          the <code>ViewContextCollection</code> to be appended as new
   *          element
   * @param namespace
   *          the namespace of the new element
   *  
   */
  protected static void appendViewContextCollection( Node toNode, ViewContextCollection vcc,
      String namespace )
  {

    Debug.debugMethodBegin();

    if( vcc != null )
    {
      Element e = createElement( namespace, "ViewContextCollection" );
      e.setAttributeNode( createAttribute( "xmlns", OGC_CONTEXT_NS ) );
      e.setAttributeNode( createAttribute( "xmlns:sld", SLD_NS ) );
      e.setAttributeNode( createAttribute( "xmlns:xlink", XLINK_NS ) );
      e.setAttributeNode( createAttribute( "xmlns:xsi", XSI_NS ) );
      e.setAttributeNode( createAttribute( "xmlns:xsi", XSI_NS ) );
      e.setAttributeNode( createAttribute( "version", "1.0.0" ) );

      ViewContextReference[] vcrs = vcc.getViewContextReferences();
      if( vcrs != null && vcrs.length > 0 )
      {
        for( int i = 0; i < vcrs.length; i++ )
        {
          if( vcrs[i] != null )
          {
            appendContextReference( e, vcrs[i], D_CONTEXT_NS );
          }
        }
      }
      toNode.appendChild( e );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a list of a
   * <code>ViewContextReference</code> to a <code>Node</code> using the
   * <code>namespace</code>. <p/>// TODO implement ID in VCR
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param vcr
   *          the <code>ViewContextReference</code> to be appended as new
   *          element
   * @param namespace
   *          the namespace of the new element
   *  
   */
  protected static void appendContextReference( Node toNode, ViewContextReference vcr,
      String namespace )
  {

    Debug.debugMethodBegin();

    if( vcr != null )
    {
      Element e = createElement( namespace, "ViewContextReference" );

      e.setAttributeNode( createAttribute( "version", "1.0.0" ) );

      String id = vcr.getTitle().replace( ' ', '_' ).toLowerCase();
      e.setAttributeNode( createAttribute( "id", id ) );

      Element t = createElement( namespace, "Title" );
      t.appendChild( createTextNode( vcr.getTitle() ) );
      e.appendChild( t );

      if( vcr.getContextURL() != null )
      {
        Element c = createElement( namespace, "ViewContextURL" );
        appendOnlineResource( c, vcr.getContextURL(), namespace );
        e.appendChild( c );
      }
      toNode.appendChild( e );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Creates a String representation ("0" or "1") of a boolean value.
   * 
   * @param value
   *          the input value
   * @return "0" or "1" if value is true or false, respectively
   */
  public static final String stringValue01( boolean value )
  {
    return value ? "1" : "0";
  }

  //***********************************************************************
  // BEGIN Deegree specific methods
  //***********************************************************************

  /**
   * Appends the XML representation of a list of a <code>GeneralExtension</code>
   * to a <code>Node</code> using the <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param genExt
   *          the <code>GeneralExtension</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   *  
   */
  protected static void appendGeneralExtension( Node toNode, GeneralExtension genExt,
      String namespace )
  {

    Debug.debugMethodBegin();

    if( genExt != null )
    {
      Element e = createElement( namespace, "Extension" );

      e.setAttribute( "xmlns:deegree", D_CONTEXT_NS );

      appendIOSettings( e, genExt.getIOSettings(), namespace );
      appendFrontend( e, genExt.getFrontend(), namespace );
      appendMapParameter( e, genExt.getMapParameter(), namespace );

      toNode.appendChild( e );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a list of a <code>IOSettings</code> to
   * a <code>Node</code> using the <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param ioSetts
   *          the <code>IOSettings</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   *  
   */
  protected static void appendIOSettings( Node toNode, IOSettings ioSetts, String namespace )
  {

    Debug.debugMethodBegin();

    if( ioSetts != null )
    {
      Element e = createElement( namespace, "deegree:IOSettings" );

      if( ioSetts.getRootDirectory() != null )
      {
        Element rd = createElement( namespace, "deegree:RootDirectory" );
        rd.appendChild( createTextNode( ioSetts.getRootDirectory() ) );
        e.appendChild( rd );
      }

      // TODO: ioSetts.getTempDirectory() , inexistent till now
      /*
       * if(ioSetts.getRootDirectory() != null ){ Element rd =
       * createElement(namespace,"deegree:TempDirectory"); rd.appendChild(
       * createTextNode( ioSetts.getRootDirectory() + "temp"));
       * e.appendChild(rd); }
       */

      appendDirectoryAccess( e, ioSetts.getDownloadDirectory(), "deegree:TempDirectory", namespace );
      appendDirectoryAccess( e, ioSetts.getDownloadDirectory(), "deegree:DownloadDirectory",
          namespace );
      appendDirectoryAccess( e, ioSetts.getSLDDirectory(), "deegree:SLDDirectory", namespace );
      appendDirectoryAccess( e, ioSetts.getPrintDirectory(), "deegree:PrintDirectory", namespace );

      toNode.appendChild( e );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a list of a <code>DirectoryAccess</code>
   * to a <code>Node</code> using the <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param dirAcc
   *          the <code>DirectoryAccess</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   *  
   */
  protected static void appendDirectoryAccess( Node toNode, DirectoryAccess dirAcc, String dirName,
      String namespace )
  {

    Debug.debugMethodBegin();

    if( dirAcc != null )
    {
      Element d = createElement( namespace, dirName );
      if( dirAcc.getDirectoryName() != null )
      {
        Element a = createElement( namespace, "deegree:Name" );
        a.appendChild( createTextNode( dirAcc.getDirectoryName() ) );
        d.appendChild( a );

      }
      if( dirAcc.getOnlineResource() != null )
      {
        Element a = createElement( namespace, "deegree:Access" );
        appendOnlineResource( a, dirAcc.getOnlineResource(), namespace );
        d.appendChild( a );
      }
      toNode.appendChild( d );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a list of a <code>Frontend</code> to a
   * <code>Node</code> using the <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param fEnd
   *          the <code>Frontend</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   *  
   */
  protected static void appendFrontend( Node toNode, Frontend fEnd, String namespace )
  {

    Debug.debugMethodBegin();

    if( fEnd != null )
    {
      Element e = createElement( namespace, "deegree:Frontend" );

      e.setAttribute( "scope", "JSP" );
      if( fEnd.getController() != null )
      {
        Element c = createElement( namespace, "deegree:Controller" );
        c.appendChild( createTextNode( fEnd.getController() ) );
        e.appendChild( c );
      }
      if( ( (JSPFrontend)fEnd ).getStyle() != null )
      {
        Element c = createElement( namespace, "deegree:Style" );
        c.appendChild( createTextNode( ( (JSPFrontend)fEnd ).getStyle() ) );
        e.appendChild( c );
      }
      if( ( (JSPFrontend)fEnd ).getHeader() != null )
      {
        Element c = createElement( namespace, "deegree:Header" );
        c.appendChild( createTextNode( ( (JSPFrontend)fEnd ).getHeader() ) );
        e.appendChild( c );
      }
      if( ( (JSPFrontend)fEnd ).getFooter() != null )
      {
        Element c = createElement( namespace, "deegree:Footer" );
        c.appendChild( createTextNode( ( (JSPFrontend)fEnd ).getFooter() ) );
        e.appendChild( c );
      }

      appendCommonJS( e, ( (JSPFrontend)fEnd ).getCommonJS(), namespace );

      appendButtons( e, ( (JSPFrontend)fEnd ).getButtons(), namespace );

      appendGUIArea( e, fEnd.getNorth(), "deegree:North", namespace );
      appendGUIArea( e, fEnd.getWest(), "deegree:West", namespace );
      appendGUIArea( e, fEnd.getCenter(), "deegree:Center", namespace );
      appendGUIArea( e, fEnd.getEast(), "deegree:East", namespace );
      appendGUIArea( e, fEnd.getSouth(), "deegree:South", namespace );

      toNode.appendChild( e );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a list of a <code>String[]</code> to a
   * <code>Node</code> using the <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param commonJS
   *          the <code>String[]</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   *  
   */
  protected static void appendCommonJS( Node toNode, String[] commonJS, String namespace )
  {

    Debug.debugMethodBegin();

    if( commonJS != null )
    {
      Element c = createElement( namespace, "deegree:CommonJS" );

      for( int i = 0; i < commonJS.length; i++ )
      {
        if( commonJS[i] != null )
        {
          Element n = createElement( namespace, "deegree:Name" );
          n.appendChild( createTextNode( commonJS[i] ) );
          c.appendChild( n );
        }
      }
      toNode.appendChild( c );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a list of a <code>String</code> to a
   * <code>Node</code> using the <code>namespace</code>. // TODO
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param buttons
   *          the <code>String</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   *  
   */
  protected static void appendButtons( Node toNode, String buttons, String namespace )
  {

    Debug.debugMethodBegin();

    if( buttons != null )
    {
      Element b = createElement( namespace, "deegree:Buttons" );
      b.appendChild( createTextNode( buttons ) );

      toNode.appendChild( b );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a list of a <code>GUIArea</code> to a
   * <code>Node</code> using the <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param guiArea
   *          the <code>GUIArea</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   *  
   */
  protected static void appendGUIArea( Node toNode, GUIArea guiArea, String name, String namespace )
  {

    Debug.debugMethodBegin();

    if( guiArea != null )
    {
      Element e = createElement( namespace, name );
      e.setAttribute( "hidden", String.valueOf( guiArea.isHidden() ) );

      Module[] mods = guiArea.getModules();
      if( mods != null )
      {
        for( int i = 0; i < mods.length; i++ )
        {
          if( mods[i] != null )
          {
            appendModule( e, mods[i], namespace );
          }
        }
      }

      toNode.appendChild( e );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a list of a <code>GUIArea</code> to a
   * <code>Node</code> using the <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param namespace
   *          the namespace of the new element
   *  
   */
  protected static void appendModule( Node toNode, Module mod, String namespace )
  {

    Debug.debugMethodBegin();

    if( mod != null )
    {
      Element m = createElement( namespace, "deegree:Module" );
      m.setAttribute( "hidden", String.valueOf( mod.isHidden() ) );

      m.setAttribute( "type", mod.getType() );
      m.setAttribute( "width", String.valueOf( (int)mod.getWidth() ) );
      m.setAttribute( "height", String.valueOf( (int)mod.getHeight() ) );
      m.setAttribute( "scrolling", String.valueOf( mod.getScrolling() ) );

      Element n = createElement( namespace, "deegree:Name" );
      n.appendChild( createTextNode( mod.getName() ) );
      m.appendChild( n );

      n = createElement( namespace, "deegree:Content" );
      n.appendChild( createTextNode( mod.getContent() ) );
      m.appendChild( n );

      appendModuleJSList( m, mod.getModuleJSList(), namespace );
      appendModuleConfiguration( m, mod.getModuleConfiguration(), namespace );
      appendParameterList( m, mod.getParameter(), namespace );

      toNode.appendChild( m );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a list of a
   * <code>ModuleConfiguration</code> to a <code>Node</code> using the
   * <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param modConf
   *          the <code>ModuleConfiguration</code> to be appended as new
   *          element
   * @param namespace
   *          the namespace of the new element
   *  
   */
  protected static void appendModuleConfiguration( Node toNode, ModuleConfiguration modConf,
      String namespace )
  {

    Debug.debugMethodBegin();

    if( modConf != null && modConf.getOnlineResource() != null )
    {
      Element e = createElement( namespace, "deegree:ModuleConfiguration" );
      appendOnlineResource( e, modConf.getOnlineResource(), namespace );
      toNode.appendChild( e );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a list of a <code>ParameterList</code>
   * to a <code>Node</code> using the <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param parList
   *          the <code>ParameterList</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   *  
   */
  protected static void appendParameterList( Node toNode, ParameterList parList, String namespace )
  {

    Debug.debugMethodBegin();

    if( parList != null && parList.getParameters().length > 0 )
    {

      Element e = createElement( namespace, "deegree:ParameterList" );

      Parameter[] pars = parList.getParameters();
      for( int i = 0; i < pars.length; i++ )
      {
        if( pars[i] != null )
        {
          Element p = createElement( namespace, "deegree:Parameter" );

          Element n = createElement( namespace, "deegree:Name" );
          String name = pars[i].getName();
          //name = name.substring(0,name.indexOf(':'));
          n.appendChild( createTextNode( name ) );
          p.appendChild( n );

          n = createElement( namespace, "deegree:Value" );
          n.appendChild( createTextNode( pars[i].getValue().toString() ) );
          p.appendChild( n );

          e.appendChild( p );
        }
      }
      toNode.appendChild( e );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a list of a <code>MapParameter</code>
   * to a <code>Node</code> using the <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param mapPar
   *          the <code>MapParameter</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   *  
   */
  protected static void appendMapParameter( Node toNode, MapParameter mapPar, String namespace )
  {

    Debug.debugMethodBegin();

    if( mapPar != null )
    {
      Element e = createElement( namespace, "deegree:MapParameter" );

      Element f = createElement( namespace, "deegree:OfferedInfoFormats" );
      appendFormats( f, mapPar.getOfferedInfoFormats(), namespace );
      e.appendChild( f );

      appendMapOperationFactors( e, mapPar.getOfferedZoomFactors(), "deegree:OfferedZoomFactor",
          namespace );
      appendMapOperationFactors( e, mapPar.getOfferedPanFactors(), "deegree:OfferedPanFactor",
          namespace );

      Element minScale = createElement( namespace, "deegree:MinScale" );
      minScale.appendChild( createTextNode( String.valueOf( mapPar.getMinScale() ) ) );
      e.appendChild( minScale );

      Element maxScale = createElement( namespace, "deegree:MaxScale" );
      maxScale.appendChild( createTextNode( String.valueOf( mapPar.getMaxScale() ) ) );
      e.appendChild( maxScale );

      toNode.appendChild( e );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a list of a <code>Format[]</code> to a
   * <code>Node</code> using the <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param formats
   *          the <code>Format[]</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   *  
   */
  protected static void appendFormats( Node toNode, Format[] formats, String namespace )
  {

    Debug.debugMethodBegin();

    if( formats != null )
    {
      for( int i = 0; i < formats.length; i++ )
      {
        if( formats[i] != null )
        {
          Element f = createElement( namespace, "deegree:Format" );

          // TODO is current or selected?
          if( formats[i].isCurrent() )
          {
            f.setAttribute( "selected", String.valueOf( formats[i].isCurrent() ) );
          }

          f.appendChild( createTextNode( formats[i].getName() ) );
          toNode.appendChild( f );
        }
      }
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a list of a
   * <code>MapOperationFactor</code> to a <code>Node</code> using the
   * <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param mapOpFac
   *          the <code>MapOperationFactor</code> to be appended as new
   *          element
   * @param namespace
   *          the namespace of the new element
   *  
   */
  protected static void appendMapOperationFactors( Node toNode, MapOperationFactor[] mapOpFac,
      String opName, String namespace )
  {

    Debug.debugMethodBegin();

    if( mapOpFac != null )
    {
      for( int i = 0; i < mapOpFac.length; i++ )
      {
        if( mapOpFac[i] != null )
        {

          Element mof = createElement( namespace, opName );
          Element f = createElement( namespace, "deegree:Factor" );
          f.appendChild( createTextNode( String.valueOf( mapOpFac[i].getFactor() ) ) );

          if( mapOpFac[i].isSelected() )
          {
            f.setAttribute( "selected", String.valueOf( mapOpFac[i].isSelected() ) );
          }

          // TODO isFree ???

          mof.appendChild( f );
          toNode.appendChild( mof );
        }
      }
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a list of a <code>LayerExtension</code>
   * to a <code>Node</code> using the <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param layExt
   *          the <code>LayerExtension</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   *  
   */
  protected static void appendLayerExtension( Node toNode, LayerExtension layExt, String namespace )
  {

    Debug.debugMethodBegin();

    if( layExt != null )
    {
      Element e = createElement( namespace, "Extension" );

      e.setAttribute( "xmlns:deegree", D_CONTEXT_NS );

      appendDataService( e, layExt.getDataService(), namespace );

      Element g = createElement( namespace, "deegree:MasterLayer" );
      g.appendChild( createTextNode( String.valueOf( layExt.isMasterLayer() ) ) );
      e.appendChild( g );

      toNode.appendChild( e );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a list of a <code>DataService</code> to
   * a <code>Node</code> using the <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param dataServ
   *          the <code>DataService</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   *  
   */
  protected static void appendDataService( Node toNode, DataService dataServ, String namespace )
  {

    Debug.debugMethodBegin();

    if( dataServ != null )
    {
      Element e = createElement( namespace, "deegree:DataService" );

      if( dataServ.getServer() != null )
      {
        appendServer( e, dataServ.getServer(), namespace );
      }
      String geoType = dataServ.getGeometryType();
      if( geoType != null )
      {
        Element g = createElement( namespace, "deegree:GeometryType" );
        g.appendChild( createTextNode( dataServ.getGeometryType() ) );
        e.appendChild( g );
      }
      String featType = dataServ.getFeatureType();
      if( featType != null )
      {
        Element g = createElement( namespace, "deegree:FeatureType" );
        g.appendChild( createTextNode( featType ) );
        e.appendChild( g );
      }

      toNode.appendChild( e );
    }
    Debug.debugMethodEnd();
  }

  /**
   * Appends the XML representation of a list of a <code>ParameterList</code>
   * to a <code>Node</code> using the <code>namespace</code>.
   * 
   * @param toNode
   *          the <code>Node</code> to append the new element to
   * @param modJSList
   *          the <code>modJSList</code> to be appended as new element
   * @param namespace
   *          the namespace of the new element
   *  
   */
  protected static void appendModuleJSList( Node toNode, String[] modJSList, String namespace )
  {

    Debug.debugMethodBegin();

    if( modJSList != null && modJSList.length > 0 )
    {

      for( int i = 0; i < modJSList.length; i++ )
      {
        if( modJSList[i] != null )
        {
          Element p = createElement( namespace, "deegree:ModuleJS" );
          p.appendChild( createTextNode( modJSList[i] ) );

          toNode.appendChild( p );
        }
      }
    }
    Debug.debugMethodEnd();
  }

}