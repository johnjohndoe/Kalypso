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

package org.deegree.xml;

// JDK 1.3
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashMap;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Attr;
import org.w3c.dom.CDATASection;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * XML Tools based on JAXP 1.1 for parsing documents and retrieving node
 * values/node attributes.
 * <p>
 * 
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$
 */
public class XMLTools
{

  /**
   * Checks if a given CDATA-value has to be escaped if it is used as a text
   * value in an XML element. If the submitted string contains a character that
   * have to be escaped or if the string is made of more than 1500 characters it
   * is encapsulated into a CDATA-section. Returns a version that is safe to be
   * used.
   * <p>
   * The method is just proofed for a UTF-8 character encoding.
   * <p>
   * 
   * @param cdata
   *          value to be used
   * @return the very same value (but escaped if necessary)
   */
  public static StringBuffer validateCDATA( String cdata )
  {
    StringBuffer sb = null;
    if( cdata != null
        && ( cdata.length() > 1000 || cdata.indexOf( '<' ) >= 0 || cdata.indexOf( '>' ) >= 0
            || cdata.indexOf( '&' ) >= 0 || cdata.indexOf( '"' ) >= 0 || cdata.indexOf( "'" ) >= 0 ) )
    {
      sb = new StringBuffer( cdata.length() + 15 );
      sb.append( "<![CDATA[" ).append( cdata ).append( "]]>" );
    }
    else
    {
      if( cdata != null )
      {
        sb = new StringBuffer( cdata );
      }
    }
    return sb;
  }

  /**
   * Returns the specified child element of the given elemen. If there are more
   * than one with the same name, the first one is returned.
   * <p>
   * 
   * @param name
   *          name of the child element
   * @param namespace
   *          namespace of the child element
   * @param node
   *          current element
   * 
   * @return the element or null, if it is missing
   * 
   * @throws XMLParsingException
   *           specified child element is missing and required is true
   */
  public static Element getRequiredChildByName( String name, String namespace, Node node )
      throws XMLParsingException
  {
    NodeList nl = node.getChildNodes();
    Element element = null;
    Element return_ = null;

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      for( int i = 0; i < nl.getLength(); i++ )
      {
        if( nl.item( i ) instanceof Element )
        {
          element = (Element)nl.item( i );
          String s = element.getNamespaceURI();
          if( ( s == null && namespace == null ) || ( namespace != null && namespace.equals( s ) ) )
          {
            if( element.getLocalName().equals( name ) )
            {
              return_ = element;
              break;
            }
          }
        }
      }
    }

    if( return_ == null )
    {
      throw new XMLParsingException( "Required child-element '" + name + "' of element '"
          + node.getNodeName() + "' is missing!" );
    }

    return return_;
  }

  /**
   * Returns the specified child element of the given elemen. If there are more
   * than one with the same name, the first one is returned.
   * <p>
   * 
   * @param name
   *          name of the child element
   * @param namespace
   *          namespace of the child element
   * @param node
   *          current element
   * 
   * @return the element or null, if it is missing
   */
  public static Element getChildByName( String name, String namespace, Node node )
  {

    NodeList nl = node.getChildNodes();
    Element element = null;
    Element return_ = null;

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      for( int i = 0; i < nl.getLength(); i++ )
      {
        if( nl.item( i ) instanceof Element )
        {
          element = (Element)nl.item( i );

          String s = element.getNamespaceURI();

          if( ( s == null && namespace == null ) || ( namespace != null && namespace.equals( s ) ) )
          {
            if( element.getLocalName().equals( name ) )
            {
              return_ = element;

              break;
            }
          }
        }
      }
    }
    return return_;
  }

  /**
   * Returns the specified child elements of the given element.
   * <p>
   * 
   * @param name
   *          name of the child elements
   * @param namespace
   *          namespace of the child elements
   * @param node
   *          current element
   * 
   * @return the list of matching child elements
   */
  public static ElementList getChildElementsByName( String name, String namespace, Node node )
  {

    NodeList nl = node.getChildNodes();
    Element element = null;
    ElementList elementList = new ElementList();

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      for( int i = 0; i < nl.getLength(); i++ )
      {
        if( nl.item( i ) instanceof Element )
        {
          element = (Element)nl.item( i );

          String s = element.getNamespaceURI();

          if( ( s == null && namespace == null ) || ( namespace != null && namespace.equals( s ) ) )
          {
            if( element.getLocalName().equals( name ) )
            {
              elementList.addElement( element );
            }
          }
        }
      }
    }
    return elementList;
  }

  /**
   * Returns the text contained in the specified element. The returned value is
   * trimmed by calling the trim() method of java.lang.String
   * <p>
   * 
   * @param node
   *          current element
   * 
   * @return the textual contents of the element or null, if it is missing
   */
  public static String getStringValue( Node node )
  {
    NodeList children = node.getChildNodes();
    StringBuffer sb = new StringBuffer( children.getLength() * 500 );

    for( int i = 0; i < children.getLength(); i++ )
    {
      if( children.item( i ).getNodeType() == Node.TEXT_NODE
          || children.item( i ).getNodeType() == Node.CDATA_SECTION_NODE )
      {
        sb.append( children.item( i ).getNodeValue() );
      }
    }

    return sb.toString().trim();
  }

  /**
   * Returns the text contained in the specified child element of the given
   * element.
   * <p>
   * 
   * @param name
   *          name of the child element
   * @param namespace
   *          namespace of the child element
   * @param node
   *          current element
   * @param defaultValue
   *          default value if element is missing
   * 
   * @return the textual contents of the element or the given default value, if
   *         missing
   */
  public static String getStringValue( String name, String namespace, Node node, String defaultValue )
  {

    String value = defaultValue;
    Element element = getChildByName( name, namespace, node );

    if( element != null )
    {
      value = getValue( element );
    }
    if( value == null || value.equals( "" ) )
    {
      value = defaultValue;
    }

    return value;
  }

  /**
   * Returns the text contained in the specified child element of the given
   * element.
   * <p>
   * 
   * @param name
   *          name of the child element
   * @param namespace
   *          namespace of the child element
   * @param node
   *          current element
   * 
   * @return the textual contents of the element or null, if it is missing
   */
  public static String getRequiredStringValue( String name, String namespace, Node node )
      throws XMLParsingException
  {
    Element element = getRequiredChildByName( name, namespace, node );
    return getValue( element );
  }

  /**
   * Returns the numerical value of the text contained in the specified child
   * element of the given element as a double (if it denotes a double).
   * <p>
   * 
   * @param name
   *          name of the child element
   * @param namespace
   *          namespace of the child element
   * @param node
   *          current element
   * @param defaultValue
   *          value to be used if the specified element is missing or it's value
   *          is not numerical
   * 
   * @return the textual contents of the element as a double-value
   */
  public static double getDoubleValue( String name, String namespace, Node node, double defaultValue )
  {
    double value = defaultValue;
    String textValue = getStringValue( name, namespace, node, null );

    if( textValue != null )
    {
      try
      {
        value = Double.parseDouble( textValue );
      }
      catch( NumberFormatException e )
      {}
    }

    return value;
  }

  /**
   * Returns the numerical value of the text contained in the specified child
   * element of the given element as a double (if it denotes a double).
   * <p>
   * 
   * @param name
   *          name of the child element
   * @param namespace
   *          namespace of the child element
   * @param node
   *          current element
   * @return the textual contents of the element as a double-value
   * 
   * @throws XMLParsingException
   *           specified child element is missing or the contained text does not
   *           denote a double value
   */
  public static double getRequiredDoubleValue( String name, String namespace, Node node )
      throws XMLParsingException
  {
    double value;
    String textValue = getRequiredStringValue( name, namespace, node );

    try
    {
      value = Double.parseDouble( textValue );
    }
    catch( NumberFormatException e )
    {
      throw new XMLParsingException( "Value ('" + textValue + "') of element '" + name
          + "' does not denote a valid double value." );
    }
    return value;
  }

  /**
   * Returns the value of the specified node attribute or null if it is missing.
   * <p>
   * 
   * @param name
   *          (local) name of attribute
   * @param node
   *          current element
   * @return the textual contents of the attribute or null
   */
  public static String getAttrValue( String name, Node node ) throws XMLParsingException
  {
    String value = null;
    NamedNodeMap atts = node.getAttributes();

    if( atts != null )
    {
      Attr attribute = (Attr)atts.getNamedItem( name );

      if( attribute != null )
      {
        value = attribute.getValue();
      }
    }
    return value;
  }

  /**
   * Returns the value of the specified node attribute.
   * <p>
   * 
   * @param name
   *          (local) name of attribute
   * @param node
   *          current element
   * @return the textual contents of the attribute
   * 
   * @throws XMLParsingException
   *           if specified attribute is missing
   */
  public static String getRequiredAttrValue( String name, Node node ) throws XMLParsingException
  {
    String value = null;
    NamedNodeMap atts = node.getAttributes();

    if( atts != null )
    {
      Attr attribute = (Attr)atts.getNamedItem( name );

      if( attribute != null )
      {
        value = attribute.getValue();
      }
    }
    if( value == null )
    {
      throw new XMLParsingException( "Required attribute '" + name + "' of element '"
          + node.getNodeName() + "' is missing." );
    }
    return value;
  }

  /**
   * Returns the value of the specified node attribute. // * FIXME: Due to
   * apparent bugs in getNamedItemNS (name, namespace), // * when used to find
   * attribute nodes, the current implementation // * uses a workaround.
   * <p>
   * 
   * @param name
   *          (local) name of attribute
   * @param namespace
   *          namespace of attribute
   * @param node
   *          current element
   * @return the textual contents of the attribute
   * 
   * @throws XMLParsingException
   *           if specified attribute is missing
   */
  public static String getRequiredAttrValue( String name, String namespace, Node node )
      throws XMLParsingException
  {
    String value = null;

    NamedNodeMap atts = node.getAttributes();

    if( atts != null )
    {
      Attr attribute = (Attr)atts.getNamedItemNS( namespace, name );

      if( attribute != null )
      {
        value = attribute.getValue();
      }
    }

    //        for (int i = 0; i < atts.getLength (); i++) {
    //            Node n = atts.item (i);
    //            System.out.println ("Nodename: " + toLocalName (n.getNodeName ()));
    //            System.out.println ("NamespaceURI: " + n.getNamespaceURI ());
    //            if (n.getNamespaceURI ().equals (namespace) && toLocalName (n.getNodeName
    // ()).equals (name)) {
    //                System.out.println ("Me is here!");
    //                value = ((Attr) n).getValue ();
    //                break;
    //            }
    //        }

    if( value == null )
    {
      throw new XMLParsingException( "Required attribute '" + namespace + ":" + name
          + "' of element '" + node.getNodeName() + "' is missing." );
    }
    return value;
  }

  /**
   * creates a new and empty dom document
   */
  public static Document create()
  {
    javax.xml.parsers.DocumentBuilder builder = null;

    try
    {
      DocumentBuilderFactory fac = DocumentBuilderFactory.newInstance();
      fac.setNamespaceAware( true );
      builder = fac.newDocumentBuilder();
    }
    catch( Exception ex )
    {
      System.out.println( ex );
    }

    Document doc = builder.newDocument();

    return doc;
  }

  /**
   * Returns the attribute value of the given node.
   * 
   * 
   * @param node
   * @param attrName
   * 
   * @return
   */
  public static String getAttrValue( Node node, String attrName )
  {
    // get attr name and dtype
    NamedNodeMap atts = node.getAttributes();

    if( atts == null )
    {
      return null;
    }

    Attr a = (Attr)atts.getNamedItem( attrName );

    if( a != null )
    {
      return a.getValue();
    }

    return null;
  }

  /**
   * Returns the attribute value of the given node.
   * 
   * 
   * @param node
   * @param attrName
   * 
   * @return
   */
  public static String getAttrValue( Node node, String namespace, String attrName )
  {
    // get attr name and dtype
    NamedNodeMap atts = node.getAttributes();

    if( atts == null )
    {
      return null;
    }

    Attr a = (Attr)atts.getNamedItemNS( namespace, attrName );

    if( a != null )
    {
      return a.getValue();
    }

    return null;
  }

  /**
   * Parses a XML document and returns a DOM object.
   * 
   * 
   * @param fileName
   *          the filename of the XML file to be parsed
   * 
   * @return a DOM object
   * 
   * @throws IOException
   * @throws SAXException
   *  
   */
  public static Document parse( String fileName ) throws IOException, SAXException
  {

    //        Reader reader = new InputStreamReader(new FileInputStream(fileName));
    //
    //        StringWriter stw = new StringWriter();
    //
    //        // remove all not writeable characters
    //        int c = -1;
    //        int cc = -1;
    //
    //        while ((c = reader.read ()) > -1) {
    //            if (c > 31) {
    //                if ((cc == 32) && (c == 32)) {
    //                } else {
    //                    stw.write (c);
    //                }
    //
    //                cc = c;
    //            }
    //        }
    //        reader.close();
    //
    //        // remove not need spaces (spaces between tags)
    //        StringBuffer sb = new StringBuffer(stw.toString ());
    //        stw.close ();
    //
    //        String s = sb.toString ();
    //
    //        while (s.indexOf ("> <") > -1) {
    //            int idx = s.indexOf ("> <");
    //            sb.replace (idx, idx + 3, "><");
    //            s = sb.toString ();
    //        }
    //
    //        Document doc = parse (new StringReader(s));
    Reader reader = new InputStreamReader( new FileInputStream( fileName ) );
    Document doc = parse( reader );

    return doc;
  }

  /**
   * Parses a XML document and returns a DOM object.
   * 
   * 
   * @param reader
   *          accessing the resource to parse
   * 
   * @return a DOM object
   * 
   * @throws IOException
   * @throws SAXException
   *  
   */
  public static Document parse( Reader reader ) throws IOException, SAXException
  {
    javax.xml.parsers.DocumentBuilder parser = null;

    try
    {
      DocumentBuilderFactory fac = DocumentBuilderFactory.newInstance();
      fac.setNamespaceAware( true );
      fac.setValidating( false );
      parser = fac.newDocumentBuilder();
    }
    catch( ParserConfigurationException ex )
    {
      ex.printStackTrace();
      throw new IOException( "Unable to initialize DocumentBuilder: " + ex.getMessage() );
    }

    Document doc = parser.parse( new InputSource( reader ) );
    reader.close();

    return doc;
  }

  /**
   * copies one node to another node (of a different dom document).
   */
  public static Node copyNode( Node source, Node dest )
  {
    //Debug.debugMethodBegin( "XMLTools", "copyNode" );
    if( source.getNodeType() == Node.TEXT_NODE )
    {
      Text tn = dest.getOwnerDocument().createTextNode( source.getNodeValue() );

      return tn;
    }
    else
    {
      NamedNodeMap attr = source.getAttributes();

      if( attr != null )
      {
        for( int i = 0; i < attr.getLength(); i++ )
        {
          ( (Element)dest ).setAttribute( attr.item( i ).getNodeName(), attr.item( i )
              .getNodeValue() );
        }
      }

      NodeList list = source.getChildNodes();

      for( int i = 0; i < list.getLength(); i++ )
      {
        if( !( list.item( i ) instanceof Text ) )
        {
          Element en = dest.getOwnerDocument().createElementNS( list.item( i ).getNamespaceURI(),
              list.item( i ).getNodeName() );

          if( list.item( i ).getNodeValue() != null )
          {
            en.setNodeValue( list.item( i ).getNodeValue() );
          }

          Node n = copyNode( list.item( i ), en );
          dest.appendChild( n );
        }
        else if( ( list.item( i ) instanceof CDATASection ) )
        {
          CDATASection cd = dest.getOwnerDocument().createCDATASection(
              list.item( i ).getNodeValue() );
          dest.appendChild( cd );
        }
        else
        {
          Text tn = dest.getOwnerDocument().createTextNode( list.item( i ).getNodeValue() );
          dest.appendChild( tn );
        }
      }
    }

    //Debug.debugMethodEnd();
    return dest;
  }

  /**
   * inserts a node into a dom element (of a different dom document)
   */
  public static Node insertNodeInto( Node source, Node dest )
  {

    Document dDoc = null;
    Document sDoc = source.getOwnerDocument();

    if( dest instanceof Document )
    {
      dDoc = (Document)dest;
    }
    else
    {
      dDoc = dest.getOwnerDocument();
    }

    if( dDoc.equals( sDoc ) )
    {
      dest.appendChild( source );
    }
    else
    {
      Element element = dDoc.createElementNS( source.getNamespaceURI(), source.getNodeName() );
      dest.appendChild( element );

      copyNode( source, element );
    }

    return dest;
  }

  /**
   * returns the first child element of the submitted node
   */
  public static Element getFirstElement( Node node )
  {

    NodeList nl = node.getChildNodes();
    Element element = null;

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      for( int i = 0; i < nl.getLength(); i++ )
      {
        if( nl.item( i ) instanceof Element )
        {
          element = (Element)nl.item( i );

          break;
        }
      }
    }

    return element;
  }

  /**
   * removes all direct child nodes of the submitted node with the also
   * submitted name
   */
  public static Node removeNamedChildNodes( Node node, String nodeName )
  {

    NodeList nl = node.getChildNodes();

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      for( int i = 0; i < nl.getLength(); i++ )
      {
        if( nl.item( i ).getNodeName().equals( nodeName ) )
        {
          node.removeChild( nl.item( i ) );
        }
      }
    }

    return node;
  }

  /**
   * returns the first child element of the submitted node
   */
  public static Element getNamedChild( Node node, String name )
  {
    //	Debug.debugMethodBegin( "XMLTools", "getNamedChild" );
    NodeList nl = node.getChildNodes();
    Element element = null;
    Element return_ = null;

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      for( int i = 0; i < nl.getLength(); i++ )
      {
        if( nl.item( i ) instanceof Element )
        {
          element = (Element)nl.item( i );

          if( element.getNodeName().equals( name ) )
          {
            return_ = element;

            break;
          }
        }
      }
    }

    //	Debug.debugMethodEnd();
    return return_;
  }

  /**
   * Returns the first child element of the submitted node that matches the
   * given namespace and name.
   */
  public static Element getNamedChild( Node node, String namespace, String name )
  {
    //	Debug.debugMethodBegin( "XMLTools", "getNamedChild" );
    NodeList nl = node.getChildNodes();
    Element element = null;
    Element return_ = null;

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      for( int i = 0; i < nl.getLength(); i++ )
      {
        if( nl.item( i ) instanceof Element )
        {
          element = (Element)nl.item( i );

          String s = element.getNamespaceURI();

          if( namespace.equals( s ) )
          {
            if( element.getLocalName().equals( name ) )
            {
              return_ = element;

              break;
            }
          }
        }
      }
    }

    //	Debug.debugMethodEnd();
    return return_;
  }

  /**
   * the method merges two or more XML-schema definitions into one
   * 'meta'-schema. for this it is nessecary that all into schemas uses the same
   * prefix for namespace: http://www.w3.org/2001/XMLSchema. the result schema
   * won't have a target namespace because the input schemas may use different
   * target namespace (it's very probably that they will).
   */
  public static Document mergeSchemas( Document[] schemas ) throws Exception
  {

    StringBuffer content = new StringBuffer();
    ArrayList attributes = new ArrayList();
    ArrayList imports = new ArrayList();
    HashMap nodes = new HashMap();
    String schemaNS = null;

    // merge schemas into one schema
    for( int i = 0; i < schemas.length; i++ )
    {
      Element lr = schemas[i].getDocumentElement();
      NamedNodeMap nnm = lr.getAttributes();

      // add attributes to the root element
      for( int j = 0; j < nnm.getLength(); j++ )
      {
        Attr attr = (Attr)nnm.item( j );
        String a = attr.getName() + "=\"" + attr.getValue() + "\" ";

        if( !attributes.contains( a ) && !attr.getName().equals( "targetNamespace" ) )
        {
          attributes.add( a );
        }

        // get schema namespace
        if( attr.getValue().equals( "http://www.w3.org/2001/XMLSchema" ) )
        {
          String s = attr.getName();
          int pos = s.indexOf( ":" );
          schemaNS = s.substring( pos + 1, s.length() );
        }
      }

      // get content --> type-, element-definitions
      NodeList nl = lr.getChildNodes();

      for( int j = 0; j < nl.getLength(); j++ )
      {
        if( nl.item( j ) instanceof Element )
        {
          if( !nl.item( j ).getLocalName().equals( "import" ) )
          {
            // get element, attribute and type nodes
            String s = DOMPrinter.nodeToString( nl.item( j ), null ).trim();

            if( nodes.get( s ) == null )
            {
              content.append( s );
              nodes.put( s, null );
            }
          }
          else
          {
            // get import nodes
            String s = DOMPrinter.nodeToString( nl.item( j ), null );

            if( !imports.contains( s ) )
            {
              imports.add( s );
            }
          }
        }
      }
    }

    // put it all together to a valid XML document
    StringBuffer sb = new StringBuffer();
    sb.append( "<" + schemaNS + ":schema " );

    for( int i = 0; i < attributes.size(); i++ )
    {
      sb.append( attributes.get( i ) );
    }

    sb.append( ">" );

    for( int i = 0; i < imports.size(); i++ )
    {
      sb.append( imports.get( i ) );
    }

    sb.append( content.toString() );
    sb.append( "</" + schemaNS + ":schema>" );

    StringReader sr = new StringReader( sb.toString() );
    Document schemaDoc = XMLTools.parse( sr );
    sr.close();

    return schemaDoc;
  }

  /**
   * Returns the concatenated Strings of all children that are TEXT_NODEs.
   */
  public static String getValue( Node node )
  {
    NodeList children = node.getChildNodes();
    StringBuffer sb = new StringBuffer( children.getLength() * 500 );

    for( int i = 0; i < children.getLength(); i++ )
    {
      if( children.item( i ).getNodeType() == Node.TEXT_NODE
          || children.item( i ).getNodeType() == Node.CDATA_SECTION_NODE )
      {
        sb.append( children.item( i ).getNodeValue() );
      }
    }

    return sb.toString().trim();
  }

  /**
   * Returns all child ELEMENTs.
   */
  public static ElementList getChildElements( Node node )
  {
    NodeList children = node.getChildNodes();
    ElementList list = new ElementList();

    for( int i = 0; i < children.getLength(); i++ )
    {
      if( children.item( i ).getNodeType() == Node.ELEMENT_NODE )
      {
        list.elements.add( children.item( i ) );
      }
    }

    return list;
  }

  /**
   * Appends a node and it's children to the given StringBuffer. Indentation is
   * added on recursion.
   */
  public static void appendNode( Node node, String indent, StringBuffer sb )
  {
    switch( node.getNodeType() )
    {
    case Node.DOCUMENT_NODE:
    {
      sb.append( "<?xml version=\"1.0\"?>\n" );

      Document doc = (Document)node;
      appendNode( doc.getDocumentElement(), "", sb );

      break;
    }

    case Node.ELEMENT_NODE:
    {
      String name = node.getNodeName();
      sb.append( indent + "<" + name );

      NamedNodeMap attributes = node.getAttributes();

      for( int i = 0; i < attributes.getLength(); i++ )
      {
        Node current = attributes.item( i );
        sb.append( " " + current.getNodeName() + "=\"" + current.getNodeValue() + "\"" );
      }

      sb.append( ">" );

      // Kinder durchgehen
      NodeList children = node.getChildNodes();

      if( children != null )
      {
        for( int i = 0; i < children.getLength(); i++ )
        {
          appendNode( children.item( i ), indent, sb );
        }
      }

      sb.append( indent + "</" + name + ">" );

      break;
    }

    case Node.TEXT_NODE:
    case Node.CDATA_SECTION_NODE:
    {
      String trimmed = node.getNodeValue().trim();

      if( !trimmed.equals( "" ) )
      {
        sb.append( indent + trimmed );
      }

      break;
    }

    case Node.PROCESSING_INSTRUCTION_NODE:
      break;

    case Node.ENTITY_REFERENCE_NODE:
      break;

    case Node.DOCUMENT_TYPE_NODE:
      break;
    }
  }

  /**
   * extracts the local name from a node name
   */
  public static String toLocalName( String nodeName )
  {
    int pos = nodeName.lastIndexOf( ':' );

    if( pos > -1 )
    {
      nodeName = nodeName.substring( pos + 1, nodeName.length() );
    }

    return nodeName;
  }
}