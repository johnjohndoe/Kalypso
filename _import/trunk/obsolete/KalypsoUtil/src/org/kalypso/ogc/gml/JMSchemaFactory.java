package org.kalypso.ogc.gml;

import java.util.ArrayList;
import java.util.List;

import org.deegree.model.feature.FeatureType;
import org.kalypso.util.xml.XMLTools;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * DOCUMENT ME!
 * 
 * @author vdoemming To change the template for this generated type comment go
 *         to Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and
 *         Comments
 */
public class JMSchemaFactory
{
  private static final int UNSUPPORTED_TYPE = 0;

  private static final int COMPLEX_TYPE = 1;

  private static final int ELEMENT_REF = 2;

//  private static final int ELEMENT_SIMPLE_SCHEMA_TYPE = 3;

//  private static final int ELEMENT_SIMPLE_TYPEREF = 4;

//  private static final int ELEMENT_GEOMETRYTYPE = 5;

  private static final int COMPLEX_CONTENT = 6;

  private static final int EXTENSION = 7;

  private static final int SEQUENCE = 8;

//  private static final int FEATURETYPE = 9;

  private static final int IGNORED_TYPE = 10;

//  private static final int ELEMENT_SIMPLE_TYPE = 11;

  private static final int RESTRICTION_TYPE = 12;

  private static final int NAMED_ELEMENT = 13;

  private static final int ENUMERATION_TYPE = 14;

  public static final int NO_XLINK_TYPE = 15;

  public static final int XLINK_TYPE_SIMPLE = 16;

  public static final int XLINK_TYPE_EXTENDED = 17;

  public static final int XLINK_TYPE_LOCATOR = 18;

  public static final int XLINK_TYPE_ARC = 19;

  public static final int XLINK_TYPE_RESOURCE = 20;

  public static int getType( JMSchema schema, Node node ) throws Exception
  {
    int result = UNSUPPORTED_TYPE;

    switch( node.getNodeType() )
    {
    case Node.ELEMENT_NODE:
      result = getType_ElementNode( schema, node );

    default:
      break;
    }

    //	info("result is"+Integer.toString(result),node);
    return result;
  }

  public static FeatureType[] createFeatureTypes( JMSchema schema ) throws Exception
  {
    NodeList nl = schema.getSchema().getElementsByTagNameNS( "http://www.w3.org/2001/XMLSchema",
        "element" );
    nl = XMLTools.reduceByAttribute( nl, "substitutionGroup", "gml:_Feature" );

    List list = new ArrayList();

    for( int i = 0; i < nl.getLength(); i++ )
    {
      JMFeatureTypeBuilder builder = new JMFeatureTypeBuilder( schema, nl.item( i ) );

      FeatureType ft = builder.toFeatureType();

      if( ft != null )
        list.add( ft );
    }

    FeatureType[] result = new FeatureType[list.size()];

    for( int i = 0; i < list.size(); i++ )
    {
      result[i] = (FeatureType)list.get( i );
    }

    return result;
  }

  public static void map( JMSchema schema, NodeList nl, JMFeatureTypeBuilder collector )
      throws Exception
  {
    for( int i = 0; i < nl.getLength(); i++ )
      map( schema, nl.item( i ), collector );
  }

  public static void map( JMSchema schema, Node node, JMFeatureTypeBuilder collector )
      throws Exception
  {
    //		if(node.getNodeType()==Node.ELEMENT_NODE)
    //		    System.out.println( "map node:" + XMLTools.toString( node ) );
    switch( getType( schema, node ) )
    {
    case COMPLEX_TYPE:
      //				System.out.println("COMPLEX_TYPE");

      break;

    case COMPLEX_CONTENT:
      //				System.out.println("COMPLEX_CONTENT");

      break;

    case EXTENSION:
      //				System.out.println("EXTENSION");

      break;

    case SEQUENCE:
      //				System.out.println("SEQUENCE");

      break;

    case ELEMENT_REF:
    //				System.out.println("ELEMENT_REF");
    {
      JMAttribute refAttribute = new JMAttribute( schema, XMLTools.getAttributeNode( node, "ref" ) );

      if( refAttribute.getValueNS().equals( schema.getTargetNS() ) )
      {
        NodeList nl = schema.getSchema().getElementsByTagNameNS(
            "http://www.w3.org/2001/XMLSchema", "element" );
        nl = XMLTools.reduceByAttribute( nl, "name", refAttribute.getValue() );
        map( schema, nl, collector );
      }
      else
      {
        // TODO: support refs to other schemas/NS
      }
    }

      break;

    case NAMED_ELEMENT:
      collector.add( new JMFeatureTypeBuilder( schema, node ) );

      break;
    case XLINK_TYPE_SIMPLE:
    {
      //          <attributeGroup name="simpleLink">
      //          <attribute name="type" type="string" fixed="simple" form="qualified"/>
      //          <attribute ref="xlink:href" use="optional"/>
      //          <attribute ref="xlink:role" use="optional"/>
      //          <attribute ref="xlink:arcrole" use="optional"/>
      //          <attribute ref="xlink:title" use="optional"/>
      //          <attribute ref="xlink:show" use="optional"/>
      //          <attribute ref="xlink:actuate" use="optional"/>
      //        </attributeGroup>
      collector.setXLinkType( XLINK_TYPE_SIMPLE );
      break;
    }

    case RESTRICTION_TYPE:
    {
      Element element = (Element)node;

      //					System.out.println("RESTRICTION_TYPE");
      if( element.hasAttribute( "base" ) )
      {
        JMAttribute baseAttribute = new JMAttribute( schema, XMLTools.getAttributeNode( node,
            "base" ) );
        collector.setTypeName( baseAttribute );
      }
      else
        throw new Exception( "unknown Restriction: " + XMLTools.toString( node ) );

    }
      break;
    case ENUMERATION_TYPE:
    {
      Element element = (Element)node;
      //					System.out.println("ENUMERATION_TYPE");
      if( element.hasAttribute( "value" ) )
      {
        JMAttribute valueAttribute = new JMAttribute( schema, XMLTools.getAttributeNode( node,
            "value" ) );
        collector.addEnumerationObject( valueAttribute.getValue() );
      }
      else
        throw new Exception( "unknown Enumeration: " + XMLTools.toString( node ) );

    }
      break;

    case IGNORED_TYPE:
      //				System.out.println("IGNORED_TYPE");

      //nothing to do
      break;

    default:

      if( node.getNodeType() == Node.ELEMENT_NODE )
        throw ( new Exception( "unsupported node: " + node.getLocalName() + " \n"
            + node.getNodeName() + "\n" + node.getNodeValue() + "\n" + node.toString() ) );

    //UNSUPPORTED_TYPE
    }

    map( schema, node.getChildNodes(), collector );

  }

  private static int getType_ElementNode( JMSchema schema, Node node ) throws Exception
  {
    Element element = (Element)node;

    if( "complexType".equals( node.getLocalName() )
        && "http://www.w3.org/2001/XMLSchema".equals( node.getNamespaceURI() ) )
      return COMPLEX_TYPE;

    // element referenz
    if( "element".equals( node.getLocalName() )
        && "http://www.w3.org/2001/XMLSchema".equals( node.getNamespaceURI() )
        && element.hasAttribute( "ref" ) )
      return ELEMENT_REF;

    if( "element".equals( node.getLocalName() )
        && "http://www.w3.org/2001/XMLSchema".equals( node.getNamespaceURI() )
        && element.hasAttribute( "name" ) )
    {
      return NAMED_ELEMENT;
    }

    if( "complexContent".equals( node.getLocalName() ) )
      return IGNORED_TYPE;

    if( "extension".equals( node.getLocalName() ) )
      return IGNORED_TYPE;

    if( "sequence".equals( node.getLocalName() ) )
      return IGNORED_TYPE;

    if( "simpleType".equals( node.getLocalName() ) )
      return IGNORED_TYPE;

    if( "restriction".equals( node.getLocalName() ) )
      return RESTRICTION_TYPE;
    if( "enumeration".equals( node.getLocalName() ) )
      return ENUMERATION_TYPE;

    if( "attributeGroup".equals( node.getLocalName() ) && element.hasAttribute( "ref" ) )
    {
      JMAttribute refAttribute = new JMAttribute( schema, XMLTools.getAttributeNode( node, "ref" ) );
   
      if( "http://www.w3.org/1999/xlink".equals( refAttribute.getValueNS() ) )
      {
        if( "simpleLink".equals( refAttribute.getValue() ) )
          return XLINK_TYPE_SIMPLE;
        if( "extendedLink".equals( refAttribute.getValue() ) )
          return XLINK_TYPE_EXTENDED;
        if( "locatorLink".equals( refAttribute.getValue() ) )
          return XLINK_TYPE_LOCATOR;
        if( "arcLink".equals( refAttribute.getValue() ) )
          return XLINK_TYPE_ARC;
        if( "resourceLink".equals( refAttribute.getValue() ) )
          return XLINK_TYPE_RESOURCE;
      }
    }
    String message = "unsupported node:" + node.getLocalName() + "\n" + XMLTools.toString( node )
        + "\n" + node.getNodeName() + "\n" + node.getNodeValue() + "\n" + node.toString();

    throw new Exception( message );
  }
  /*
   * private static FeatureTypeProperty createFeatureTypeGeometryProperty(
   * JMSchema schema, Node node) { JMAttribute nameAttribute = new
   * JMAttribute(schema, XMLTools.getAttributeNode(node, "name")); JMAttribute
   * typeAttribute = new JMAttribute(schema, XMLTools.getAttributeNode(node,
   * "type")); String type =
   * Mapper.mapGMLSchemaType2JavaType(typeAttribute.getValue()); String name =
   * nameAttribute.getValue(); boolean nullable = true; // todo: support
   * nullable
   * 
   * return FeatureFactory.createFeatureTypeProperty(name, type, nullable); }
   */
  /*
   * private static FeatureTypeProperty createFeatureTypeSimpleSCHEMAProperty(
   * JMSchema jmSchema, Node node) throws Exception { JMAttribute nameAttribute =
   * new JMAttribute(jmSchema, XMLTools.getAttributeNode(node, "name"));
   * JMAttribute typeAttribute = new JMAttribute(jmSchema,
   * XMLTools.getAttributeNode(node, "type")); String type =
   * Mapper.mapXMLSchemaType2JavaType(typeAttribute.getValue()); String name =
   * nameAttribute.getValue(); boolean nullable = true; // todo: support
   * nullable
   * 
   * return FeatureFactory.createFeatureTypeProperty(name, type, nullable); }
   */

  //	private static void info(String text, Node node)
  //	{
  //		System.out.println(text + ":\n");
  //		System.out.println(" LocalNome:" + node.getLocalName());
  //		System.out.println(" NodeName:" + node.getNodeName());
  //		System.out.println(" NodeValue:" + node.getNodeValue());
  //		System.out.println(" NSURI:" + node.getNamespaceURI());
  //	}
}