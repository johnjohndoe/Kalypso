package org.deegree_impl.gml.schema;

import java.util.ArrayList;
import java.util.List;

import org.deegree.model.feature.FeatureType;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * 
 * @author doemming
 * 
 */
public class GMLSchemaFactory
{
  private static final int UNSUPPORTED_TYPE = 0;

  private static final int COMPLEX_TYPE = 1;

  private static final int ELEMENT_REF = 2;

  private static final int COMPLEX_CONTENT = 6;

  private static final int EXTENSION = 7;

  private static final int SEQUENCE = 8;

  private static final int IGNORED_TYPE = 10;

  private static final int RESTRICTION_TYPE = 12;

  private static final int NAMED_ELEMENT = 13;

  private static final int ENUMERATION_TYPE = 14;

  public static final int NO_XLINK_TYPE = 15;

  public static final int XLINK_TYPE_SIMPLE = 16;

  public static final int XLINK_TYPE_EXTENDED = 17;

  public static final int XLINK_TYPE_LOCATOR = 18;

  public static final int XLINK_TYPE_ARC = 19;

  public static final int XLINK_TYPE_RESOURCE = 20;

  private static int getType( GMLSchema schema, Node node ) throws Exception
  {
    int result = UNSUPPORTED_TYPE;

    switch( node.getNodeType() )
    {
    case Node.ELEMENT_NODE:
      result = getType_ElementNode( schema, node );

    default:
      break;
    }
  return result;
  }

  public static FeatureType[] createFeatureTypes( GMLSchema schema ) throws Exception
  {
    NodeList nl = schema.getSchema().getElementsByTagNameNS( "http://www.w3.org/2001/XMLSchema",
        "element" );
    nl = XMLHelper.reduceByAttribute( nl, "substitutionGroup", "gml:_Feature" );

    List list = new ArrayList();

    for( int i = 0; i < nl.getLength(); i++ )
    {
      FeatureTypeBuilder builder = new FeatureTypeBuilder( schema, nl.item( i ) );

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

  private static void map( GMLSchema schema, NodeList nl, FeatureTypeBuilder collector )
      throws Exception
  {
    for( int i = 0; i < nl.getLength(); i++ )
      map( schema, nl.item( i ), collector );
  }

  public static void map( GMLSchema schema, Node node, FeatureTypeBuilder collector )
      throws Exception
  {
    switch( getType( schema, node ) )
    {
    case COMPLEX_TYPE:
   
      break;

    case COMPLEX_CONTENT:
  
      break;

    case EXTENSION:
   
      break;

    case SEQUENCE:
   
      break;

    case ELEMENT_REF:
    {
      SchemaAttribute refAttribute = new SchemaAttribute( schema, XMLHelper.getAttributeNode( node, "ref" ) );

      if( refAttribute.getValueNS().equals( schema.getTargetNS() ) )
      {
        NodeList nl = schema.getSchema().getElementsByTagNameNS(
            "http://www.w3.org/2001/XMLSchema", "element" );
        nl = XMLHelper.reduceByAttribute( nl, "name", refAttribute.getValue() );
        map( schema, nl, collector );
      }
      else
      {
        // TODO: support refs to other schemas/NS
      }
    }

      break;

    case NAMED_ELEMENT:
      collector.add( new FeatureTypeBuilder( schema, node ) );

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

      if( element.hasAttribute( "base" ) )
      {
        SchemaAttribute baseAttribute = new SchemaAttribute( schema, XMLHelper.getAttributeNode( node,
            "base" ) );
        collector.setTypeName( baseAttribute );
      }
      else
        throw new Exception( "unknown Restriction: " + XMLHelper.toString( node ) );

    }
      break;
    case ENUMERATION_TYPE:
    {
      Element element = (Element)node;
       if( element.hasAttribute( "value" ) )
      {
        SchemaAttribute valueAttribute = new SchemaAttribute( schema, XMLHelper.getAttributeNode( node,
            "value" ) );
        collector.addEnumerationObject( valueAttribute.getValue() );
      }
      else
        throw new Exception( "unknown Enumeration: " + XMLHelper.toString( node ) );

    }
      break;

    case IGNORED_TYPE:
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

  private static int getType_ElementNode( GMLSchema schema, Node node ) throws Exception
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
      SchemaAttribute refAttribute = new SchemaAttribute( schema, XMLHelper.getAttributeNode( node, "ref" ) );
   
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
    String message = "unsupported node:" + node.getLocalName() + "\n" + XMLHelper.toString( node )
        + "\n" + node.getNodeName() + "\n" + node.getNodeValue() + "\n" + node.toString();

    throw new Exception( message );
  }
  
}