package org.deegree_impl.gml.schema;

import java.util.HashMap;

import org.deegree.model.feature.Annotation;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.xml.XMLTools;
import org.w3c.dom.Document;
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
  private static final boolean DEBUG = false;

  // TODO remove DEBUG-option
  private static final int UNSUPPORTED_TYPE = 0;

  private static final int COMPLEX_TYPE = 1;

  private static final int ELEMENT_REF = 2;

  private static final int COMPLEX_CONTENT = 3;

  private static final int SEQUENCE = 4;

  private static final int IGNORED_TYPE = 5;

  private static final int RESTRICTION_SIMPLETYPE = 6;

  private static final int RESTRICTION_SIMPLECONTENT = 7;

  private static final int RESTRICTION_COMPLEXCONTENT = 8;

  private static final int NAMED_ELEMENT = 9;

  private static final int ENUMERATION_TYPE = 10;

  public static final int NO_XLINK_TYPE = 17;

  public static final int XLINK_TYPE_SIMPLE = 18;

  public static final int XLINK_TYPE_EXTENDED = 19;

  public static final int XLINK_TYPE_LOCATOR = 20;

  public static final int XLINK_TYPE_ARC = 21;

  public static final int XLINK_TYPE_RESOURCE = 22;

  private static final int EXTENSION_SIMPLETYPE = 23;

  private static final int EXTENSION_COMPLEXCONTENT = 24;

  private static final int ANNOTATION_TYPE = 25;

  private static final int DOCUMENTATION_TYPE = 26;

  private static final int ATTRIBUTE_TYPE = 27;

  private static final int ATTRIBUTEGROUP_TYPE = 28;

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

  public static NodeList getRootElements( GMLSchema schema )
  {
    NodeList_Impl result = new NodeList_Impl();
    final Document document = schema.getSchema();
    NodeList childNodes = document.getElementsByTagNameNS( XMLHelper.XMLSCHEMA_NS, "element" );
    //NodeList childNodes2 = ((Element)document).getChildNodes();
    for( int i = 0; i < childNodes.getLength(); i++ )
    {
      Node node = childNodes.item( i );
      // root elements must be global
      if( !XMLHelper.isGlobalElementDefinition( node ) )
        continue;
      // root elements must not be abstract
      if( XMLHelper.isAbstractElementDefinition( node ) )
        continue;
      SchemaAttribute typeAttribute = new SchemaAttribute( schema, XMLHelper.getAttributeNode(
          node, "type" ) );
      // XML_SCHEMA_Types can not be Applicationschema Root-Elements
      if( XMLHelper.XMLSCHEMA_NS.equals( typeAttribute.getValueNS() ) )
        continue;
      //      System.out.println(XMLHelper.toString(node));
      Node cNode = schema.getContentNode( typeAttribute.getValueNS(), typeAttribute.getValue() );
      //      System.out.println(XMLHelper.toString(cNode));
      // ComplexType must not be abstract
      if( XMLHelper.isAbstractElementDefinition( cNode ) )
        continue;
      String baseType = XMLHelper.getGMLBaseType( schema, cNode );
      //      System.out.println("BaseType: "+baseType);
      if( "AbstractFeatureType".equals( baseType ) )
        result.add( node );
    }
    return result;
  }

  public static void createFeatureTypes( GMLSchema schema, HashMap ftMap ) throws Exception
  {
    //    NodeList nl = schema.getSchema().getElementsByTagNameNS(
    // "http://www.w3.org/2001/XMLSchema",
    //        "element" );
    //    nl = XMLHelper.reduceByAttribute( nl, "substitutionGroup", "gml:_Feature"
    // );

    NodeList nl = GMLSchemaFactory.getRootElements( schema );

    for( int i = 0; i < nl.getLength(); i++ )
    {
      if( DEBUG )
      {
        String debugTxt = XMLHelper.toString( nl.item( i ) );
        if( !"".equals( debugTxt ) )
        {
          System.out.print( "\n\n" + debugTxt + "\n(build it)" );
          System.out.println();
        }
      }
      FeatureType ft = (FeatureType)schema.getMappedType( nl.item( i ) );
      //      FeatureTypeBuilder builder = new FeatureTypeBuilder( schema, nl.item( i
      // ) );
      //      FeatureType ft = builder.toFeatureType();
      if( ft != null )
        ftMap.put( ft.getName(), ft );
    }
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
    if( DEBUG )
    {
      String debugTxt = XMLHelper.toString( node ).trim();
      if( !"<?xml version=\"1.0\" encoding=\"UTF-8\"?>".equals( debugTxt ) )
      {
        String[] strings = debugTxt.split( ">" );
        System.out.print( "\n\n" + strings[1] + "|\n(map it)" );
        //      System.out.println( );
      }
    }

    switch( getType( schema, node ) )
    {
    case COMPLEX_TYPE:
      //<complexType name="FeatureAssociationType">
      Node nameNode = XMLHelper.getAttributeNode( node, "name" );
      SchemaAttribute nameAttribute = new SchemaAttribute( schema, nameNode );
      if(
      //          XMLHelper.GMLSCHEMA_NS.equals(nameAttribute.getValueNS())
      //          &&
      "FeatureAssociationType".equals( nameAttribute.getValue() ) )
        collector.setFeatureAssociation();
      break;

    case COMPLEX_CONTENT:

      break;

    case SEQUENCE:

      break;

    case ELEMENT_REF:
    {
      SchemaAttribute refAttribute = new SchemaAttribute( schema, XMLHelper.getAttributeNode( node,
          "ref" ) );
      if( DEBUG )
        System.out.println( refAttribute.getValueNS() );

      //      ITypeRegistry typeRegistry = TypeRegistrySingleton.getTypeRegistry();
      //      String valueNS = refAttribute.getValueNS();
      //      String value = refAttribute.getValue();
      //      ITypeHandler typeHandlerForTypeName =
      // typeRegistry.getTypeHandlerForTypeName(valueNS+":"+value);
      //      if(typeHandlerForTypeName!=null)
      //      {
      //      	    System.out.println("debug");
      //        	String className = typeHandlerForTypeName.getClassName();
      //      		FeatureTypeProperty property =
      // FeatureFactory.createFeatureTypeProperty("name","namespace",className,true);
      //        	collector.add(property);
      //      }
      //  else
      {
        GMLSchema refSchema = schema.getGMLSchema( refAttribute.getValueNS() );
        NodeList nl = refSchema.getSchema().getElementsByTagNameNS(
            "http://www.w3.org/2001/XMLSchema", "element" );
        nl = XMLHelper.reduceByAttribute( nl, "name", refAttribute.getValue() );
        //      NodeList_Impl nl2=new NodeList_Impl();
        // must refer to a global element definition, so filter for it
        Node refNode = null;
        for( int i = 0; i < nl.getLength(); i++ )
          if( XMLHelper.isGlobalElementDefinition( nl.item( i ) ) )
            refNode = nl.item( i );

        if( refNode != null )
        {
          //       map( refSchema, refNode, collector );
          Object mapedElement = refSchema.getMappedType( refNode );
          if( mapedElement instanceof FeatureType )
          {
            FeatureType ft = (FeatureType)mapedElement;
            collector.setOccurency( ft.getName(), ft.getNamespace(), node );
            collector.add( ft );
          }
          // are we allready building this node ?
          // (recursive definition ?)
          else if( mapedElement instanceof Node )
          {
            //refSchema.
            // FeatureType ft=(FeatureType)mapedElement;
            String name = ( (Element)refNode ).getAttribute( "name" );
            collector.setOccurency( name, refSchema.getTargetNS(), node );
            collector.add( refNode );
          }
          else if( mapedElement instanceof FeatureTypeProperty )
          {
            FeatureTypeProperty ftp = (FeatureTypeProperty)mapedElement;

            collector.setOccurency( ftp.getName(), ftp.getNamespace(), node );
            collector.add( ftp );

            if( mapedElement instanceof CustoumFeatureTypeProperty )
              return;

          }
          else
            throw new Exception( "refered element is nether featuretype "
                + "nor featuretypeproperty :" + mapedElement.getClass().toString() );

          //FeatureTypeBuilder childCollector = new FeatureTypeBuilder(
          // refSchema, refNode );
          //collector.setOccurency( childCollector.getName(),
          // childCollector.getNamespace(), node );
          //collector.add( childCollector );

        }
      }
    }

      break;

    case NAMED_ELEMENT:
      FeatureTypeBuilder childCollector = new FeatureTypeBuilder( schema, node );
      map(schema,node.getChildNodes(),childCollector);
      collector.setOccurency( childCollector.getName(), childCollector.getNamespace(), node );
      collector.add( childCollector );
      return;
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

    //    case EXTENSION:
    //
    //      break;
    case EXTENSION_COMPLEXCONTENT:
      mapBaseType( schema, node, collector );
      //
      break;
    case RESTRICTION_SIMPLECONTENT:
      if( DEBUG )
        System.out.println( "TODO map restriction simplecontent" );
    //TODO break;
    case RESTRICTION_COMPLEXCONTENT:
      // RESTRICTION_COMPLEXCONTENT redefines the complete contents of the
      // basetype,
      // but still keeps all restrictions from the basetype.
      // The redifinition must be a subset (restriction) of the original
      // basetype,
      // but we assume that the schema is valid and so do not check this)
      // 1. extend from basetype to collect all restrictions
      mapBaseType( schema, node, collector );
      // 2. remove contents but not restrictions
      collector.removeContents();
      // 3. parse new contents, will happen automatic
      break;
    case RESTRICTION_SIMPLETYPE:
    {
      Element element = (Element)node;
      if( element.hasAttribute( "base" ) )
      {
        SchemaAttribute baseAttribute = new SchemaAttribute( schema, XMLHelper.getAttributeNode(
            node, "base" ) );
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
        SchemaAttribute valueAttribute = new SchemaAttribute( schema, XMLHelper.getAttributeNode(
            node, "value" ) );
        collector.addEnumerationObject( valueAttribute.getValue() );
      }
      else
        throw new Exception( "unknown Enumeration: " + XMLHelper.toString( node ) );

    }
      break;
    case ANNOTATION_TYPE:
      if( DEBUG )
        System.out.println( "TODO annotation" );
      // TODO support annotations
      break;
    case DOCUMENTATION_TYPE:     
      collector.addAnnotation( createAnnotation( (Element)node ) );
      return;
    // TODO support doucumentations
    case ATTRIBUTE_TYPE:
      if( DEBUG )
        System.out.println( "TODO map attribute" );
      // TODO support attributes
      break;
    case ATTRIBUTEGROUP_TYPE:
      if( DEBUG )
        System.out.println( "TODO map attributegroup" );
      // TODO support attributes
      break;

    case IGNORED_TYPE:
      if( DEBUG )
        System.out.println( "nothing todo" );
      //nothing to do
      break;

    default:

      if( node.getNodeType() == Node.ELEMENT_NODE )
        throw ( new Exception( "unsupported node: " + node.getLocalName() + " \n"
            + node.getNodeName() + "\n" + node.getNodeValue() + "\n" + node.toString() ) );

    //UNSUPPORTED_TYPE
    }

    map( schema, node.getChildNodes(), collector );

  } //  private static void mapGMLExtension( String typeName, FeatureTypeBuilder

  private static Annotation createAnnotation( Element element )
  {
    String lang = element.getAttributeNS( "http://www.w3.org/XML/1998/namespace", "lang" );
    final String tooltip = XMLHelper.getStringFromChildElement( element, "http://www.w3.org/2001/XMLSchema", "tooltip" );
    final String label = XMLHelper.getStringFromChildElement( element, "http://www.w3.org/2001/XMLSchema", "label" );
    final String description = XMLHelper.getStringFromChildElement( element, "http://www.w3.org/2001/XMLSchema", "description" );

    return new Annotation( lang, label, tooltip, description );
  }

  // collector )
  //  {
  //    if("AbstractFeatureType".equals(typeName))
  //      {
  //// <element ref="gml:description" minOccurs="0"/>
  //// <element ref="gml:name" minOccurs="0"/>
  //// <element ref="gml:boundedBy" minOccurs="0"/>
  //        FeatureFactory.createFeatureTypeProperty("name",XMLHelper.GMLSCHEMA_NS,"java.lang.String",
  // false);
  //      }
  //  }
  private static void mapBaseType( GMLSchema schema, Node node, FeatureTypeBuilder collector )
      throws Exception
  {
    SchemaAttribute base = new SchemaAttribute( schema, XMLHelper.getAttributeNode( node, "base" ) );
    if( XMLHelper.XMLSCHEMA_NS.equals( base.getValueNS() ) )
      throw new UnsupportedOperationException( "TODO: map " + XMLHelper.toString( node ) );
    //       if(XMLHelper.GMLSCHEMA_NS.equals(base.getValueNS()))
    //           mapGMLExtension(base.getValue() , collector );
    // map base type and then continue mapping complex content
    String typeNamespace = base.getValueNS();
    String typeName = base.getValue();

    GMLSchema typeSchema = schema.getGMLSchema( typeNamespace );
    Node typeNode = typeSchema.getContentNode( typeNamespace, typeName );
    //      if( typeNode == null )
    //        System.out.println( "typeNode==null" );//
    //  System.out.println( XMLHelper.toString( typeNode ) );
    map( typeSchema, typeNode, collector );
  }

  private static int getType_ElementNode( GMLSchema schema, Node node ) throws Exception
  {
    Element element = (Element)node;
    String parentNodeName = element.getParentNode().getLocalName();

    final String localName = node.getLocalName();
    if( "complexType".equals( localName )
        && "http://www.w3.org/2001/XMLSchema".equals( node.getNamespaceURI() ) )
      return COMPLEX_TYPE;

    // element referenz
    if( "element".equals( localName )
        && "http://www.w3.org/2001/XMLSchema".equals( node.getNamespaceURI() )
        && element.hasAttribute( "ref" ) )
      return ELEMENT_REF;

    if( "element".equals( localName )
        && "http://www.w3.org/2001/XMLSchema".equals( node.getNamespaceURI() )
        && element.hasAttribute( "name" ) )
    {
      return NAMED_ELEMENT;
    }

    if( "complexContent".equals( localName ) )
      return IGNORED_TYPE;

    if( "extension".equals( localName ) )
    {
      if( "simpleType".equals( parentNodeName ) )
        return EXTENSION_SIMPLETYPE;
      if( "complexContent".equals( parentNodeName ) )
        return EXTENSION_COMPLEXCONTENT;
    }
    if( "restriction".equals( localName ) )
    {
      if( "complexContent".equals( parentNodeName ) )
        return RESTRICTION_COMPLEXCONTENT;
      if( "simpleContent".equals( parentNodeName ) )
        return RESTRICTION_SIMPLECONTENT;
      if( "simpleType".equals( parentNodeName ) )
        return RESTRICTION_SIMPLETYPE;
    }
    if( "sequence".equals( localName ) )
      return IGNORED_TYPE;

    if( "simpleType".equals( localName ) )
      return IGNORED_TYPE;

    if( "enumeration".equals( localName ) )
      return ENUMERATION_TYPE;

    if( "annotation".equals( localName ) )
      return ANNOTATION_TYPE;

    if( "documentation".equals( localName ) )
      return DOCUMENTATION_TYPE;

    if( "attribute".equals( localName ) )
      return ATTRIBUTE_TYPE;

    if( "attributeGroup".equals( localName ) && element.hasAttribute( "ref" ) )
    {
      SchemaAttribute refAttribute = new SchemaAttribute( schema, XMLHelper.getAttributeNode( node,
          "ref" ) );

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
      else
        return ATTRIBUTEGROUP_TYPE;
    }

    String message = "unsupported node:" + localName + "\n" + XMLHelper.toString( node ) + "\n"
        + node.getNodeName() + "\n" + node.getNodeValue() + "\n" + node.toString();

    throw new Exception( message );
  }

}