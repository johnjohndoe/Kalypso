package org.deegree_impl.gml.schema;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.deegree.model.feature.Annotation;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree_impl.extension.ITypeHandler;
import org.deegree_impl.extension.TypeRegistrySingleton;
import org.deegree_impl.model.feature.FeatureAssociationTypeProperty_Impl;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.feature.XLinkFeatureTypeProperty;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Helper class to build featuretype. It collects informations from
 * schema-definition and builds FeatureType objects.
 * 
 * @author doemming
 */
public class FeatureTypeBuilder
{
  private GMLSchema m_schema;

  private final Map m_annotationMap = new HashMap();

  private final List m_enumeration = new ArrayList();

  private final List m_featureProtoTypes = new ArrayList();

  private final String m_name;

  private final String m_namespace;

  private String m_typeName = null;

  private XLinkFeatureTypeProperty m_XLinkProp = null;

  private final HashMap m_minOccurs;

  private final HashMap m_maxOccurs;

  private boolean m_isCutoumType = false;

  private String m_substitutionGroup = null;

  public FeatureTypeBuilder( GMLSchema schema, Node node ) throws Exception
  {
    m_minOccurs = new HashMap();
    m_maxOccurs = new HashMap();
    m_schema = schema;
    m_typeName = null;
    m_namespace = schema.getTargetNS();
    Element element = (Element)node;

    if( !element.hasAttribute( "name" ) || !element.hasAttribute( "type" ) )
      throw new Exception( "no valid elementnode: " + XMLHelper.toString( node ) );

    // set Name
    SchemaAttribute nameAttribute = new SchemaAttribute( m_schema, XMLHelper.getAttributeNode(
        node, "name" ) );
    m_name = nameAttribute.getValue();

    // set substitutiongroup
    final Node substitutionGroupNode = XMLHelper.getAttributeNode( node, "substitutionGroup" );

    if( substitutionGroupNode != null )
    {
      final SchemaAttribute substitueAttribute = new SchemaAttribute( m_schema,
          substitutionGroupNode );
      String substitutionNS = substitueAttribute.getValueNS();
      String substitutionName = substitueAttribute.getValue();
      m_substitutionGroup = substitutionNS + ":" + substitutionName;
    }

    // set Type
    SchemaAttribute typeAttribute = new SchemaAttribute( schema, XMLHelper.getAttributeNode( node,
        "type" ) );
    setTypeName( typeAttribute );
    //   setOccurency(node);
    // set cardinality
    // min:
    //    System.out.println( "------------------------------------------" );
    //    System.out.println( "created feature type: " + m_name + " : " +
    // m_typeName );
    //    System.out.println( "------------------------------------------" );
  }

  public void setOccurency( String name, String namespace, Node node )
  {
    Node minNode = XMLHelper.getAttributeNode( node, "minOccurs" );
    String minOccurs = "1";
    String maxOccurs = "1";
    if( minNode != null )
    {
      SchemaAttribute minOccursAttribute = new SchemaAttribute( m_schema, minNode );
      minOccurs = minOccursAttribute.getValue();
    }
    // set cardinality
    // max:
    Node maxNode = XMLHelper.getAttributeNode( node, "maxOccurs" );
    if( maxNode != null )
    {
      SchemaAttribute maxOccursAttribute = new SchemaAttribute( m_schema, maxNode );
      maxOccurs = maxOccursAttribute.getValue();
    }
    m_minOccurs.put( namespace + ":" + name, minOccurs );
    m_maxOccurs.put( namespace + ":" + name, maxOccurs );
  }

  public void setTypeName( SchemaAttribute typeAttribute ) throws Exception
  {
    final String typeName = typeAttribute.getValue();
    final String valueNS = typeAttribute.getValueNS();

    // is registred type ?
    final String typename = valueNS + ":" + typeName;
    final ITypeHandler typeHandler = TypeRegistrySingleton.getTypeRegistry()
        .getTypeHandlerForTypeName( typename );
    if( typeHandler != null )
    {
      m_typeName = typeHandler.getClassName();
      //      if(m_typeName==null)
      //      	System.out.println("debug");
      m_isCutoumType = true;
      return;
    }
    // type is XML SCHEMA TYPE // TODO let typeHandler do this
    if( XMLHelper.XMLSCHEMA_NS.equals( valueNS ) )
    {
      m_typeName = Mapper.mapXMLSchemaType2JavaType( typeName );
      return;
    }
    // type is GML SCHEMA TYPE
    else if( XMLHelper.GMLSCHEMA_NS.equals( valueNS ) )
    {
      String gmlTypeName = Mapper.mapGMLSchemaType2JavaType( typeName );
      if( gmlTypeName != null )
        m_typeName = gmlTypeName;
      return;
    }

    final GMLSchema schema = m_schema.getGMLSchema( valueNS );
    final Node cNode = schema.getContentNode( typeName );
    if( cNode == null )
      throw new Exception( "No content node for typeName: " + typeName );

    if( "complexType".equals( ( (Element)cNode ).getLocalName() ) )
    {
      String baseType = XMLHelper.getGMLBaseType( schema, cNode );
      if( baseType != null )
      { // a GMLBaseType
        m_typeName = baseType;
        //        m_typeName = "org.deegree.model.Feature";
        GMLSchemaFactory.map( schema, cNode, this );
        return;
      }

      NodeList custoumElement = ( (Element)cNode ).getElementsByTagNameNS( XMLHelper.XMLSCHEMA_NS,
          "element" );
      ///////////
      Node custoumNode = custoumElement.item( 0 );

      SchemaAttribute innerTypeAttribute = new SchemaAttribute( schema, XMLHelper.getAttributeNode(
          custoumNode, "type" ) );
      setTypeName( innerTypeAttribute );
      return;
      //      	custoumNode.getAttributes();xx
      //      	final String typeName = typeAttribute.getValue();
      //        final String valueNS = typeAttribute.getValueNS();
      //
      //        // is registred type ?
      //        final String typename = valueNS + ":" + typeName;
      //
      //      	final ITypeHandler typeHandler =
      // TypeRegistrySingleton.getTypeRegistry()
      //	        .getTypeHandlerForTypeName( typename );
      //	    if( typeHandler != null )
      //	    {
      //	      m_typeName = typeHandler.getClassName();
      //	      if(m_typeName==null)
      //	      	System.out.println("debug");
      //	      m_isCutoumType=true;
      //	      return;
      //	    }
      //      	custoum
      // a custoum base type ?
      ///////////

    }
    else if( "simpleType".equals( ( (Element)cNode ).getLocalName() ) )
    {
      m_typeName = "org.deegree.model.FeatureProperty";
      GMLSchemaFactory.map( schema, cNode, this );
      return;
    }
    else
      throw new Exception( "content of element not found in Schema " + typeAttribute.toString() );
  }

  public String getName()
  {
    return m_name;
  }

  public String getNamespace()
  {
    return m_namespace;
  }

  //  public void add( FeatureTypeProperty featureTypeProperty )
  //  {
  //    m_featureProtoTypes.add( featureTypeProperty );
  //  }
  public void add( FeatureTypeProperty ftp )
  {
    m_featureProtoTypes.add( ftp );
  }

  public void add( FeatureType ft )
  {
    m_featureProtoTypes.add( ft );
  }

  public void add( Node recursiveElementNode )
  {
    m_featureProtoTypes.add( recursiveElementNode );
  }

  public void add( FeatureTypeBuilder featureProtoType )
  {
    m_featureProtoTypes.add( featureProtoType );

  }

  public void addEnumerationObject( String allowedContent )
  {
    m_enumeration.add( allowedContent );
  }

  /**
   * removes contents, but not restrictions, attributes or enumerations
   */
  public void removeContents()
  {

    m_featureProtoTypes.clear();

  }

  public FeatureTypeProperty toFeatureTypeProperty()
  {
    if( "FeatureAssociationType".equals( m_typeName ) )
    {
      Object associateableFeatureType = m_featureProtoTypes.get( 0 );
      FeatureType ftp = null;
      if( associateableFeatureType instanceof FeatureType )
      {
        ftp = (FeatureType)associateableFeatureType;
        return new FeatureAssociationTypeProperty_Impl( m_name, m_namespace, m_typeName, true,
            m_schema, ftp, m_annotationMap );
      }
      if( associateableFeatureType instanceof FeatureTypeBuilder )
      {
        ftp = ( (FeatureTypeBuilder)associateableFeatureType ).toFeatureType();
        return new FeatureAssociationTypeProperty_Impl( m_name, m_namespace, m_typeName, true,
            m_schema, ftp, m_annotationMap );
      }
      if( associateableFeatureType instanceof Node )
      {
        return new FeatureAssociationTypeProperty_Impl( m_name, m_namespace, m_typeName, true,
            m_schema, (Node)associateableFeatureType, m_annotationMap );
        //Object
        // o=m_schema.getMappedType((Node)associateableFeatureType);
        //  x ftp=(FeatureType)o;
      }
      if( associateableFeatureType instanceof FeatureTypeProperty )
        return (FeatureTypeProperty)associateableFeatureType;
      throw new UnsupportedOperationException();
    }
    if( m_enumeration.size() > 0 )
      return new EnumerationFeatureTypeProperty( m_name, m_namespace, m_typeName, true,
          m_enumeration.toArray(), m_annotationMap );

    if( isXLink() )
    {
      return m_XLinkProp;
    }
//    if( m_typeName == null )
//      System.out.println( "debug" );

    if( m_isCutoumType )
      return new CustoumFeatureTypeProperty( m_name, m_namespace, m_typeName, true, m_annotationMap );
    return FeatureFactory.createFeatureTypeProperty( m_name, m_namespace, m_typeName, true,
        m_annotationMap );
  }

  public FeatureType toFeatureType()
  {
    //    if( m_isFeatureAssociation )
    //      System.out.println( "stop" );
    List featureTypeList = new ArrayList();
    List featureTypePropertyList = new ArrayList();

    for( int i = 0; i < m_featureProtoTypes.size(); i++ )
    {
      Object o = m_featureProtoTypes.get( i );
      if( o instanceof FeatureTypeBuilder )
      {
        FeatureTypeBuilder builder = (FeatureTypeBuilder)o;
        if( builder.isFeatureType() )
          featureTypeList.add( builder.toFeatureType() );

        if( builder.isFeaturePropertyType() )
          featureTypePropertyList.add( builder.toFeatureTypeProperty() );
      }
      else if( o instanceof FeatureType )
        featureTypeList.add( o );
      else if( o instanceof FeatureTypeProperty )
        featureTypePropertyList.add( o );

    }
    FeatureTypeProperty[] ftProperties = (FeatureTypeProperty[])featureTypePropertyList
        .toArray( new FeatureTypeProperty[featureTypePropertyList.size()] );
    FeatureType[] ftChilds = (FeatureType[])featureTypeList
        .toArray( new FeatureType[featureTypeList.size()] );
    int[] minOccurs = new int[ftProperties.length];
    int[] maxOccurs = new int[ftProperties.length];
    for( int i = 0; i < ftProperties.length; i++ )
    {
      String key = ftProperties[i].getNamespace() + ":" + ftProperties[i].getName();
      //      System.out.println("KEY:"+key);
      minOccurs[i] = Integer.parseInt( (String)m_minOccurs.get( key ) );
      String max = (String)m_maxOccurs.get( key );
      if( "unbounded".equals( max ) )
        maxOccurs[i] = FeatureType.UNBOUND_OCCURENCY;
      else
        maxOccurs[i] = Integer.parseInt( max );
    }
    return FeatureFactory.createFeatureType( m_name, m_namespace, ftProperties, minOccurs,
        maxOccurs, m_substitutionGroup, m_annotationMap );
  }

  public boolean isFeaturePropertyType()
  {
    return !isFeatureType();
  }

  public boolean isFeatureType()
  {
    if( "AbstractFeatureType".equals( m_typeName ) )
      return true;
    return false;
    //    return m_featureProtoTypes.size() > 0;
  }

  private boolean isXLink()
  {
    return m_XLinkProp != null;
  }

  public void setXLinkType( int xlink_type )
  {
    switch( xlink_type )
    {
    case GMLSchemaFactory.XLINK_TYPE_SIMPLE:
    {
      //        <attributeGroup name="simpleLink">
      //        <attribute name="type" type="string" fixed="simple"
      // form="qualified"/>
      m_XLinkProp = new XLinkFeatureTypeProperty( m_name, m_namespace,
          XLinkFeatureTypeProperty.XLINK_SIMPLE, true, m_annotationMap );

      //        <attribute ref="xlink:href" use="optional"/>
      //        <attribute ref="xlink:role" use="optional"/>
      //        <attribute ref="xlink:arcrole" use="optional"/>
      //        <attribute ref="xlink:title" use="optional"/>
      //        <attribute ref="xlink:show" use="optional"/>
      //        <attribute ref="xlink:actuate" use="optional"/>
      break;
    }
    default:
      break;
    }
  }

  public void setFeatureAssociation()
  {
  }

  /**
   * @return
   */
  public boolean isCustoumType()
  {
    return m_isCutoumType;
  }

  public void addAnnotation( Annotation annotation )
  {
    String lang = annotation.getLang();
    if( lang != null && lang.length() > 0 )
      m_annotationMap.put( lang, annotation );
  }

}