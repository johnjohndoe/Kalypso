package org.kalypsodeegree_impl.gml.schema;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.kalypsodeegree.model.feature.Annotation;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree_impl.extension.IMarshallingTypeHandler;
import org.kalypsodeegree_impl.extension.MarshallingTypeRegistrySingleton;
import org.kalypsodeegree_impl.model.feature.FeatureAssociationTypeProperty_Impl;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Helper class to build featuretype. It collects informations from schema-definition and builds FeatureType objects.
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

    if( !element.hasAttribute( "name" ) )
      // || !element.hasAttribute( "type" ) )
      throw new Exception( "no valid elementnode (name is missing): " + XMLHelper.toString( node ) );

    // set Name
    SchemaAttribute nameAttribute = new SchemaAttribute( m_schema, XMLHelper.getAttributeNode( node, "name" ) );
    m_name = nameAttribute.getValue();

    // set substitutiongroup
    final Node substitutionGroupNode = XMLHelper.getAttributeNode( node, "substitutionGroup" );

    if( substitutionGroupNode != null )
    {
      final SchemaAttribute substitueAttribute = new SchemaAttribute( m_schema, substitutionGroupNode );
      String substitutionNS = substitueAttribute.getValueNS();
      String substitutionName = substitueAttribute.getValue();
      m_substitutionGroup = substitutionNS + ":" + substitutionName;
    }

    // set Type
    final Node attributeNode = XMLHelper.getAttributeNode( node, "type" );
    if( attributeNode == null )
    { // complexType is inner definition
      Node cNode = XMLHelper.getFirstChildElement( node, XMLHelper.XMLSCHEMA_NS, "complexType", 1 );
      String baseType = XMLHelper.getGMLBaseType( schema, cNode );
      if( baseType != null )
      { // a GMLBaseType
        m_typeName = baseType;
        // m_typeName = "org.kalypsodeegree.model.Feature";
        GMLSchemaFactory.map( schema, cNode, this );
      }
    }
    else
    // complexType
    {
      SchemaAttribute typeAttribute = new SchemaAttribute( schema, attributeNode );
      setTypeName( typeAttribute );
    }
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

  public void setTypeName( final SchemaAttribute typeAttribute ) throws Exception
  {
    try
    {
    final String typeName = typeAttribute.getValue();
    final String valueNS = typeAttribute.getValueNS();

    // is registred type ?
    final String typename = valueNS + ":" + typeName;
    final IMarshallingTypeHandler typeHandler = (IMarshallingTypeHandler)MarshallingTypeRegistrySingleton.getTypeRegistry().getTypeHandlerForTypeName( typename );
    if( typeHandler != null )
    {
      m_typeName = typeHandler.getClassName();
      // if(m_typeName==null)
      // System.out.println("debug");
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
      // map gml:FeatureAssociationType
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
        // m_typeName = "org.kalypsodeegree.model.Feature";
        GMLSchemaFactory.map( schema, cNode, this );
        return;
      }

      NodeList custoumElement = ( (Element)cNode ).getElementsByTagNameNS( XMLHelper.XMLSCHEMA_NS, "element" );
      // /////////
      Node custoumNode = custoumElement.item( 0 );

      SchemaAttribute innerTypeAttribute = new SchemaAttribute( schema, XMLHelper
          .getAttributeNode( custoumNode, "type" ) );
      setTypeName( innerTypeAttribute );
      return;
    }
    else if( "simpleType".equals( ( (Element)cNode ).getLocalName() ) )
    {
      m_typeName = "org.kalypsodeegree.model.FeatureProperty";
      GMLSchemaFactory.map( schema, cNode, this );
      return;
    }
    else
      throw new Exception( "content of element not found in Schema " + typeAttribute.toString() );
    }
    finally
    {
      if( m_typeName == null )
        throw new Exception( "Could not determine typeName of: " + typeAttribute.toString() );
    }
  }

  public String getName()
  {
    return m_name;
  }

  public String getNamespace()
  {
    return m_namespace;
  }

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
      if( m_featureProtoTypes.size() > 0 )
      {
        Object associateableFeatureType = m_featureProtoTypes.get( 0 );
        FeatureType ftp = null;
        if( associateableFeatureType instanceof FeatureType )
        {
          ftp = (FeatureType)associateableFeatureType;
          return new FeatureAssociationTypeProperty_Impl( m_name, m_namespace, m_typeName, true, m_schema, ftp,
              m_annotationMap );
        }
        if( associateableFeatureType instanceof FeatureTypeBuilder )
        {
          ftp = ( (FeatureTypeBuilder)associateableFeatureType ).toFeatureType();
          return new FeatureAssociationTypeProperty_Impl( m_name, m_namespace, m_typeName, true, m_schema, ftp,
              m_annotationMap );
        }
        if( associateableFeatureType instanceof Node )
        {
          return new FeatureAssociationTypeProperty_Impl( m_name, m_namespace, m_typeName, true, m_schema,
              (Node)associateableFeatureType, m_annotationMap );
          // Object
          // o=m_schema.getMappedType((Node)associateableFeatureType);
          // x ftp=(FeatureType)o;
        }
        if( associateableFeatureType instanceof FeatureTypeProperty )
          return (FeatureTypeProperty)associateableFeatureType;
      }
      else
      {
        if( XMLHelper.GMLSCHEMA_NS.equals( m_namespace ) && "featureMember".equals( m_name ) )
          return new FeatureAssociationTypeProperty_Impl( m_name, m_namespace, m_typeName, true, m_schema, m_schema
              .getFeatureType( "_Feature" ), m_annotationMap );
      }
      throw new UnsupportedOperationException();
    }
    if( m_enumeration.size() > 0 )
      return new EnumerationFeatureTypeProperty( m_name, m_namespace, m_typeName, true, m_enumeration.toArray(),
          m_annotationMap );

    if( m_isCutoumType )
      return new CustoumFeatureTypeProperty( m_name, m_namespace, m_typeName, true, m_annotationMap );
    return FeatureFactory.createFeatureTypeProperty( m_name, m_namespace, m_typeName, true, m_annotationMap );
  }

  public FeatureType toFeatureType()
  {
    final List featureTypeList = new ArrayList();
    final List featureTypePropertyList = new ArrayList();

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

    int[] minOccurs = new int[ftProperties.length];
    int[] maxOccurs = new int[ftProperties.length];
    for( int i = 0; i < ftProperties.length; i++ )
    {
      String key = ftProperties[i].getNamespace() + ":" + ftProperties[i].getName();
      final String min = (String)m_minOccurs.get( key );
      // TODO: Andreas? warum war das bisher kein Problem??
      if( min == null )
        minOccurs[i] = 1;
      else
        minOccurs[i] = Integer.parseInt( min );

      final String max = (String)m_maxOccurs.get( key );
      if( "unbounded".equals( max ) )
        maxOccurs[i] = FeatureType.UNBOUND_OCCURENCY;
      // TODO: Andreas? warum war das bisher kein Problem??
      else if( max == null )
        maxOccurs[i] = 1;
      // TODO: Andreas? warum war das bisher kein Problem??
      else
        maxOccurs[i] = Integer.parseInt( max );
    }
    final FeatureType realFeatureType = FeatureFactory.createFeatureType( m_name, m_namespace, ftProperties, minOccurs,
        maxOccurs, m_substitutionGroup, m_annotationMap );

    realFeatureType
        .setVirtuelFeatureTypeProperty( FeatureFactory.createVirtualFeatureTypeProperties( realFeatureType ) );
    return realFeatureType;
  }

  public boolean isFeaturePropertyType()
  {
    return !isFeatureType();
  }

  public boolean isFeatureType()
  {
    if( "AbstractFeatureType".equals( m_typeName ) || "org.kalypsodeegree.model.feature.Feature".equals( m_typeName ) )
      return true;
    return false;
    // return m_featureProtoTypes.size() > 0;
  }

  /**
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