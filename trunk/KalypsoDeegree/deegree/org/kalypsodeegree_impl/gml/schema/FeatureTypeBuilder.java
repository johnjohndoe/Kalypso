package org.deegree_impl.gml.schema;

import java.util.ArrayList;
import java.util.List;

import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree_impl.extension.ITypeHandler;
import org.deegree_impl.extension.TypeRegistrySingleton;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.feature.XLinkFeatureTypeProperty;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Helper class to build featuretype. It collects informations
 * from schema-definition and builds FeatureType objects.
 * 
 * @author doemming
 */
public class FeatureTypeBuilder
{
  private GMLSchema m_schema;

  private List m_enumeration = new ArrayList();

  private List m_featureProtoTypes = new ArrayList();

  private String m_name = null;

  private String m_typeName = null;

  private XLinkFeatureTypeProperty m_XLinkProp = null;

  
  public FeatureTypeBuilder( GMLSchema schema, Node node ) throws Exception
  {

    m_schema = schema;
    m_typeName = null;

    Element element = (Element)node;

    if( !element.hasAttribute( "name" ) || !element.hasAttribute( "type" ) )
      throw new Exception( "no valid elementnode: " + XMLHelper.toString( node ) );

    SchemaAttribute nameAttribute = new SchemaAttribute( m_schema, XMLHelper.getAttributeNode( node, "name" ) );
    m_name = nameAttribute.getValue();

    SchemaAttribute typeAttribute = new SchemaAttribute( schema, XMLHelper.getAttributeNode( node, "type" ) );

    setTypeName( typeAttribute );
  }

  public void setTypeName( SchemaAttribute typeAttribute ) throws Exception
  {
    final String typeName = typeAttribute.getValue();
    final String valueNS = typeAttribute.getValueNS();

    if( "http://www.w3.org/2001/XMLSchema".equals( valueNS ) )
      m_typeName = Mapper.mapXMLSchemaType2JavaType( typeName );
    else if( "http://www.opengis.net/gml".equals( valueNS ) )
      m_typeName = Mapper.mapGMLSchemaType2JavaType( typeName );
    else if( m_schema.getTargetNS().equals( valueNS ) )
    {
      final Node cNode = getContentNode( typeName );
      if( ( (Element)cNode ).getLocalName().equals( "complexType" ) )
      {
        m_typeName = "org.deegree.model.Feature";
        GMLSchemaFactory.map( m_schema, cNode, this );
 
      }
      else if( ( (Element)cNode ).getLocalName().equals( "simpleType" ) )
      {
        m_typeName = "org.deegree.model.FeatureProperty";
        GMLSchemaFactory.map( m_schema, cNode, this );
      }
      else
        throw new Exception( "content of element not found in Schema " + typeAttribute.toString() );
    }
    else
    {
      final String typename = valueNS + ":" + typeName;
      final ITypeHandler typeHandler = TypeRegistrySingleton.getTypeRegistry().getTypeHandlerForTypeName( typename );
      if( typeHandler == null )
        throw new Exception( "undefined type in schema: " + typename );
      m_typeName = typeHandler.getClassName();
    }
  }

  private Node getContentNode( String type )
  {
    NodeList nl = m_schema.getSchema().getElementsByTagNameNS( "http://www.w3.org/2001/XMLSchema",
        "complexType" );
    nl = XMLHelper.reduceByAttribute( nl, "name", type );
    if( nl.getLength() > 0 ) // complexType ?
      return nl.item( 0 );
    nl = m_schema.getSchema().getElementsByTagNameNS( "http://www.w3.org/2001/XMLSchema",
        "simpleType" );
    nl = XMLHelper.reduceByAttribute( nl, "name", type );
    if( nl.getLength() > 0 ) // simpleType ?
      return nl.item( 0 );
    return null;
  }

  public String getName()
  {
    return m_name;
  }

  public void add( FeatureTypeProperty featureTypeProperty )
  {
    m_featureProtoTypes.add( featureTypeProperty );
  }

  public void add( FeatureTypeBuilder featureProtoType )
  {
    m_featureProtoTypes.add( featureProtoType );

  }

  public void addEnumerationObject( Object object )
  {
    m_enumeration.add( object );
  }

  public FeatureTypeProperty toFeatureTypeProperty()
  {
    if( m_enumeration.size() > 0 )
      return new EnumerationFeatureTypeProperty( m_name, m_typeName, true, m_enumeration.toArray() );

    if( isXLink() )
    {
      return m_XLinkProp;
    }

    return FeatureFactory.createFeatureTypeProperty( m_name, m_typeName, true );
  }

  public FeatureType toFeatureType()
  {
    List featureTypeList = new ArrayList();
    List featureTypePropertyList = new ArrayList();

    for( int i = 0; i < m_featureProtoTypes.size(); i++ )
    {
      FeatureTypeBuilder builder = (FeatureTypeBuilder)m_featureProtoTypes.get( i );

      if( builder.isFeatureType() )
        featureTypeList.add( builder.toFeatureType() );

      if( builder.isFeaturePropertyType() )
        featureTypePropertyList.add( builder.toFeatureTypeProperty() );

    }
    FeatureTypeProperty[] ftProperties = (FeatureTypeProperty[])featureTypePropertyList
        .toArray( new FeatureTypeProperty[featureTypePropertyList.size()] );
    FeatureType[] ftChilds = (FeatureType[])featureTypeList
        .toArray( new FeatureType[featureTypeList.size()] );

    return FeatureFactory.createFeatureType( new FeatureType[]
    { null },
    // parents
        ftChilds, // children
        m_name, ftProperties );
  }

  private boolean isFeaturePropertyType()
  {
    return m_featureProtoTypes.size() == 0;
    // TODO
  }

  private boolean isFeatureType()
  {
    return m_featureProtoTypes.size() > 0;
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
      //        <attribute name="type" type="string" fixed="simple" form="qualified"/>
      m_XLinkProp = new XLinkFeatureTypeProperty( m_name, XLinkFeatureTypeProperty.XLINK_SIMPLE,
          true );

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
}