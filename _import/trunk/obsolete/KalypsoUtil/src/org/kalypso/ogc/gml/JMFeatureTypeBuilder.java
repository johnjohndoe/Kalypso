package org.kalypso.ogc.gml;

import java.util.ArrayList;
import java.util.List;

import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree_impl.extension.ITypeHandler;
import org.deegree_impl.extension.TypeRegistrySingleton;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.feature.XLinkFeatureTypeProperty;
import org.kalypso.util.xml.XMLTools;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * 
 * @author vDoemming
 */
public class JMFeatureTypeBuilder
{
  private JMSchema mySchema;

  private List myEnumeration = new ArrayList();

  private List myFeatureProtoTypes = new ArrayList();

  private String myName = null;

  private String myTypeName = null;

  private XLinkFeatureTypeProperty myXLinkProp = null;

  /**
   * @param schema
   * @param node
   *          elementNode <element name="NAModell" type="na:_NAModellType"
   *          substitutionGroup="gml:_FeatureCollection">
   * 
   * <element name="Knoten" type="na:_KnotenType"
   * substitutionGroup="gml:_Feature"/> <element name="Ausdehnung"
   * type="gml:PolygonPropertyType" substitutionGroup="gml:polygonProperty"/>
   * <element name="minQ" type="double"/> <element name="Landnutzung"
   * type="na:_LandnutzungType"/>
   * 
   * but no references like <element ref="na:Position"/>
   * 
   * @throws Exception
   */
  public JMFeatureTypeBuilder( JMSchema schema, Node node ) throws Exception
  {

    mySchema = schema;
    myTypeName = null;

    Element element = (Element)node;

    if( !element.hasAttribute( "name" ) || !element.hasAttribute( "type" ) )
      throw new Exception( "no valid elementnode: " + XMLTools.toString( node ) );

    JMAttribute nameAttribute = new JMAttribute( mySchema, XMLTools.getAttributeNode( node, "name" ) );
    myName = nameAttribute.getValue();

    JMAttribute typeAttribute = new JMAttribute( schema, XMLTools.getAttributeNode( node, "type" ) );

    setTypeName( typeAttribute );
  }

  public void setTypeName( JMAttribute typeAttribute ) throws Exception
  {
    final String typeName = typeAttribute.getValue();
    final String valueNS = typeAttribute.getValueNS();

    if( "http://www.w3.org/2001/XMLSchema".equals( valueNS ) )
      myTypeName = Mapper.mapXMLSchemaType2JavaType( typeName );
    else if( "http://www.opengis.net/gml".equals( valueNS ) )
      myTypeName = Mapper.mapGMLSchemaType2JavaType( typeName );
    else if( mySchema.getTargetNS().equals( valueNS ) )
    {
      final Node cNode = getContentNode( typeName );
      if( ( (Element)cNode ).getLocalName().equals( "complexType" ) )
      {
        myTypeName = "org.deegree.model.Feature";
        JMSchemaFactory.map( mySchema, cNode, this );
      }
      else if( ( (Element)cNode ).getLocalName().equals( "simpleType" ) )
      {
        myTypeName = "org.deegree.model.FeatureProperty";
        JMSchemaFactory.map( mySchema, cNode, this );
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
      myTypeName = typeHandler.getClassName();
    }
  }

  private Node getContentNode( String type )
  {
    NodeList nl = mySchema.getSchema().getElementsByTagNameNS( "http://www.w3.org/2001/XMLSchema",
        "complexType" );
    nl = XMLTools.reduceByAttribute( nl, "name", type );
    if( nl.getLength() > 0 ) // complexType ?
      return nl.item( 0 );
    nl = mySchema.getSchema().getElementsByTagNameNS( "http://www.w3.org/2001/XMLSchema",
        "simpleType" );
    nl = XMLTools.reduceByAttribute( nl, "name", type );
    if( nl.getLength() > 0 ) // simpleType ?
      return nl.item( 0 );
    return null;
  }

  public String getName()
  {
    return myName;
  }

  public void add( FeatureTypeProperty featureTypeProperty )
  {
    myFeatureProtoTypes.add( featureTypeProperty );
  }

  public void add( JMFeatureTypeBuilder featureProtoType )
  {
    myFeatureProtoTypes.add( featureProtoType );

  }

  public void addEnumerationObject( Object object )
  {
    myEnumeration.add( object );
  }

  public FeatureTypeProperty toFeatureTypeProperty()
  {
    if( myEnumeration.size() > 0 )
      return new EnumerationFeatureTypeProperty( myName, myTypeName, true, myEnumeration.toArray() );

    if( isXLink() )
    {
      return myXLinkProp;
    }

    return FeatureFactory.createFeatureTypeProperty( myName, myTypeName, true );
  }

  public FeatureType toFeatureType()
  {
    List featureTypeList = new ArrayList();
    List featureTypePropertyList = new ArrayList();

    for( int i = 0; i < myFeatureProtoTypes.size(); i++ )
    {
      JMFeatureTypeBuilder builder = (JMFeatureTypeBuilder)myFeatureProtoTypes.get( i );

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
        myName, ftProperties );
  }

  //private FeaturePropertyType();
  private boolean isFeaturePropertyType()
  {
    return myFeatureProtoTypes.size() == 0;

    // TODO
  }

  private boolean isFeatureType()
  {
    return myFeatureProtoTypes.size() > 0;
  }

  private boolean isXLink()
  {
    return myXLinkProp != null;
  }

  public void setXLinkType( int xlink_type )
  {
    switch( xlink_type )
    {
    case JMSchemaFactory.XLINK_TYPE_SIMPLE:
    {
      //        <attributeGroup name="simpleLink">
      //        <attribute name="type" type="string" fixed="simple" form="qualified"/>
      myXLinkProp = new XLinkFeatureTypeProperty( myName, XLinkFeatureTypeProperty.XLINK_SIMPLE,
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

  //  private void addAttribute(XLinkAttribute att)
  //  {
  //      myAttributes.put(att.getLocalName(),att);
  //  }
}