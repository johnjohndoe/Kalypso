package org.deegree_impl.gml.schema;

import java.net.URI;
import java.net.URL;
import java.util.Collection;
import java.util.HashMap;

import org.deegree.model.feature.FeatureType;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * This is a XML-schema convenient class for generating GML-FeatureType elements
 * from a XML schema
 * 
 * @author doemming
 */
public class GMLSchema
{
  private final Document m_schemaDoc;

  private final HashMap m_ns = new HashMap(); // (key,NS)

  private final HashMap m_importedSchemas = new HashMap(); // (NS,GMLSchema)

  private String m_targetNS = null;

  private HashMap m_featureTypes = null;

  private final HashMap m_nodeFeatureTypeMap;

  private final URL m_url;

  /**
   * @param schemaDoc
   *          DOM-root element of schema instance
   * @throws Exception
   *           Falls GML nicht geladen werden kann
   */
  public GMLSchema( final URL documentURL ) throws Exception
  {
    m_url = documentURL;
    m_nodeFeatureTypeMap = new HashMap();

    m_schemaDoc = XMLHelper.getAsDOM( documentURL,true );

    setNameSpaces();
    setImportedSchemas();
  }

  private void setImportedSchemas()
  {
    //    <import namespace="http://www.opengis.net/gml"
    //            schemaLocation="feature.xsd"/>
    final NodeList nl = m_schemaDoc.getElementsByTagNameNS( XMLHelper.XMLSCHEMA_NS, "import" );
    for( int i = 0; i < nl.getLength(); i++ )
    {
      try
      {
        final Node attributeNode = XMLHelper.getAttributeNode( nl.item( i ), "schemaLocation" );
        final String schemaLocation = attributeNode.getNodeValue();

        final URI uri = new URI( schemaLocation );
        final URL url = uri.isAbsolute() ? new URL( schemaLocation ) : new URL( m_url, schemaLocation );
        
//        System.out.println( "m_url: " + m_url.toString() );
//        System.out.println( "SchemaLocation: " + schemaLocation );
//        System.out.println( "url: " + url.toString() );

        final GMLSchema schema = new GMLSchema( url );
        m_importedSchemas.put( schema.getTargetNS(), schema );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }
  }

  public String getDefaultNS()
  {
    return getNameSpace( "xmlns" );
  }

  public FeatureType[] getFeatureTypes()
  {
    if( m_featureTypes == null )
      try
      {
        m_featureTypes = new HashMap();
        GMLSchemaFactory.createFeatureTypes( this, m_featureTypes );
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }

    Collection result = m_featureTypes.values();
    return (FeatureType[])result.toArray( new FeatureType[result.size()] );
  }

  public String getNameSpace( String xmlns )
  {
    return (String)m_ns.get( xmlns );
  }

  public Document getSchema()
  {
    return m_schemaDoc;
  }

  public String getTargetNS()
  {
    return m_targetNS;
  }

  private void setNameSpaces()
  {
    // set target namespace
    try
    {
      m_targetNS = XMLHelper
          .getAttributeValue( m_schemaDoc.getDocumentElement(), "targetNamespace" );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    // assuming that all namespaces are declared in root element
    // TODO handle declaration of namespaces anywhere in schema
    Node node = m_schemaDoc.getDocumentElement();
    NamedNodeMap nodeMap = node.getAttributes();

    for( int i = 0; i < nodeMap.getLength(); i++ )
    {
      try
      {
        Node attribute = nodeMap.item( i );
        if( "http://www.w3.org/2000/xmlns/".equals( attribute.getNamespaceURI() ) )
          m_ns.put( attribute.getLocalName(), attribute.getNodeValue() );
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }
  }

  public Node getContentNode( String namespace, String typeName )
  {
    GMLSchema schema = getGMLSchema( namespace );
    return schema.getContentNode( typeName );
  }

  public Node getContentNode( String type )
  {
    NodeList nl = getSchema().getElementsByTagNameNS( "http://www.w3.org/2001/XMLSchema",
        "complexType" );
    nl = XMLHelper.reduceByAttribute( nl, "name", type );
    if( nl.getLength() > 0 ) // complexType ?
      return nl.item( 0 );
    nl = getSchema().getElementsByTagNameNS( "http://www.w3.org/2001/XMLSchema", "simpleType" );
    nl = XMLHelper.reduceByAttribute( nl, "name", type );
    if( nl.getLength() > 0 ) // simpleType ?
      return nl.item( 0 );
    return null;
  }

  public boolean isImportedNS( String nameSpace )
  {
    if( m_targetNS.equals( nameSpace ) )
      return false;
    return m_importedSchemas.containsKey( nameSpace );
  }

  public GMLSchema getGMLSchema( String valueNS )
  {
    if( m_targetNS.equals( valueNS ) )
      return this;
    return (GMLSchema)m_importedSchemas.get( valueNS );
  }

  private void register( Node node )
  {
    if( m_nodeFeatureTypeMap.containsKey( node ) )
      return;
    try
    {
      // to avoid recursive endless featurebuilding,
      // we first register the node and overwrite this later with the
      // mapped type
      m_nodeFeatureTypeMap.put( node, node );
      FeatureTypeBuilder builder = new FeatureTypeBuilder( this, node );
      if( builder.isFeaturePropertyType() )
        m_nodeFeatureTypeMap.put( node, builder.toFeatureTypeProperty() );
      else if( builder.isFeatureType() )
        m_nodeFeatureTypeMap.put( node, builder.toFeatureType() );
      else
        throw new Exception( "it must be featuretype or featurepropertytype but it is not" );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  public Object getMappedType( Node node )
  {
    register( node );
    return m_nodeFeatureTypeMap.get( node );
  }

  public FeatureType getFeatureType( String typeName )
  {
    FeatureType[] featureTypes = getFeatureTypes();
    for( int i = 0; i < featureTypes.length; i++ )
    {
      FeatureType type = featureTypes[i];
      if( type.getName().equals( typeName ) )
        return type;
    }
    return null;
  }
}