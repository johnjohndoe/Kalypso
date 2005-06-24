package org.kalypsodeegree_impl.gml.schema;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.deegree.gml.GMLException;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree_impl.gml.schema.vistors.GMLSchemaVisitor;
import org.kalypsodeegree_impl.gml.schema.vistors.RecursiveSchemaVisitor;
import org.kalypsodeegree_impl.gml.schema.vistors.SubstitutionGroupRegistrator;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * This is a XML-schema convenient class for generating GML-FeatureType elements from a XML schema
 * 
 * @author doemming
 */
public class GMLSchema
{
  private Document m_schemaDoc;

  private final HashMap m_ns = new HashMap(); // (key,NS)

  private final HashMap m_importedSchemas = new HashMap(); // (NS,GMLSchema)

  private String m_targetNS = null;

  private HashMap m_featureTypes = null;

  private final HashMap m_nodeFeatureTypeMap;

  private final URL m_url;

  public GMLSchema( final URL documentURL ) throws Exception
  {
    this( loadDocFromURL( documentURL ), documentURL );
  }

  private static Document loadDocFromURL( final URL documentURL ) throws Exception
  {
    return XMLHelper.getAsDOM( documentURL, true );
  }

  public GMLSchema( final Document document, final URL context ) throws Exception
  {
    m_url = context;
    m_nodeFeatureTypeMap = new HashMap();

    m_schemaDoc = document;

    setNameSpaces();
    setImportedSchemas();
    // to force building of featuretypes
    FeatureType[] featureTypes = getFeatureTypes();

    for( int i = 0; i < featureTypes.length; i++ )
      accept( new SubstitutionGroupRegistrator( featureTypes[i] ) );
  }

  private void setImportedSchemas() throws GMLException, MalformedURLException
  {
    // <import namespace="http://www.opengis.net/gml"
    // schemaLocation="feature.xsd"/>
    final NodeList nl = m_schemaDoc.getElementsByTagNameNS( XMLHelper.XMLSCHEMA_NS, "import" );
    for( int i = 0; i < nl.getLength(); i++ )
    {
      final Node namespaceNode = XMLHelper.getAttributeNode( nl.item( i ), "namespace" );
      final String namespace = namespaceNode.getNodeValue();
      final Node locationNode = XMLHelper.getAttributeNode( nl.item( i ), "schemaLocation" );
      final String location = locationNode.getNodeValue();

      GMLSchema schema = GMLSchemaCatalog.getSchema( namespace );
      if( schema == null )
      {
        final URL url = new URL( getUrl(), location );
        schema = GMLSchemaCatalog.getSchema( url );
      }

      if( schema == null )
        throw new GMLException( "Could not load schema: namespace='" + namespace + "', location='" + location + "'" );

      m_importedSchemas.put( namespace, schema );
    }
  }

  public Document getXMLDocument()
  {
    return m_schemaDoc;
  }

  public String getDefaultNS()
  {
    return getNameSpace( "xmlns" );
  }

  /**
   * 
   * @return the featuretypes that are defined in this schema, <br>
   *         featuretypes of imported schemas are not included
   */
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
    final Collection result = m_featureTypes.values();
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
      m_targetNS = XMLHelper.getAttributeValue( m_schemaDoc.getDocumentElement(), "targetNamespace" );
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
    NodeList nl = getSchema().getElementsByTagNameNS( "http://www.w3.org/2001/XMLSchema", "complexType" );
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

  /** Returns a map xmlns -> namespaceUri */
  public Map getNamespaceMap()
  {
    return m_ns;
  }

  public void accept( final GMLSchemaVisitor visitor )
  {
    final RecursiveSchemaVisitor outer = new RecursiveSchemaVisitor( visitor );
    outer.visit( this );
  }

  public GMLSchema[] getImportedSchemas()
  {
    final Collection collection = m_importedSchemas.values();
    return (GMLSchema[])collection.toArray( new GMLSchema[collection.size()] );
  }

  public URL getUrl()
  {
    return m_url;
  }

}