package org.kalypso.ogc.wfs;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpMethod;
import org.apache.commons.httpclient.URI;
import org.apache.commons.httpclient.URIException;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.httpclient.params.HttpClientParams;
import org.apache.commons.io.IOUtils;
import org.deegree.datatypes.QualifiedName;
import org.deegree.model.filterencoding.capabilities.Operator;
import org.deegree.model.filterencoding.capabilities.SpatialOperator;
import org.deegree.ogcwebservices.getcapabilities.DCPType;
import org.deegree.ogcwebservices.getcapabilities.HTTP;
import org.deegree.ogcwebservices.getcapabilities.InvalidCapabilitiesException;
import org.deegree.ogcwebservices.wfs.capabilities.FeatureTypeList;
import org.deegree.ogcwebservices.wfs.capabilities.WFSCapabilities;
import org.deegree.ogcwebservices.wfs.capabilities.WFSCapabilitiesDocument;
import org.deegree.ogcwebservices.wfs.capabilities.WFSFeatureType;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.opengis.cs.CS_CoordinateSystem;
import org.xml.sax.SAXException;

/**
 * Implements the WFSService.<br>
 * This file is mainly a copy from the WFService.java in deeJump from Deegree.<br>
 * So the most credits go to:<br>
 * Ugo Taddei<br>
 * Changes:<br>
 * Holger Albert
 * 
 * @author <a href="mailto:taddei@lat-lon.de">Ugo Taddei</a>
 * @author Holger Albert
 */
public class WFService
{
  public static String WFS_PREFIX = "wfs";

  private String m_wfsURL;

  private WFSCapabilities m_wfsCapabilities;

  private Map<QName, QualifiedName> m_featureTypeToQName;

  /**
   * Contains a list of feature types (as Strings) offered by the WFService
   */
  private String[] m_featureTypes;

  /**
   * Maps a feature type to its schem. Geometry property is not held here!
   */
  private Map<QName, IGMLSchema> m_ftToSchema;

  /**
   * Maps a feature type to its geometry!
   */
  private Map<QName, QName[]> m_geoProperties;

  private HttpClient m_httpClient;

  private String m_getFeatureUrl;

  private String m_descrFtUrl;

  public WFService( String wfsURL ) throws Exception
  {
    m_ftToSchema = new HashMap<QName, IGMLSchema>( 10 );
    m_geoProperties = new HashMap<QName, QName[]>( 10 );

    createHttpClient();

    // TODO validate string
    setWfsURL( wfsURL + "?REQUEST=GetCapabilities&VERSION=1.1.0&SERVICE=WFS" );
    WFSCapabilitiesDocument wfsCapsDoc = new WFSCapabilitiesDocument();

    try
    {
      wfsCapsDoc.load( new URL( this.m_wfsURL ) );
    }
    catch( MalformedURLException e )
    {
      System.out.println( "Invalid URL.\nException: " + e.getLocalizedMessage() );
      throw e;
    }
    catch( IOException e )
    {
      System.out.println( "IOException when opening: " + this.m_wfsURL + " \nException: " + e.getLocalizedMessage() );
      throw e;
    }
    catch( SAXException e )
    {
      throw e;
    }

    try
    {
      m_wfsCapabilities = (WFSCapabilities) wfsCapsDoc.parseCapabilities();
    }
    catch( InvalidCapabilitiesException e )
    {
      System.out.println( "Could not initalize WFS capabilities.\nException: " + e.getLocalizedMessage() );
      throw e;
    }
  }

  private String[] extractFeatureTypes( ) throws Exception
  {
    WFSFeatureType[] featTypes = m_wfsCapabilities.getFeatureTypeList().getFeatureTypes();
    m_featureTypeToQName = new HashMap<QName, QualifiedName>();
    String[] fts = new String[featTypes.length];
    for( int i = 0; i < fts.length; i++ )
    {

      QualifiedName qn = featTypes[i].getName();
      String prefixedName = qn.getPrefixedName();
      fts[i] = prefixedName;

      m_featureTypeToQName.put( new QName( qn.getNamespace().toASCIIString(), qn.getLocalName(), qn.getPrefix() ), qn );
    }

    return fts;
  }

  /**
   * Gets a document containing a valid description feature type (schema; XSD). Code adapted from deegree viewer
   * 
   * @param featureType
   *            the feature type whose description should be returned
   * @return an XSD containing the type description
   * @throws Exception
   */
  private IGMLSchema loadForFeatureType( QName featureType ) throws Exception
  {
    org.deegree.ogcwebservices.getcapabilities.Operation[] ops = this.m_wfsCapabilities.getOperationsMetadata().getOperations();

    boolean isGet = true;

    if( m_descrFtUrl == null )
    {
      for( int i = 0; i < ops.length && m_descrFtUrl == null; i++ )
      {
        if( ops[i].getName().equals( "DescribeFeatureType" ) )
        {
          DCPType[] dcps = ops[i].getDCPs();
          if( dcps.length > 0 )
          {
            m_descrFtUrl = ((HTTP) dcps[0].getProtocol()).getGetOnlineResources()[0].toString();
          }

          if( m_descrFtUrl == null )
          {
            m_descrFtUrl = ((HTTP) dcps[0].getProtocol()).getPostOnlineResources()[0].toString();
            isGet = false;
          }
        }
      }
    }

    if( m_descrFtUrl == null )
      throw new RuntimeException( "Service does not have a DescribeFeatureType operation accessible by HTTP GET or POST." );

    QualifiedName ft = getQualiNameByFeatureTypeName( featureType );
    String format = null;
    try
    {
      format = URLEncoder.encode( "text/xml; subtype=gml/3.1.1", "UTF-8" );
    }
    catch( UnsupportedEncodingException e )
    {
      throw e;
    }

    String serverReq = "SERVICE=WFS&REQUEST=DescribeFeatureType&version=1.1.0&OUTPUTFORMAT=" + format + "&TYPENAME=" + featureType + "&NAMESPACE=xmlns(" + ft.getPrefix() + "=" + ft.getNamespace()
        + ")";

    String httpProtocolMethod = isGet ? "HTTP_GET" : "HTTP_POST";

    System.out.println( "Using " + httpProtocolMethod + " to get feature type description from " + m_descrFtUrl + serverReq );

    HttpMethod httpMethod = createHttpMethod( httpProtocolMethod );// new GetMethod( serverUrl );

    URI uri;
    try
    {
      uri = new URI( m_descrFtUrl, true );
      httpMethod.setURI( uri );

    }
    catch( URIException e )
    {
      throw e;
    }

    httpMethod.setQueryString( serverReq );

    try
    {
      m_httpClient.executeMethod( httpMethod );

      GMLSchema gmlSchema = GMLSchemaFactory.createGMLSchema( httpMethod.getResponseBodyAsStream(), null, new URL( m_descrFtUrl ) );
      return gmlSchema;
    }
    catch( Exception e )
    {
      String mesg = "Error fetching FeatureType description";
      System.out.println( mesg + " for " + featureType + " from " + uri + " using " + m_descrFtUrl + "?" + serverReq );
      throw new Exception( mesg, e );
    }
  }

  /**
   * Creates an String[] containing the attributes of a given feature type
   * 
   * @param featureTypeName
   *            the name of the feature type
   * @throws Exception
   */
  private void createSchemaForFeatureType( QName featureTypeName ) throws Exception
  {
    IGMLSchema xsd = loadForFeatureType( featureTypeName );
    if( xsd == null )
      return;

    this.m_ftToSchema.put( featureTypeName, xsd );

    QName[] geoProp = guessGeomProperty( xsd );

    this.m_geoProperties.put( featureTypeName, geoProp );
  }

  /**
   * guess which property might be "the" geometry property
   * 
   * @param propNames
   * @return
   */
  protected QName[] guessGeomProperty( IGMLSchema schema )
  {
    QName[] geoPropNames = null;
    List<QName> tmpList = new ArrayList<QName>( 20 );

    IFeatureType[] fts = schema.getAllFeatureTypes();
    for( int i = 0; i < fts.length; i++ )
    {
      IPropertyType[] props = fts[i].getAllGeomteryProperties();
      for( int j = 0; j < props.length; j++ )
        tmpList.add( props[j].getQName() );
    }

    geoPropNames = tmpList.toArray( new QName[tmpList.size()] );
    return geoPropNames;
  }

  public void setWfsURL( String wfsURL )
  {
    if( wfsURL == null || "".equals( wfsURL ) )
      throw new IllegalArgumentException( "The URL for the WFServer cannot be null or empty." );

    this.m_wfsURL = wfsURL;
  }

  public String getWfsURL( )
  {
    return this.m_wfsURL;
  }

  public String[] getFeatureTypes( ) throws Exception
  {
    if( m_featureTypes == null )
      m_featureTypes = extractFeatureTypes();

    return m_featureTypes;
  }

  public QName[] getFeatureProperties( QName featureType ) throws Exception
  {
    createSchemaForFeatureType( featureType );

    List<QName> propsList = new ArrayList<QName>();

    IGMLSchema schema = this.m_ftToSchema.get( featureType );

    IFeatureType[] fts = schema.getAllFeatureTypes();
    for( int i = 0; i < fts.length; i++ )
    {
      IPropertyType[] props = fts[i].getProperties();
      for( int j = 0; j < props.length; j++ )
      {
        // if( !(props[j].getType() == Types.GEOMETRY) )
        propsList.add( props[j].getQName() );
      }
    }

    return propsList.toArray( new QName[propsList.size()] );
  }

  public IGMLSchema getSchemaForFeatureType( QName featureType )
  {
    return this.m_ftToSchema.get( featureType );
  }

  public QName[] getGeometryProperties( QName featureType )
  {
    return this.m_geoProperties.get( featureType );
  }

  public QualifiedName getQualiNameByFeatureTypeName( QName ftName )
  {
    return m_featureTypeToQName.get( ftName );
  }

  public String getGetFeatureURL( )
  {
    org.deegree.ogcwebservices.getcapabilities.Operation[] ops = this.m_wfsCapabilities.getOperationsMetadata().getOperations();
    m_getFeatureUrl = null;

    for( int i = 0; i < ops.length && m_getFeatureUrl == null; i++ )
    {
      if( ops[i].getName().equals( "GetFeature" ) )
      {
        DCPType[] dcps = ops[i].getDCPs();
        if( dcps.length > 0 )
          m_getFeatureUrl = ((HTTP) dcps[0].getProtocol()).getPostOnlineResources()[0].toString();
      }
    }

    if( m_getFeatureUrl == null )
      throw new RuntimeException( "Service does not have a GetFeature operation accessible by HTTP POST." );

    return m_getFeatureUrl;
  }

  String getCrsForFeatureType( QualifiedName featureTypeName )
  {
    String crs = null;

    FeatureTypeList ftl = this.m_wfsCapabilities.getFeatureTypeList();

    QualifiedName qn = featureTypeName;// getQualiNameByFeatureTypeName( featureType );
    WFSFeatureType ft = ftl.getFeatureType( qn );
    crs = ft.getDefaultSRS().toASCIIString();

    return crs;
  }

  private void createHttpClient( )
  {
    m_httpClient = new HttpClient();

    HttpClientParams clientPars = new HttpClientParams();
    clientPars.setConnectionManagerTimeout( 60000 );

    m_httpClient.setParams( clientPars );

  }

  private HttpMethod createHttpMethod( String methodName )
  {
    HttpMethod httpMethod = null;

    if( "HTTP_GET".equals( methodName ) )
      httpMethod = new GetMethod();
    else if( "HTTP_POST".equals( methodName ) )
      httpMethod = new PostMethod();
    else
      throw new IllegalArgumentException( "method mame must be either 'HTTP_GET' or 'HTTP_POST'" );

    return httpMethod;
  }

  /**
   * This function returns the capabilities of the service.
   * 
   * @return The wfs capabilities.
   */
  public WFSCapabilities getCapabilities( )
  {
    return m_wfsCapabilities;
  }

  /**
   * This function returns all filter capabilities operations for the wfs.
   * 
   * @raturn All filter capabilities operations.
   */
  public String[] getAllFilterCapabilitesOperations( )
  {
    ArrayList<String> operators = new ArrayList<String>();

    SpatialOperator[] spatialOperators = m_wfsCapabilities.getFilterCapabilities().getSpatialCapabilities().getSpatialOperators();
    for( SpatialOperator spatialOperator : spatialOperators )
      operators.add( spatialOperator.getName() );

    QualifiedName[] geometryOperands = m_wfsCapabilities.getFilterCapabilities().getSpatialCapabilities().getGeometryOperands();
    for( QualifiedName qualifiedName : geometryOperands )
      operators.add( qualifiedName.getLocalName() );

    Operator[] arithmeticOperators = m_wfsCapabilities.getFilterCapabilities().getScalarCapabilities().getArithmeticOperators();
    for( Operator operator : arithmeticOperators )
      operators.add( operator.getName() );

    Operator[] comparisonOperators = m_wfsCapabilities.getFilterCapabilities().getScalarCapabilities().getComparisonOperators();
    for( Operator operator : comparisonOperators )
      operators.add( operator.getName() );

    return operators.toArray( new String[] {} );
  }

  /**
   * This function creates a gml workspace from a get feature request.
   * 
   * @param featureTypeToLoad
   * @param targetCRS
   * @param filter
   * @param maxFeatureAsString
   */
  public GMLWorkspace createGMLWorkspaceFromGetFeature( QName featureTypeToLoad, CS_CoordinateSystem targetCRS, String filter, String maxFeatureAsString ) throws Exception
  {
    BufferedInputStream inputStream = null;
    PrintStream postWriter = null;

    /* Create the GetFeature URL. */
    String getFeatureURL = getGetFeatureURL();
    URL getFeature = new URL( getFeatureURL );

    try
    {
      URLConnection con = getFeature.openConnection();
      con.setDoOutput( true );
      con.setDoInput( true );

      /* Write request to the WFS server. */
      OutputStream connectionOutputStream = con.getOutputStream();
      postWriter = new PrintStream( connectionOutputStream );

      String getFeaturePost = buildGetFeatureRequestPOST( m_wfsCapabilities, featureTypeToLoad, filter, maxFeatureAsString );
      postWriter.print( getFeaturePost );

      /* Read response from the WFS server and create a GMLWorkspace. */
      inputStream = new BufferedInputStream( con.getInputStream() );

      GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( inputStream, buildDescribeURLForFeatureType( featureTypeToLoad ), false, null );
      inputStream.close();

      if( targetCRS != null )
        workspace.accept( new TransformVisitor( targetCRS ), workspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );

      return workspace;
    }
    finally
    {
      IOUtils.closeQuietly( postWriter );
      IOUtils.closeQuietly( inputStream );
    }
  }

  /**
   * This function builds a GetFeature request XML.
   * 
   * @param wfsCaps
   * @param ftQName
   * @param filter
   * @param maxFeatureAsString
   * @return GetFeature request
   */
  private String buildGetFeatureRequestPOST( WFSCapabilities wfsCaps, final QName ftQName, final String filter, final String maxFeatureAsString )
  {
    StringBuffer sb = new StringBuffer();

    sb.append( "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n" );
    sb.append( "<wfs:GetFeature " );

    String oFormat = "text/xml; subtype=gml/3.1.1";
    if( oFormat != null )
      sb.append( " outputFormat=\"" + oFormat + "\"" );

    String version = wfsCaps.getVersion();
    if( version != null && version.length() > 0 )
      sb.append( " version=\"" + version + "\" " );
    sb.append( " xmlns:gml=\"http://www.opengis.net/gml\" " );
    sb.append( " xmlns:wfs=\"http://www.opengis.net/wfs\"" );
    sb.append( " xmlns:ogc=\"http://www.opengis.net/ogc\"" );
    if( maxFeatureAsString != null && maxFeatureAsString.length() > 0 )
      sb.append( " maxFeatures=\"" + maxFeatureAsString + "\"" );
    sb.append( " >\n" );
    String namespaceURI = ftQName.getNamespaceURI();
    String localPart = ftQName.getLocalPart();
    if( version == null ) // deegree1 gazetteer
      sb.append( "<wfs:Query typeName=\"" + localPart + "\">\n" );
    else if( namespaceURI != null && namespaceURI.length() > 0 )
      sb.append( "<wfs:Query typeName=\"sn99:" + localPart + "\" xmlns:sn99=\"" + namespaceURI + "\">\n" );
    else
      sb.append( "<wfs:Query typeName=\"" + localPart + "\">\n" );

    if( filter != null && filter.length() > 0 )
      sb.append( filter ).append( "\n" );

    sb.append( "</wfs:Query>\n" );
    sb.append( "</wfs:GetFeature>" );

    System.out.println( sb.toString() );

    return sb.toString();
  }

  public URL buildDescribeURLForFeatureType( QName featureType ) throws Exception
  {
    org.deegree.ogcwebservices.getcapabilities.Operation[] ops = this.m_wfsCapabilities.getOperationsMetadata().getOperations();

    if( m_descrFtUrl == null )
    {
      for( int i = 0; i < ops.length && m_descrFtUrl == null; i++ )
      {
        if( ops[i].getName().equals( "DescribeFeatureType" ) )
        {
          DCPType[] dcps = ops[i].getDCPs();
          if( dcps.length > 0 )
            m_descrFtUrl = ((HTTP) dcps[0].getProtocol()).getGetOnlineResources()[0].toString();

          if( m_descrFtUrl == null )
            m_descrFtUrl = ((HTTP) dcps[0].getProtocol()).getPostOnlineResources()[0].toString();
        }
      }
    }

    if( m_descrFtUrl == null )
      throw new RuntimeException( "Service does not have a DescribeFeatureType operation accessible by HTTP GET or POST." );

    QualifiedName ft = getQualiNameByFeatureTypeName( featureType );
    String format = null;
    try
    {
      format = URLEncoder.encode( "text/xml; subtype=gml/3.1.1", "UTF-8" );
    }
    catch( UnsupportedEncodingException e )
    {
      throw e;
    }

    String serverReq = "SERVICE=WFS&REQUEST=DescribeFeatureType&version=1.1.0&OUTPUTFORMAT=" + format + "&TYPENAME=" + featureType + "&NAMESPACE=xmlns(" + ft.getPrefix() + "=" + ft.getNamespace()
        + ")";

    return new URL( m_descrFtUrl + "?" + serverReq );
  }

  /**
   * This function returns the layer of the wfs.
   * 
   * @return The layer of the wfs.
   */
  public IWFSLayer[] getLayer( )
  {
    List<IWFSLayer> list = new ArrayList<IWFSLayer>();

    final FeatureTypeList featureTypeList = m_wfsCapabilities.getFeatureTypeList();
    final WFSFeatureType[] featureTypes = featureTypeList.getFeatureTypes();
    for( int i = 0; i < featureTypes.length; i++ )
    {
      final WFSFeatureType ft = featureTypes[i];
      final QualifiedName name = ft.getName();
      final String title = ft.getTitle();
      final String srs = ft.getDefaultSRS().toASCIIString();
      QName qName = new QName( name.getNamespace().toASCIIString(), name.getLocalName(), name.getPrefix() );
      URL url = null;
      try
      {
        url = buildDescribeURLForFeatureType( qName );
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
      list.add( new WFSLayer( qName, title, url, srs ) );
    }

    return list.toArray( new IWFSLayer[list.size()] );
  }
}