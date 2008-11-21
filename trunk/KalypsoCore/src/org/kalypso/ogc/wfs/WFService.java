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
import org.kalypso.core.i18n.Messages;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
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
 * @deprecated Do not use anymore, use {@link WFSClient} instead. Used only by Gazetteer stuff; remove when gazetter is
 *             refaktored.
 * @author Holger Albert
 */
@Deprecated
public class WFService
{
  public static String WFS_PREFIX = "wfs"; //$NON-NLS-1$

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
  private final Map<QName, IGMLSchema> m_ftToSchema;

  /**
   * Maps a feature type to its geometry!
   */
  private final Map<QName, QName[]> m_geoProperties;

  private HttpClient m_httpClient;

  private String m_getFeatureUrl;

  private String m_descrFtUrl;

  public WFService( final String wfsURL ) throws Exception
  {
    m_ftToSchema = new HashMap<QName, IGMLSchema>( 10 );
    m_geoProperties = new HashMap<QName, QName[]>( 10 );

    createHttpClient();

    // TODO validate string
    setWfsURL( wfsURL + "?REQUEST=GetCapabilities&VERSION=1.1.0&SERVICE=WFS" ); //$NON-NLS-1$
    final WFSCapabilitiesDocument wfsCapsDoc = new WFSCapabilitiesDocument();

    try
    {
      wfsCapsDoc.load( new URL( this.m_wfsURL ) );
    }
    catch( final MalformedURLException e )
    {
      System.out.println( Messages.getString("org.kalypso.ogc.wfs.WFService.2") + e.getLocalizedMessage() ); //$NON-NLS-1$
      throw e;
    }
    catch( final IOException e )
    {
      System.out.println( Messages.getString("org.kalypso.ogc.wfs.WFService.3") + this.m_wfsURL + Messages.getString("org.kalypso.ogc.wfs.WFService.4") + e.getLocalizedMessage() ); //$NON-NLS-1$ //$NON-NLS-2$
      throw e;
    }
    catch( final SAXException e )
    {
      throw e;
    }

    try
    {
      m_wfsCapabilities = (WFSCapabilities) wfsCapsDoc.parseCapabilities();
    }
    catch( final InvalidCapabilitiesException e )
    {
      System.out.println( Messages.getString("org.kalypso.ogc.wfs.WFService.5") + e.getLocalizedMessage() ); //$NON-NLS-1$
      throw e;
    }
  }

  private String[] extractFeatureTypes( ) throws Exception
  {
    final WFSFeatureType[] featTypes = m_wfsCapabilities.getFeatureTypeList().getFeatureTypes();
    m_featureTypeToQName = new HashMap<QName, QualifiedName>();
    final String[] fts = new String[featTypes.length];
    for( int i = 0; i < fts.length; i++ )
    {

      final QualifiedName qn = featTypes[i].getName();
      final String prefixedName = qn.getPrefixedName();
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
  private IGMLSchema loadForFeatureType( final QName featureType ) throws Exception
  {
    final org.deegree.ogcwebservices.getcapabilities.Operation[] ops = this.m_wfsCapabilities.getOperationsMetadata().getOperations();

    boolean isGet = true;

    if( m_descrFtUrl == null )
    {
      for( int i = 0; i < ops.length && m_descrFtUrl == null; i++ )
      {
        if( ops[i].getName().equals( "DescribeFeatureType" ) ) //$NON-NLS-1$
        {
          final DCPType[] dcps = ops[i].getDCPs();
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
      throw new RuntimeException( Messages.getString("org.kalypso.ogc.wfs.WFService.7") ); //$NON-NLS-1$

    final QualifiedName ft = getQualiNameByFeatureTypeName( featureType );
    String format = null;
    try
    {
      format = URLEncoder.encode( "text/xml; subtype=gml/3.1.1", "UTF-8" ); //$NON-NLS-1$ //$NON-NLS-2$
    }
    catch( final UnsupportedEncodingException e )
    {
      throw e;
    }

    final String serverReq = "SERVICE=WFS&REQUEST=DescribeFeatureType&version=1.1.0&OUTPUTFORMAT=" + format + "&TYPENAME=" + featureType + "&NAMESPACE=xmlns(" + ft.getPrefix() + "=" + ft.getNamespace() //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    + ")"; //$NON-NLS-1$

    final String httpProtocolMethod = isGet ? "HTTP_GET" : "HTTP_POST"; //$NON-NLS-1$ //$NON-NLS-2$

    System.out.println( Messages.getString("org.kalypso.ogc.wfs.WFService.17") + httpProtocolMethod + Messages.getString("org.kalypso.ogc.wfs.WFService.18") + m_descrFtUrl + serverReq ); //$NON-NLS-1$ //$NON-NLS-2$

    final HttpMethod httpMethod = createHttpMethod( httpProtocolMethod );// new GetMethod( serverUrl );

    URI uri;
    try
    {
      uri = new URI( m_descrFtUrl, true );
      httpMethod.setURI( uri );

    }
    catch( final URIException e )
    {
      throw e;
    }

    httpMethod.setQueryString( serverReq );

    try
    {
      m_httpClient.executeMethod( httpMethod );

      final GMLSchema gmlSchema = GMLSchemaFactory.createGMLSchema( httpMethod.getResponseBodyAsStream(), null, new URL( m_descrFtUrl ) );
      return gmlSchema;
    }
    catch( final Exception e )
    {
      final String mesg = Messages.getString("org.kalypso.ogc.wfs.WFService.19"); //$NON-NLS-1$
      System.out.println( mesg + Messages.getString("org.kalypso.ogc.wfs.WFService.20") + featureType + Messages.getString("org.kalypso.ogc.wfs.WFService.21") + uri + Messages.getString("org.kalypso.ogc.wfs.WFService.22") + m_descrFtUrl + "?" + serverReq ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
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
  private void createSchemaForFeatureType( final QName featureTypeName ) throws Exception
  {
    final IGMLSchema xsd = loadForFeatureType( featureTypeName );
    if( xsd == null )
      return;

    this.m_ftToSchema.put( featureTypeName, xsd );

    final QName[] geoProp = guessGeomProperty( xsd );

    this.m_geoProperties.put( featureTypeName, geoProp );
  }

  /**
   * guess which property might be "the" geometry property
   *
   * @param propNames
   * @return
   */
  protected QName[] guessGeomProperty( final IGMLSchema schema )
  {
    QName[] geoPropNames = null;
    final List<QName> tmpList = new ArrayList<QName>( 20 );

    final IFeatureType[] fts = schema.getAllFeatureTypes();
    for( final IFeatureType ft : fts )
    {
      final IPropertyType[] props = ft.getAllGeomteryProperties();
      for( final IPropertyType prop : props )
        tmpList.add( prop.getQName() );
    }

    geoPropNames = tmpList.toArray( new QName[tmpList.size()] );
    return geoPropNames;
  }

  public void setWfsURL( final String wfsURL )
  {
    if( wfsURL == null || "".equals( wfsURL ) ) //$NON-NLS-1$
      throw new IllegalArgumentException( Messages.getString("org.kalypso.ogc.wfs.WFService.25") ); //$NON-NLS-1$

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

  public QName[] getFeatureProperties( final QName featureType ) throws Exception
  {
    createSchemaForFeatureType( featureType );

    final List<QName> propsList = new ArrayList<QName>();

    final IGMLSchema schema = this.m_ftToSchema.get( featureType );

    final IFeatureType[] fts = schema.getAllFeatureTypes();
    for( final IFeatureType ft : fts )
    {
      final IPropertyType[] props = ft.getProperties();
      for( final IPropertyType prop : props )
      {
        // if( !(props[j].getType() == Types.GEOMETRY) )
        propsList.add( prop.getQName() );
      }
    }

    return propsList.toArray( new QName[propsList.size()] );
  }

  public IGMLSchema getSchemaForFeatureType( final QName featureType )
  {
    return this.m_ftToSchema.get( featureType );
  }

  public QName[] getGeometryProperties( final QName featureType )
  {
    return this.m_geoProperties.get( featureType );
  }

  public QualifiedName getQualiNameByFeatureTypeName( final QName ftName )
  {
    return m_featureTypeToQName.get( ftName );
  }

  public String getGetFeatureURL( )
  {
    final org.deegree.ogcwebservices.getcapabilities.Operation[] ops = this.m_wfsCapabilities.getOperationsMetadata().getOperations();
    m_getFeatureUrl = null;

    for( int i = 0; i < ops.length && m_getFeatureUrl == null; i++ )
    {
      if( ops[i].getName().equals( "GetFeature" ) ) //$NON-NLS-1$
      {
        final DCPType[] dcps = ops[i].getDCPs();
        if( dcps.length > 0 )
          m_getFeatureUrl = ((HTTP) dcps[0].getProtocol()).getPostOnlineResources()[0].toString();
      }
    }

    if( m_getFeatureUrl == null )
      throw new RuntimeException( Messages.getString("org.kalypso.ogc.wfs.WFService.27") ); //$NON-NLS-1$

    return m_getFeatureUrl;
  }

  String getCrsForFeatureType( final QualifiedName featureTypeName )
  {
    String crs = null;

    final FeatureTypeList ftl = this.m_wfsCapabilities.getFeatureTypeList();

    final QualifiedName qn = featureTypeName;// getQualiNameByFeatureTypeName( featureType );
    final WFSFeatureType ft = ftl.getFeatureType( qn );
    crs = ft.getDefaultSRS().toASCIIString();

    return crs;
  }

  private void createHttpClient( )
  {
    m_httpClient = new HttpClient();

    final HttpClientParams clientPars = new HttpClientParams();
    clientPars.setConnectionManagerTimeout( 60000 );

    m_httpClient.setParams( clientPars );

  }

  private HttpMethod createHttpMethod( final String methodName )
  {
    HttpMethod httpMethod = null;

    if( "HTTP_GET".equals( methodName ) ) //$NON-NLS-1$
      httpMethod = new GetMethod();
    else if( "HTTP_POST".equals( methodName ) ) //$NON-NLS-1$
      httpMethod = new PostMethod();
    else
      throw new IllegalArgumentException( Messages.getString("org.kalypso.ogc.wfs.WFService.30") ); //$NON-NLS-1$

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
    final ArrayList<String> operators = new ArrayList<String>();

    final SpatialOperator[] spatialOperators = m_wfsCapabilities.getFilterCapabilities().getSpatialCapabilities().getSpatialOperators();
    for( final SpatialOperator spatialOperator : spatialOperators )
      operators.add( spatialOperator.getName() );

    final QualifiedName[] geometryOperands = m_wfsCapabilities.getFilterCapabilities().getSpatialCapabilities().getGeometryOperands();
    for( final QualifiedName qualifiedName : geometryOperands )
      operators.add( qualifiedName.getLocalName() );

    final Operator[] arithmeticOperators = m_wfsCapabilities.getFilterCapabilities().getScalarCapabilities().getArithmeticOperators();
    for( final Operator operator : arithmeticOperators )
      operators.add( operator.getName() );

    final Operator[] comparisonOperators = m_wfsCapabilities.getFilterCapabilities().getScalarCapabilities().getComparisonOperators();
    for( final Operator operator : comparisonOperators )
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
  public GMLWorkspace createGMLWorkspaceFromGetFeature( final QName featureTypeToLoad, final String targetCRS, final String filter, final String maxFeatureAsString ) throws Exception
  {
    BufferedInputStream inputStream = null;
    PrintStream postWriter = null;

    /* Create the GetFeature URL. */
    final String getFeatureURL = getGetFeatureURL();
    final URL getFeature = new URL( getFeatureURL );

    try
    {
      final URLConnection con = getFeature.openConnection();
      con.setDoOutput( true );
      con.setDoInput( true );

      /* Write request to the WFS server. */
      final OutputStream connectionOutputStream = con.getOutputStream();
      postWriter = new PrintStream( connectionOutputStream );

      final String getFeaturePost = buildGetFeatureRequestPOST( m_wfsCapabilities, featureTypeToLoad, filter, maxFeatureAsString );
      postWriter.print( getFeaturePost );

      /* Read response from the WFS server and create a GMLWorkspace. */
      inputStream = new BufferedInputStream( con.getInputStream() );

      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( inputStream, buildDescribeURLForFeatureType( featureTypeToLoad ), null );
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
  private String buildGetFeatureRequestPOST( final WFSCapabilities wfsCaps, final QName ftQName, final String filter, final String maxFeatureAsString )
  {
    final StringBuffer sb = new StringBuffer();

    sb.append( "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n" ); //$NON-NLS-1$
    sb.append( "<wfs:GetFeature " ); //$NON-NLS-1$

    final String oFormat = "text/xml; subtype=gml/3.1.1"; //$NON-NLS-1$
    if( oFormat != null )
      sb.append( " outputFormat=\"" + oFormat + "\"" ); //$NON-NLS-1$ //$NON-NLS-2$

    final String version = wfsCaps.getVersion();
    if( version != null && version.length() > 0 )
      sb.append( " version=\"" + version + "\" " ); //$NON-NLS-1$ //$NON-NLS-2$
    sb.append( " xmlns:gml=\"http://www.opengis.net/gml\" " ); //$NON-NLS-1$
    sb.append( " xmlns:wfs=\"http://www.opengis.net/wfs\"" ); //$NON-NLS-1$
    sb.append( " xmlns:ogc=\"http://www.opengis.net/ogc\"" ); //$NON-NLS-1$
    if( maxFeatureAsString != null && maxFeatureAsString.length() > 0 )
      sb.append( " maxFeatures=\"" + maxFeatureAsString + "\"" ); //$NON-NLS-1$ //$NON-NLS-2$
    sb.append( " >\n" ); //$NON-NLS-1$
    final String namespaceURI = ftQName.getNamespaceURI();
    final String localPart = ftQName.getLocalPart();
    if( version == null ) // deegree1 gazetteer
      sb.append( "<wfs:Query typeName=\"" + localPart + "\">\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    else if( namespaceURI != null && namespaceURI.length() > 0 )
      sb.append( "<wfs:Query typeName=\"sn99:" + localPart + "\" xmlns:sn99=\"" + namespaceURI + "\">\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    else
      sb.append( "<wfs:Query typeName=\"" + localPart + "\">\n" ); //$NON-NLS-1$ //$NON-NLS-2$

    if( filter != null && filter.length() > 0 )
      sb.append( filter ).append( "\n" ); //$NON-NLS-1$

    sb.append( "</wfs:Query>\n" ); //$NON-NLS-1$
    sb.append( "</wfs:GetFeature>" ); //$NON-NLS-1$

    System.out.println( sb.toString() );

    return sb.toString();
  }

  public URL buildDescribeURLForFeatureType( final QName featureType ) throws Exception
  {
    final org.deegree.ogcwebservices.getcapabilities.Operation[] ops = this.m_wfsCapabilities.getOperationsMetadata().getOperations();

    if( m_descrFtUrl == null )
    {
      for( int i = 0; i < ops.length && m_descrFtUrl == null; i++ )
      {
        if( ops[i].getName().equals( "DescribeFeatureType" ) ) //$NON-NLS-1$
        {
          final DCPType[] dcps = ops[i].getDCPs();
          if( dcps.length > 0 )
            m_descrFtUrl = ((HTTP) dcps[0].getProtocol()).getGetOnlineResources()[0].toString();

          if( m_descrFtUrl == null )
            m_descrFtUrl = ((HTTP) dcps[0].getProtocol()).getPostOnlineResources()[0].toString();
        }
      }
    }

    if( m_descrFtUrl == null )
      throw new RuntimeException( Messages.getString("org.kalypso.ogc.wfs.WFService.55") ); //$NON-NLS-1$

    final QualifiedName ft = getQualiNameByFeatureTypeName( featureType );
    String format = null;
    try
    {
      format = URLEncoder.encode( "text/xml; subtype=gml/3.1.1", "UTF-8" ); //$NON-NLS-1$ //$NON-NLS-2$
    }
    catch( final UnsupportedEncodingException e )
    {
      throw e;
    }

    final String serverReq = "SERVICE=WFS&REQUEST=DescribeFeatureType&version=1.1.0&OUTPUTFORMAT=" + format + "&TYPENAME=" + featureType + "&NAMESPACE=xmlns(" + ft.getPrefix() + "=" + ft.getNamespace() //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    + ")"; //$NON-NLS-1$

    return new URL( m_descrFtUrl + "?" + serverReq ); //$NON-NLS-1$
  }

  /**
   * This function returns the layer of the wfs.
   *
   * @return The layer of the wfs.
   */
  public IWFSLayer[] getLayer( )
  {
    final List<IWFSLayer> list = new ArrayList<IWFSLayer>();

    final FeatureTypeList featureTypeList = m_wfsCapabilities.getFeatureTypeList();
    final WFSFeatureType[] featureTypes = featureTypeList.getFeatureTypes();
    for( final WFSFeatureType ft : featureTypes )
    {
      final QualifiedName name = ft.getName();
      final String title = ft.getTitle();
      final String srs = ft.getDefaultSRS().toASCIIString();
      final QName qName = new QName( name.getNamespace().toASCIIString(), name.getLocalName(), name.getPrefix() );
      URL url = null;
      try
      {
        url = buildDescribeURLForFeatureType( qName );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
      list.add( new WFSLayer( qName, title, url, srs ) );
    }

    return list.toArray( new IWFSLayer[list.size()] );
  }
}