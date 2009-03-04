package org.kalypso.ogc.wfs;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Formatter;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.deegree.datatypes.QualifiedName;
import org.deegree.model.filterencoding.capabilities.Operator;
import org.deegree.model.filterencoding.capabilities.SpatialOperator;
import org.deegree.ogcwebservices.getcapabilities.DCPType;
import org.deegree.ogcwebservices.getcapabilities.HTTP;
import org.deegree.ogcwebservices.getcapabilities.InvalidCapabilitiesException;
import org.deegree.ogcwebservices.getcapabilities.Operation;
import org.deegree.ogcwebservices.getcapabilities.Protocol;
import org.deegree.ogcwebservices.wfs.capabilities.FeatureTypeList;
import org.deegree.ogcwebservices.wfs.capabilities.WFSCapabilities;
import org.deegree.ogcwebservices.wfs.capabilities.WFSCapabilitiesDocument;
import org.deegree.ogcwebservices.wfs.capabilities.WFSFeatureType;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.net.ProxyUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.net.UrlUtilities;
import org.kalypso.core.i18n.Messages;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.xml.sax.SAXException;

/**
 * An WebFeatureServiceClient. Implements the basic operations to access an OGC-WebFeatureService.
 * 
 * @author Gernot Belger
 */
public class WFSClient
{
  public static final String OPERATION_DESCRIBE_FEATURE_TYPE = "DescribeFeatureType"; //$NON-NLS-1$

  public static final String OPERATION_GET_CAPABILITIES = "GetCapabilities"; //$NON-NLS-1$

  private static final String OPERATION_GET_FEATURE = "GetFeature";

  // TODO: move these in constant interface
  public static final String URL_PARAM_VERSION = "VERSION"; //$NON-NLS-1$

  public static final String URL_PARAM_VERSION_DEFAULT = "1.1.0"; //$NON-NLS-1$

  public static final String URL_PARAM_SERVICE = "SERVICE"; //$NON-NLS-1$

  public static final String URL_PARAM_SERVICE_DEFAULT = "WFS"; //$NON-NLS-1$

  public static final String URL_PARAM_REQUEST = "REQUEST"; //$NON-NLS-1$

  private static final String PARAM_DESCRIBE_FEATURE_TYPE_NAMESPACE = "NAMESPACE"; //$NON-NLS-1$

  private static final String PARAM_DESCRIBE_FEATURE_TYPE_TYPENAME = "TYPENAME"; //$NON-NLS-1$

  public static String PARAM_DESCRIBE_FEATURE_TYPE_FORMAT = "OUTPUTFORMAT"; //$NON-NLS-1$

  private static final String OUTPUT_FORMAT = "text/xml; subtype=gml/3.1.1";

  public static String PARAM_DESCRIBE_FEATURE_TYPE_FORMAT_DEFAULT;
  {
    try
    {
      PARAM_DESCRIBE_FEATURE_TYPE_FORMAT_DEFAULT = URLEncoder.encode( OUTPUT_FORMAT, "UTF-8" );
    }
    catch( final UnsupportedEncodingException e )
    {
      // this will never happen
      e.printStackTrace();
    }
  }

  private final URL m_wfsURL;

  private WFSCapabilities m_wfsCapabilities;

  private final HttpClient m_httpClient;

  /** All Feature types supported by this WFS. */
  private final Map<QName, WFSFeatureType> m_featureTypes = new HashMap<QName, WFSFeatureType>();

  private final Map<WFSFeatureType, IFeatureType> m_schemaHash = new HashMap<WFSFeatureType, IFeatureType>();

  public WFSClient( final URL wfsURL )
  {
    m_wfsURL = wfsURL;
    m_httpClient = createHttpClient( wfsURL );
  }

  public URL getUrl( )
  {
    return m_wfsURL;
  }

  /**
   * Initialises the client. Must be called before any other method is called.
   */
  public IStatus load( )
  {
    try
    {
      final WFSCapabilitiesDocument wfsCapsDoc = new WFSCapabilitiesDocument();
      final URL capasUrl = createCapabilitiesUrl( m_wfsURL );
      wfsCapsDoc.load( capasUrl );
      m_wfsCapabilities = (WFSCapabilities) wfsCapsDoc.parseCapabilities();

      /* Hash the feature types */
      final FeatureTypeList featureTypeList = m_wfsCapabilities.getFeatureTypeList();
      final WFSFeatureType[] featureTypes = featureTypeList.getFeatureTypes();
      for( final WFSFeatureType featureType : featureTypes )
      {
        final QualifiedName name = featureType.getName();
        final QName qname = new QName( name.getNamespace().toString(), name.getLocalName(), name.getPrefix() );
        m_featureTypes.put( qname, featureType );
      }

      return Status.OK_STATUS;
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
      return StatusUtilities.createStatus( IStatus.ERROR, Messages.format( "org.kalypso.ogc.wfs.WFSClient.1", m_wfsURL.toExternalForm() ), e );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      return StatusUtilities.createStatus( IStatus.ERROR, Messages.format( "org.kalypso.ogc.wfs.WFSClient.2", m_wfsURL.toExternalForm() ), e );
    }
    catch( final SAXException e )
    {
      e.printStackTrace();
      return StatusUtilities.createStatus( IStatus.ERROR, Messages.format( "org.kalypso.ogc.wfs.WFSClient.3", m_wfsURL.toExternalForm() ), e );
    }
    catch( final InvalidCapabilitiesException e )
    {
      e.printStackTrace();
      return StatusUtilities.createStatus( IStatus.ERROR, Messages.format( "org.kalypso.ogc.wfs.WFSClient.4", m_wfsURL.toExternalForm() ), e );
    }
  }

  /**
   * Creates an url to the capabilities document for a given WFs-base-URL. I.e. adds the appropriate query part to the
   * given url.
   */
  private static URL createCapabilitiesUrl( final URL wfsURL ) throws MalformedURLException
  {
    final Map<String, String> params = UrlUtilities.parseQuery( wfsURL );

    /* Check if necessary parameters already exists; if yes they must fit else we have a problem */

    /* SERVICE */
    if( params.containsKey( URL_PARAM_SERVICE ) )
    {
      final String serviceName = params.get( URL_PARAM_SERVICE );
      if( !URL_PARAM_SERVICE_DEFAULT.equals( serviceName ) )
        throw new IllegalArgumentException( "WFS-URL contains wrong service parameter: " + serviceName );
    }
    else
    {
      params.put( URL_PARAM_SERVICE, URL_PARAM_SERVICE_DEFAULT );
    }

    /* VERSION */
    if( params.containsKey( URL_PARAM_VERSION ) )
    {
      final String version = params.get( URL_PARAM_VERSION );
      if( !URL_PARAM_VERSION_DEFAULT.equals( version ) )
        throw new IllegalArgumentException( "WFS-URL contains unsupported version parameter: " + version );
    }
    else
    {
      params.put( URL_PARAM_VERSION, URL_PARAM_VERSION_DEFAULT );
    }

    /* REQUEST */
    if( params.containsKey( URL_PARAM_REQUEST ) )
    {
      final String request = params.get( URL_PARAM_REQUEST );
      if( !OPERATION_GET_CAPABILITIES.equals( request ) )
        throw new IllegalArgumentException( "WFS-URL contains already another request parameter: " + request );
    }
    else
    {
      params.put( URL_PARAM_REQUEST, OPERATION_GET_CAPABILITIES );
    }

    return UrlUtilities.addQuery( wfsURL, params );
  }

  public WFSFeatureType[] getFeatureTypes( )
  {
    return m_featureTypes.values().toArray( new WFSFeatureType[m_featureTypes.size()] );
  }

  public WFSFeatureType getFeatureType( final QName name )
  {
    return m_featureTypes.get( name );
  }

  private static HttpClient createHttpClient( final URL url )
  {
    final HttpClient client = ProxyUtilities.getConfiguredHttpClient( 60000, url, 3 );

    return client;
  }

  /**
   * This function returns all filter capabilities operations for the wfs.
   * 
   * @return All filter capabilities operations.
   */
  public String[] getAllFilterCapabilitesOperations( )
  {
    final List<String> operators = new ArrayList<String>();

    final SpatialOperator[] spatialOperators = m_wfsCapabilities.getFilterCapabilities().getSpatialCapabilities().getSpatialOperators();
    for( final SpatialOperator spatialOperator : spatialOperators )
    {
      operators.add( spatialOperator.getName() );
    }

    final QualifiedName[] geometryOperands = m_wfsCapabilities.getFilterCapabilities().getSpatialCapabilities().getGeometryOperands();
    for( final QualifiedName qualifiedName : geometryOperands )
    {
      operators.add( qualifiedName.getLocalName() );
    }

    final Operator[] arithmeticOperators = m_wfsCapabilities.getFilterCapabilities().getScalarCapabilities().getArithmeticOperators();
    for( final Operator operator : arithmeticOperators )
    {
      operators.add( operator.getName() );
    }

    final Operator[] comparisonOperators = m_wfsCapabilities.getFilterCapabilities().getScalarCapabilities().getComparisonOperators();
    for( final Operator operator : comparisonOperators )
    {
      operators.add( operator.getName() );
    }

    return operators.toArray( new String[] {} );
  }

  public Operation getOperation( final String operationName )
  {
    final Operation[] operations = m_wfsCapabilities.getOperationsMetadata().getOperations();
    for( final Operation operation : operations )
    {
      if( operation.getName().equals( operationName ) )
        return operation;
    }

    return null;
  }

  /**
   * Finds the first get operation {@link URL} for the given operation.
   */
  public URL findGetOperationURL( final String operationName )
  {
    final Operation operation = getOperation( operationName );
    if( operation == null )
      return null;

    final DCPType[] dcps = operation.getDCPs();
    for( final DCPType dcp : dcps )
    {
      final Protocol protocol = dcp.getProtocol();
      if( protocol instanceof HTTP )
      {
        final URL[] getOnlineResources = ((HTTP) protocol).getGetOnlineResources();
        if( getOnlineResources.length > 0 )
          return getOnlineResources[0];
      }
    }

    return null;
  }

  /**
   * Finds the first post operation {@link URL} for the given operation.
   */
  public URL findPostOperationURL( final String operationName )
  {
    final Operation operation = getOperation( operationName );
    if( operation == null )
      return null;

    final DCPType[] dcps = operation.getDCPs();
    for( final DCPType dcp : dcps )
    {
      final Protocol protocol = dcp.getProtocol();
      if( protocol instanceof HTTP )
      {
        final URL[] postOnlineResources = ((HTTP) protocol).getPostOnlineResources();
        if( postOnlineResources.length > 0 )
          return postOnlineResources[0];
      }
    }

    return null;
  }

  /**
   * Implementation of the 'DescribeFeatureType'-Operation. Returns the URL to the schema document.
   */
  public URL createDescribeFeatureTypeURL( final QName name ) throws MalformedURLException
  {
    final WFSFeatureType featureType = getFeatureType( name );
    final URL describeUrl = findGetOperationURL( WFSClient.OPERATION_DESCRIBE_FEATURE_TYPE );
    if( describeUrl == null )
      throw new IllegalStateException( "WFS does not support GET for " + WFSClient.OPERATION_DESCRIBE_FEATURE_TYPE );

    final QualifiedName qname = featureType.getName();

    final Map<String, String> params = UrlUtilities.parseQuery( describeUrl );
    if( !params.containsKey( URL_PARAM_SERVICE ) )
    {
      params.put( URL_PARAM_SERVICE, URL_PARAM_SERVICE_DEFAULT );
    }

    params.put( URL_PARAM_VERSION, m_wfsCapabilities.getVersion() );
    params.put( URL_PARAM_REQUEST, OPERATION_DESCRIBE_FEATURE_TYPE );
    params.put( PARAM_DESCRIBE_FEATURE_TYPE_FORMAT, PARAM_DESCRIBE_FEATURE_TYPE_FORMAT_DEFAULT );

    final String namespaceUri = qname.getNamespace().toString();
    final String prefix = qname.getPrefix();
    final String namespaceValue = String.format( "xmlns(%s=%s)", prefix, namespaceUri );
    params.put( PARAM_DESCRIBE_FEATURE_TYPE_NAMESPACE, namespaceValue );

    final String typenameValue = String.format( "%s:%s", prefix, qname.getLocalName() );
    params.put( PARAM_DESCRIBE_FEATURE_TYPE_TYPENAME, typenameValue );

    return UrlUtilities.addQuery( describeUrl, params );
  }

  public WFSCapabilities operationGetCapabilities( )
  {
    return m_wfsCapabilities;
  }

  public URL operationDescribeFeatureType( final QName name ) throws CoreException
  {
    try
    {
      return createDescribeFeatureTypeURL( name );
    }
    catch( final MalformedURLException e )
    {
      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, Messages.format( "org.kalypso.ogc.wfs.WFSClient.1", m_wfsURL.toExternalForm() ), e );
      throw new CoreException( status );
    }
  }

  /**
   * Implementation of the 'GetFeature'-Operation. Returns XXXX?
   */
  // TODO: exception handling
  public GMLWorkspace operationGetFeature( final QName name, final String filter, final Integer maxFeatures ) throws CoreException
  {
    InputStream inputStream = null;
    try
    {
// /* First, get schema */
// final URL schemaLocation = operationDescribeFeatureType( name );
// // HACK: force schema into schema-catalog-cache; it would be better just to make it known to the catalog and load
// // it
// // on demand
// // for in order to do this the IUrlCatalog stuff should be refactored
// final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
// schemaCatalog.getSchema( name.getNamespaceURI(), "3.1.1", schemaLocation );

      /* Create getFeature URL */
      final WFSFeatureType featureType = getFeatureType( name );
// final URL getUrl = findPostOperationURL( WFSClient.OPERATION_GET_FEATURE );
// if( getUrl == null )
// {
// final String msg = String.format( "WFS does not support HTTP-GET for '%s'", WFSClient.OPERATION_DESCRIBE_FEATURE_TYPE
      // );
// throw new IllegalStateException( msg );
// }

      final QualifiedName qname = featureType.getName();

      final Map<String, String> params = new HashMap<String, String>(); // UrlUtilities.parseQuery( getUrl );
// }

      params.put( URL_PARAM_SERVICE, "WFS" );
      params.put( URL_PARAM_VERSION, m_wfsCapabilities.getVersion() );
      params.put( URL_PARAM_REQUEST, OPERATION_GET_FEATURE );
      params.put( PARAM_DESCRIBE_FEATURE_TYPE_FORMAT, PARAM_DESCRIBE_FEATURE_TYPE_FORMAT_DEFAULT );
      params.put( PARAM_DESCRIBE_FEATURE_TYPE_NAMESPACE, qname.getNamespace().toString() );
      params.put( PARAM_DESCRIBE_FEATURE_TYPE_TYPENAME, qname.getLocalName() );

      /* Post the request */
      final URL url = getGetUrl( name, params );
      final GetMethod getMethod = new GetMethod( url.toURI().toString() );

// final PostMethod postMethod = new PostMethod( getUrl.toURI().toString() );
// final RequestEntity requestEntity = new StringRequestEntity( sb.toString(), "text/xml", "UTF-8" );
// postMethod.setRequestEntity( requestEntity );

      final int statusCode = m_httpClient.executeMethod( getMethod );
      if( statusCode != 200 )
      {
        // REMARK: with OGC-Services, it's always 200, even on error...
        System.out.println( "Status Code: " + statusCode );
        throw new HttpException( "Connection error: " + statusCode );
      }

      final String responseBodyAsString = getMethod.getResponseBodyAsString();
      FileUtils.writeStringToFile( new File( "C:\\temp\\blubb.gml" ), responseBodyAsString, "UTF-8" );

      inputStream = new BufferedInputStream( getMethod.getResponseBodyAsStream() );

      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( inputStream, null, null );
      inputStream.close();
      return workspace;
    }
// catch( final InvocationTargetException e )
// {
// final Throwable targetException = e.getTargetException();
// final String message = String.format( "Failed to load schema for type %s", name );
// final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, message, targetException );
// throw new CoreException( status );
// }
    catch( final URISyntaxException e )
    {
      final String message = Messages.format( "org.kalypso.ogc.wfs.WFSClient.1", e.getInput() );
      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, message, e );
      throw new CoreException( status );
    }
    catch( final UnsupportedEncodingException e )
    {
      // should never happen
      final String message = String.format( e.toString() );
      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, message, e );
      throw new CoreException( status );
    }
    catch( final HttpException e )
    {
      final String message = String.format( "Failed to access WFS", name );
      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, message, e );
      throw new CoreException( status );
    }
    catch( final IOException e )
    {
      final String message = String.format( "Failed to access WFS", name );
      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, message, e );
      throw new CoreException( status );
    }
    catch( final Exception e )
    {
      final String message = String.format( "Failed to load result GML", name );
      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, message, e );
      throw new CoreException( status );
    }
    finally
    {
      IOUtils.closeQuietly( inputStream );
    }
  }

  public URL getGetUrl( final QName name, final Map<String, String> params ) throws MalformedURLException
  {
    final WFSFeatureType featureType = getFeatureType( name );
    final URL describeUrl = findGetOperationURL( WFSClient.OPERATION_DESCRIBE_FEATURE_TYPE );
    if( describeUrl == null )
      throw new IllegalStateException( "WFS does not support GET for " + WFSClient.OPERATION_DESCRIBE_FEATURE_TYPE );

    final QualifiedName qname = featureType.getName();

    final String namespaceUri = qname.getNamespace().toString();
    final String prefix = qname.getPrefix();
    final String namespaceValue = String.format( "xmlns(%s=%s)", prefix, namespaceUri );
    params.put( PARAM_DESCRIBE_FEATURE_TYPE_NAMESPACE, namespaceValue );

    final String typenameValue = String.format( "%s:%s", prefix, qname.getLocalName() );
    params.put( PARAM_DESCRIBE_FEATURE_TYPE_TYPENAME, typenameValue );

    return UrlUtilities.addQuery( describeUrl, params );
  }

  /**
   * This function writes a GetFeature request XML into a given {@link Formatter}.<br>
   */
  private void formatGetFeatureRequestPOST( final Formatter formater, final QName ftQName, final String filter, final Integer maxFeatures )
  {
    formater.format( "<?xml version=\"1.0\" encoding=\"UTF-8\"?>%n" ); //$NON-NLS-1$
    formater.format( "<wfs:GetFeature outputFormat=\"%s\"", OUTPUT_FORMAT ); //$NON-NLS-1$

    final String version = m_wfsCapabilities.getVersion();
    if( version != null && version.length() > 0 )
    {
      formater.format( " version=\"" + version + "\" " ); //$NON-NLS-1$ //$NON-NLS-2$
    }
//    formater.format( " xmlns:gml=\"http://www.opengis.net/gml\" " ); //$NON-NLS-1$
    formater.format( " xmlns:wfs=\"http://www.opengis.net/wfs\"" ); //$NON-NLS-1$
//    formater.format( " xmlns:ogc=\"http://www.opengis.net/ogc\"" ); //$NON-NLS-1$
    if( maxFeatures != null )
    {
      formater.format( " maxFeatures=\"%d\"", maxFeatures ); //$NON-NLS-1$
    }
    formater.format( " >%n" ); //$NON-NLS-1$

    final String namespaceURI = ftQName.getNamespaceURI();
    final String localPart = ftQName.getLocalPart();
    if( version == null )
    {
      formater.format( "<wfs:Query typeName=\"%s\">%n", localPart ); //$NON-NLS-1$
    }
    else if( namespaceURI != null && namespaceURI.length() > 0 )
    {
      formater.format( "<wfs:Query typeName=\"sn99:" + localPart + "\" xmlns:sn99=\"" + namespaceURI + "\">\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }
    else
    {
      formater.format( "<wfs:Query typeName=\"%s\">%n", localPart ); //$NON-NLS-1$
    }

    if( filter != null && filter.length() > 0 )
    {
      formater.format( "%s%n", filter ); //$NON-NLS-1$
    }

    formater.format( "</wfs:Query>%n" ); //$NON-NLS-1$
    formater.format( "</wfs:GetFeature>%n" ); //$NON-NLS-1$
  }

  public IFeatureType getFeatureType( final WFSFeatureType type ) throws CoreException
  {
    try
    {
      if( m_schemaHash.containsKey( type ) )
        return m_schemaHash.get( type );

      final QualifiedName name = type.getName();
      final QName qname = new QName( name.getNamespace().toString(), name.getLocalName() );
      final URL schemaLocation = operationDescribeFeatureType( qname );

      final IGMLSchema schema = GMLSchemaFactory.createGMLSchema( "3.1.1", schemaLocation );
      final IFeatureType featureType = schema.getFeatureType( qname );

      m_schemaHash.put( type, featureType );

      return featureType;
    }
    catch( final GMLSchemaException e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }
  }
}