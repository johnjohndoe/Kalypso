package org.kalypso.ogc.gml.map.themes;

import java.awt.Point;
import java.net.URL;
import java.rmi.RemoteException;
import java.util.HashMap;

import org.deegree.services.WebServiceException;
import org.deegree.services.capabilities.DCPType;
import org.deegree.services.capabilities.HTTP;
import org.deegree.services.capabilities.Protocol;
import org.deegree.services.wms.capabilities.Operation;
import org.deegree.services.wms.capabilities.Request;
import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.deegree_impl.services.wms.RemoteWMService;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.ogc.gml.wms.WMSCapabilitiesHelper;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.ct.GeoTransformer;
import org.kalypsodeegree_impl.tools.WMSHelper;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Overwritten in order to better encapsulate the service.
 */
final class KalypsoRemoteWMService extends RemoteWMService
{
  private final CS_CoordinateSystem m_srs;

  private final String m_layers;

  private final String m_styles;

  public KalypsoRemoteWMService( final WMSCapabilities capas, final CS_CoordinateSystem srs, final String layers, final String styles ) throws WebServiceException
  {
    super( capas );

    m_srs = srs;
    m_layers = layers;
    m_styles = styles;
  }

  /**
   * @see org.deegree_impl.services.OGCWebService_Impl#getCapabilities()
   */
  public WMSCapabilities getWMSCapabilities( )
  {
    return capabilities;
  }

  public static KalypsoRemoteWMService initializeService( final URL serviceUrl, final CS_CoordinateSystem preferedSRS, final String layers, final String styles, final IProgressMonitor monitor ) throws CoreException
  {
    final WMSCapabilities wmsCaps = WMSCapabilitiesHelper.loadCapabilities( serviceUrl, monitor );
    final CS_CoordinateSystem srs = negotiateCRS( preferedSRS, wmsCaps, layers.split( "," ) );

    try
    {
      return new KalypsoRemoteWMService( wmsCaps, srs, layers, styles );
    }
    catch( final WebServiceException e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e, "WebMapService konnte nicht initialisiert werden: " + e.getLocalizedMessage() );
      KalypsoGisPlugin.getDefault().getLog().log( status );
      throw new CoreException( status );
    }
  }

  /**
   * @see WMSHelper#negotiateCRS(CS_CoordinateSystem, WMSCapabilities, String[])
   */
  private static CS_CoordinateSystem negotiateCRS( final CS_CoordinateSystem localSRS, final WMSCapabilities wmsCapabilities, final String[] layers ) throws CoreException
  {
    // match the local with the remote coordinate system
    try
    {
      final CS_CoordinateSystem[] crs = WMSHelper.negotiateCRS( localSRS, wmsCapabilities, layers );
      if( crs.length > 0 )
        return crs[0];
    }
    catch( final RemoteException e )
    {
      final IStatus errorStatus = StatusUtilities.createErrorStatus( "Failed to negotiate CRS." );
      KalypsoGisPlugin.getDefault().getLog().log( errorStatus );
      throw new CoreException( errorStatus );
    }

    return localSRS;
  }

  public GM_Envelope getMaxExtend( final CS_CoordinateSystem localSRS ) throws CoreException
  {
    try
    {
      final GM_Envelope maxEnvRemoteSRS = WMSHelper.getMaxExtend( m_layers.split( "," ), capabilities, m_srs );
      final GeoTransformer gt = new GeoTransformer( localSRS );
      return gt.transformEnvelope( maxEnvRemoteSRS, m_srs );
    }
    catch( final Exception e )
    {
      final IStatus errorStatus = StatusUtilities.statusFromThrowable( e, "Failed to determine extent." );
      KalypsoGisPlugin.getDefault().getLog().log( errorStatus );
      throw new CoreException( errorStatus );
    }
  }

  // TODO directly return GetMapRequest
  // TODO move this into static helper (protocol factory)?
  public HashMap<String, String> createGetMapRequestParameter( final int width, final int height, final GM_Envelope requestedEnvLocalSRS, final CS_CoordinateSystem localSRS ) throws CoreException
  {
    try
    {
      final HashMap<String, String> wmsParameter = prepareRequestParameters( "GetMap" );

      wmsParameter.put( "LAYERS", m_layers );
      if( m_styles != null )
        wmsParameter.put( "STYLES", m_styles );

      // some WMS-themes use style name="" and when deegree makes "STYLES=default" out of this, this does not work
      // I think style name="" is also not valid (can we be flexible ?)
      // ask me ( v.doemming@tuhh.de )
      wmsParameter.put( "FORMAT", "image/png" );
      wmsParameter.put( "TRANSPARENT", "TRUE" );
      wmsParameter.put( "WIDTH", "" + width );
      wmsParameter.put( "HEIGHT", "" + height );
      wmsParameter.put( "SRS", m_srs.getName() );

      final GeoTransformer gt = new GeoTransformer( m_srs );
      final GM_Envelope targetEnvRemoteSRS = gt.transformEnvelope( requestedEnvLocalSRS, localSRS );
      if( targetEnvRemoteSRS.getMax().getX() - targetEnvRemoteSRS.getMin().getX() <= 0 )
        throw new Exception( "invalid bbox" );
      if( targetEnvRemoteSRS.getMax().getY() - targetEnvRemoteSRS.getMin().getY() <= 0 )
        throw new Exception( "invalid bbox" );
      final String targetEnvRemoteSRSstring = WMSHelper.env2bboxString( targetEnvRemoteSRS );
      wmsParameter.put( "BBOX", targetEnvRemoteSRSstring );
      return wmsParameter;
    }
    catch( final Exception e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e, "Could not create map request: " );
      throw new CoreException( status );
    }
  }

  /**
   * Tries to find the operation.
   * 
   * @throws CoreException
   *             If this service does not supports this operation.
   */
  private Operation checkOperation( final String name ) throws CoreException
  {
    final Request request = capabilities.getCapability().getRequest();
    final Operation operation = request.getOperation( name );

    if( operation == null )
      throw new CoreException( StatusUtilities.createErrorStatus( "Operation nicht unterstützt: " + name ) );

    return operation;
  }

  public CS_CoordinateSystem getSRS( )
  {
    return m_srs;
  }

  // TODO: directly return the request object
  // TODO: does not work at the moment
  // TODO: Move to protocol factory
  public HashMap<String, String> createGetFeatureinfoRequest( final Point pointOfInterest, final String format ) throws CoreException
  {
    final HashMap<String, String> parameterMap = prepareRequestParameters( "GetFeatureInfo" );
    parameterMap.put( "QUERY_LAYERS", m_layers );

    if( format != null && format.length() > 0 )
      parameterMap.put( "INFO_FORMAT", format );

    parameterMap.put( "X", Integer.toString( pointOfInterest.x ) );
    parameterMap.put( "Y", Integer.toString( pointOfInterest.y ) );

// System.out.print( "?" );
// for( final String key : parameterMap.keySet() )
// {
// final String value = parameterMap.get( key );
// System.out.print( key + "=" + value + "&" );
// }
//
// System.out.println();

    parameterMap.put( "LAYERS", "dummy" );
    parameterMap.put( "FORMAT", "image/gif" );
    parameterMap.put( "WIDTH", "100" );
    parameterMap.put( "HEIGHT", "100" );
    parameterMap.put( "SRS", "100" );
    parameterMap.put( "BBOX", "100" );

    return parameterMap;
  }

  private HashMap<String, String> prepareRequestParameters( final String operationName ) throws CoreException
  {
    final HashMap<String, String> wmsParameter = new HashMap<String, String>();

    // HACK: in order to keep any existing query parts, add existing query parts from base url
    final Operation operation = checkOperation( operationName );
    final DCPType[] types = operation.getDCPTypes();
    for( final DCPType type : types )
    {
      final Protocol protocol = type.getProtocol();
      if( protocol instanceof HTTP )
      {
        final HTTP httpProtocol = (HTTP) protocol;
        final URL[] getOnlineResources = httpProtocol.getGetOnlineResources();
        for( final URL url : getOnlineResources )
        {
          if( url != null )
          {
            final String query = url.getQuery();
            if( query != null && query.length() > 0 )
            {
              // The base URL may already contain a query part, we do not want to delete it
              // Quotation from WMS-Spec: "An OGC Web Service shall be prepared to encounter parameters that are not
              // part of this specification."
              final String[] requestParts = query.split( "&" );
              for( final String requestPart : requestParts )
              {
                final String[] queryParts = requestPart.split( "=" );
                if( queryParts.length != 2 )
                  continue;

                wmsParameter.put( queryParts[0], queryParts[1] );
              }
            }
            // the first valid url is enough
            break;
          }

          // the first http-protocol is enough
          break;
        }
      }
    }

    wmsParameter.put( "SERVICE", "WMS" );
    wmsParameter.put( "VERSION", capabilities.getVersion() );
    wmsParameter.put( "REQUEST", operationName );
    wmsParameter.put( "EXCEPTIONS", "application/vnd.ogc.se_xml" );

    return wmsParameter;
  }

}