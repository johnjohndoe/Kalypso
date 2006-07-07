/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.map.themes;

import java.awt.Graphics;
import java.awt.Image;
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;
import java.io.StringWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.Set;

import javax.media.jai.PlanarImage;
import javax.media.jai.TiledImage;

import org.deegree.services.OGCWebServiceClient;
import org.deegree.services.OGCWebServiceEvent;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.OGCWebServiceResponse;
import org.deegree.services.capabilities.DCPType;
import org.deegree.services.capabilities.HTTP;
import org.deegree.services.capabilities.Protocol;
import org.deegree.services.wms.capabilities.Format;
import org.deegree.services.wms.capabilities.Operation;
import org.deegree.services.wms.capabilities.Request;
import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.deegree.services.wms.protocol.WMSFeatureInfoRequest;
import org.deegree.services.wms.protocol.WMSFeatureInfoResponse;
import org.deegree.services.wms.protocol.WMSGetMapRequest;
import org.deegree.services.wms.protocol.WMSGetMapResponse;
import org.deegree_impl.services.OGCWebServiceEvent_Impl;
import org.deegree_impl.services.wms.RemoteWMService;
import org.deegree_impl.services.wms.protocol.WMSProtocolFactory;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.commons.java.util.PropertiesHelper;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.core.runtime.jobs.MutexRule;
import org.kalypso.contribs.java.xml.XMLHelper;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.ogc.gml.AbstractKalypsoTheme;
import org.kalypso.ogc.gml.IGetFeatureInfoResultProcessor;
import org.kalypso.ogc.gml.wms.WMSCapabilitiesHelper;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.ct.GeoTransformer;
import org.kalypsodeegree_impl.tools.WMSHelper;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Document;

import com.sun.xml.stream.writers.WriterUtility;

/**
 * @author Doemming Kuepferle
 */
public class KalypsoWMSTheme extends AbstractKalypsoTheme
{
  public final static String KEY_LAYERS = "LAYERS";

  public final static String KEY_URL = "URL";

  public static final String KEY_STYLES = "STYLES";

  protected final static ISchedulingRule m_jobMutexRule = new MutexRule();

  /** layerlist to fetch from WMS */
  private final String m_layers;

  /** styleList to fetch from WMS */
  private final String m_styles;

  /** remote WMS */
  private RemoteWMService m_remoteWMS;

  /** source key */
  private final String m_source;

  /** bbox that is requested by last paint call */
  protected GM_Envelope m_requestedEnvLocalSRS = null;

  /** the local CS */
  private final CS_CoordinateSystem m_localSRS;

  /** the remote CS, may be different from local */
  private CS_CoordinateSystem m_remoteSRS = null;

  /** max envelope of layer on WMS (local CS) */
  private GM_Envelope m_maxEnvLocalSRS = null;

  /** capabilities from WMS */
  private WMSCapabilities m_wmsCaps;

  /** buffered image */
  private Image m_buffer = null;

  /** envelope of buffered image (local SRS) */
  private GM_Envelope m_bufferEnvLocalSRS = null;

  /** temporary locked request bbox to aviod multi requests on same bbox */
  private GM_Envelope m_lockRequestEnvLocalSRS = null;

  /** type name */

  public static final String TYPE_NAME = "wms";

  private int m_lastWidth;

  private int m_lastHeight;

  private boolean m_isInitialized = false;

  private boolean m_isInvalid = false;

  private final String m_service;

  public KalypsoWMSTheme( final String linktype, final String themeName, final String source, final CS_CoordinateSystem localSRS )
  {
    super( themeName, linktype.toUpperCase() );
    final Properties sourceProps = PropertiesHelper.parseFromString( source, '#' );
    m_layers = sourceProps.getProperty( KEY_LAYERS, null );
    m_styles = sourceProps.getProperty( KEY_STYLES, null );
    m_service = sourceProps.getProperty( KEY_URL, null );
    m_localSRS = localSRS;
    m_source = source;
  }

  private void init( )
  {
    try
    {
      m_wmsCaps = WMSCapabilitiesHelper.loadCapabilities( new URL( m_service ), new NullProgressMonitor() );
      m_remoteWMS = new RemoteWMService( m_wmsCaps );

      // match the local with the remote coordiante system
      final CS_CoordinateSystem[] crs = WMSHelper.negotiateCRS( m_localSRS, m_wmsCaps, m_layers.split( "," ) );
      if( !crs[0].equals( m_localSRS ) )
        m_remoteSRS = crs[0];
      else
        m_remoteSRS = m_localSRS;
      // set max extent for Map Layer

      // set max envelope
      try
      {
        final GM_Envelope maxEnvRemoteSRS = WMSHelper.getMaxExtend( m_layers.split( "," ), m_wmsCaps, m_remoteSRS );
        final GeoTransformer gt = new GeoTransformer( m_localSRS );
        m_maxEnvLocalSRS = gt.transformEnvelope( maxEnvRemoteSRS, m_remoteSRS );
      }
      catch( Exception e )
      {
        // hack, when the WMS serves unparseble boundingboxes
        e.printStackTrace();
        m_maxEnvLocalSRS = null;
      }
    }
    catch( final Exception e )
    {
      // nothing to do
      e.printStackTrace();

      m_isInvalid = true;
    }

    m_isInitialized = true;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, double,
   *      org.kalypsodeegree.model.geometry.GM_Envelope, boolean)
   */
  public void paint( final Graphics g, final GeoTransform geoTransform, final double scale, final GM_Envelope bbox, final boolean selected )
  {
    // if invalid, just return
    if( m_isInvalid )
      return;

    if( !m_isInitialized )
      init();

    m_requestedEnvLocalSRS = bbox;
    m_lastWidth = (int) g.getClip().getBounds().getWidth();
    m_lastHeight = (int) g.getClip().getBounds().getHeight();
    if( selected ) // image can not be selected
      return;

    if( m_buffer != null && m_bufferEnvLocalSRS.equals( m_requestedEnvLocalSRS ) )
      g.drawImage( m_buffer, 0, 0, null );
    else
    {
      try
      {
        updateImage( geoTransform, m_requestedEnvLocalSRS );
      }
      catch( Exception e )
      {
        // simply do not paint it
      }
    }
  }

  private void updateImage( final GeoTransform geoTransformToLocalSRS, GM_Envelope envRequestLocalSRS ) throws Exception
  {
    if( m_wmsCaps == null )
      return;
    // check if nothing to request
    if( envRequestLocalSRS == null )
      return;

    // check if bbox is locked for request
    if( m_lockRequestEnvLocalSRS != null && m_lockRequestEnvLocalSRS.equals( envRequestLocalSRS ) )
      return;
    // lock it now
    final GM_Envelope targetEnvLocalSRS = (GM_Envelope) envRequestLocalSRS.clone();
    m_lockRequestEnvLocalSRS = targetEnvLocalSRS;

    final String id = "KalypsoWMSRequest" + getName() + Long.toString( (new Date()).getTime() );
    final HashMap<String, String> parameterMap = createGetMapRequestParameter();

    final WMSGetMapRequest request = WMSProtocolFactory.createGetMapRequest( id, parameterMap );

    final int width = m_lastWidth;
    final int height = m_lastHeight;
    final OGCWebServiceClient client = new OGCWebServiceClient()
    {
      public void write( Object responseEvent )
      {
        if( isObsolete( targetEnvLocalSRS ) )
          return;
        if( !(responseEvent instanceof OGCWebServiceEvent) )
          return;
        final OGCWebServiceResponse response = ((OGCWebServiceEvent) responseEvent).getResponse();
        if( !(response instanceof WMSGetMapResponse) )
          return;
        if( isObsolete( targetEnvLocalSRS ) )
          return;
        final Job renderJob = new Job( "loading map from WMS " + getName() )
        {
          /**
           * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
           */
          @Override
          protected IStatus run( IProgressMonitor monitor )
          {
            try
            {
              final WMSGetMapResponse mapResponse = (WMSGetMapResponse) response;

              final RenderedImage resultImage = (RenderedImage) (mapResponse).getMap();
              if( resultImage == null )
              {
                final OGCWebServiceRequest mapRequest = mapResponse.getRequest();
                final Document exception = mapResponse.getException();
                final StringWriter stringWriter = new StringWriter();
                XMLHelper.writeDOM( exception, WriterUtility.DEFAULT_ENCODING, stringWriter );

                final MultiStatus status = new MultiStatus( KalypsoCorePlugin.getID(), 0, "Fehler bei laden vom WMS " + KalypsoWMSTheme.this.getName()
                    + ". Das Thema sollte unsichtbar geschaltet werden.", null );
                status.add( StatusUtilities.createErrorStatus( "Request war: '" + mapRequest + "'" ) );
                status.add( StatusUtilities.createErrorStatus( "Exception-Dokument: " ) );
                status.add( StatusUtilities.createMultiStatusFromMessage( IStatus.ERROR, KalypsoCorePlugin.getID(), 0, stringWriter.toString(), "\n", null ) );

                return status;
              }
              final PlanarImage remoteImage = PlanarImage.wrapRenderedImage( resultImage );
              setImage( new TiledImage( remoteImage, true ), targetEnvLocalSRS, width, height, geoTransformToLocalSRS );
              return Status.OK_STATUS;
            }
            catch( final Exception e )
            {
              return StatusUtilities.statusFromThrowable( e, "Fehler bei laden vom WMS " + KalypsoWMSTheme.this.getName() + "\n das Thema sollte unsichtbar geschaltet werden" );
            }
          }
        };

        renderJob.setRule( m_jobMutexRule );
        renderJob.schedule();
      }

      private boolean isObsolete( GM_Envelope envLocalSRS )
      {
        return m_requestedEnvLocalSRS == null || !(m_requestedEnvLocalSRS.equals( envLocalSRS ));
      }
    };

    final OGCWebServiceEvent ogcWSEvent = new OGCWebServiceEvent_Impl( this, // source
        request, // request
        null, // message
        client );

    m_remoteWMS.doService( ogcWSEvent );
  }

  /**
   * @throws Exception
   */
  private HashMap<String, String> createGetMapRequestParameter( ) throws Exception
  {
    final HashMap<String, String> wmsParameter = new HashMap<String, String>();

    // HACK: add existing query parts from base url
    final Request capaRequest = m_wmsCaps.getCapability().getRequest();
    final Operation operation = capaRequest.getOperation( 0 );
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
              // @belger: is this for UMN-MapServer ?? or why are here allready parameters in the baseURL ?
              // (doemming)
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
    wmsParameter.put( "VERSION", m_wmsCaps.getVersion() );
    wmsParameter.put( "REQUEST", "getMap" );
    wmsParameter.put( "LAYERS", m_layers );
    if( m_styles != null )
      wmsParameter.put( "STYLES", m_styles );

    // some WMS-themes use style name="" and when deegree makes "STYLES=default" out of this, this does not work
    // I think style name="" is also not valid (can we be flexible ?)
    // ask me ( v.doemming@tuhh.de )
    wmsParameter.put( "FORMAT", "image/png" );
    wmsParameter.put( "TRANSPARENT", "TRUE" );
    wmsParameter.put( "EXCEPTIONS", "application/vnd.ogc.se_xml" );
    wmsParameter.put( "WIDTH", "" + m_lastWidth );
    wmsParameter.put( "HEIGHT", "" + m_lastHeight );
    wmsParameter.put( "SRS", m_remoteSRS.getName() );

    // if( m_authentification )
    // {
    // if(m_pass == null || m_user == null )
    // return;
    // final String pw = m_user + ":" + m_pass;
    // final String epw = "Basic " + ( new BASE64Encoder() ).encode(
    // pw.getBytes() );
    //
    // wmsParameter.put("Proxy-Authorization", epw );
    // }

    final GeoTransformer gt = new GeoTransformer( m_remoteSRS );
    final GM_Envelope targetEnvRemoteSRS = gt.transformEnvelope( m_requestedEnvLocalSRS, m_localSRS );
    if( targetEnvRemoteSRS.getMax().getX() - targetEnvRemoteSRS.getMin().getX() <= 0 )
      throw new Exception( "invalid bbox" );
    if( targetEnvRemoteSRS.getMax().getY() - targetEnvRemoteSRS.getMin().getY() <= 0 )
      throw new Exception( "invalid bbox" );
    final String targetEnvRemoteSRSstring = WMSHelper.env2bboxString( targetEnvRemoteSRS );
    wmsParameter.put( "BBOX", targetEnvRemoteSRSstring );
    return wmsParameter;
  }

  /**
   * @param image
   * @param targetEnvLocalSRS
   * @param width
   * @param height
   * @param geoTransform
   * @throws Exception
   */
  protected synchronized void setImage( final TiledImage image, final GM_Envelope targetEnvLocalSRS, final int width, final int height, final GeoTransform geoTransform ) throws Exception
  {
    try
    {
      final Image buffer = new BufferedImage( width, height, BufferedImage.TYPE_INT_ARGB );
      final GeoTransformer geoTransformerToRemoteSRS = new GeoTransformer( m_remoteSRS );
      final GM_Envelope remoteEnv = geoTransformerToRemoteSRS.transformEnvelope( targetEnvLocalSRS, m_localSRS );
      // paint image on buffer
      WMSHelper.transformImage( image, remoteEnv, m_localSRS, m_remoteSRS, geoTransform, buffer.getGraphics() );
      m_buffer = buffer;
      m_bufferEnvLocalSRS = targetEnvLocalSRS;
    }
    catch( Exception e )
    {
      m_buffer = null;
      m_bufferEnvLocalSRS = null;
      throw e;
      // TODO set theme invisible
    }
    // inform to paint new image
    fireModellEvent( null );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#dispose()
   */
  @Override
  public void dispose( )
  {
    if( m_buffer != null )
      m_buffer.flush();

    super.dispose();
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getBoundingBox()
   */
  public GM_Envelope getBoundingBox( )
  {
    return m_maxEnvLocalSRS;
  }

  /**
   * @return source key
   */
  public String getSource( )
  {
    return m_source;
  }

  /**
   * @param pointOfInterest
   * @param format
   * @throws Exception
   */
  public void performGetFeatureinfoRequest( final Point pointOfInterest, final String format, final IGetFeatureInfoResultProcessor getFeatureInfoResultProcessor ) throws Exception
  {
    // check if nothing to request
    if( m_maxEnvLocalSRS == null )
      return;

    final String id = "KalypsoWMSGetFeatureInfoRequest" + getName() + Long.toString( (new Date()).getTime() );
    final HashMap<String, String> parameterMap = createGetMapRequestParameter();
    parameterMap.put( "REQUEST", "GetFeatureInfo" );
    parameterMap.put( "QUERY_LAYERS", m_layers );
    if( format != null && format.length() > 0 )
      parameterMap.put( "INFO_FORMAT", format );
    parameterMap.put( "X", Integer.toString( pointOfInterest.x ) );
    parameterMap.put( "Y", Integer.toString( pointOfInterest.y ) );

    final Set set = parameterMap.keySet();
    System.out.print( "?" );
    for( Iterator iter = set.iterator(); iter.hasNext(); )
    {
      final String key = (String) iter.next();
      final String value = parameterMap.get( key );
      System.out.print( key + "=" + value + "&" );
    }
    System.out.println();
    // TODO: the WMSFeatureInfoRequest does not support Base URLs with query part. Fix this.
    final WMSFeatureInfoRequest getFeatureInfoRequest = WMSProtocolFactory.createGetFeatureInfoRequest( id, parameterMap );
    final OGCWebServiceClient client = new OGCWebServiceClient()
    {
      public void write( Object responseEvent )
      {
        if( !(responseEvent instanceof OGCWebServiceEvent) )
          return;
        final OGCWebServiceResponse response = ((OGCWebServiceEvent) responseEvent).getResponse();
        if( !(response instanceof WMSFeatureInfoResponse) )
          return;
        try
        {
          final WMSFeatureInfoResponse featureInfoResponse = (WMSFeatureInfoResponse) response;
          final StringBuffer result = new StringBuffer();
          final String featureInfo = featureInfoResponse.getFeatureInfo();
          if( featureInfo != null )
          {
            // String xsl="";
            //              
            // XMLHelper.xslTransform(new InputSource(featureInfo), null);
            result.append( featureInfo );
          }
          else
            result.append( " keine oder fehlerhafte Antwort vom Server" );
          final Document exception = featureInfoResponse.getException();
          result.append( "\n\nFehlerMeldung: " );
          if( exception != null )
            result.append( "\n" + XMLHelper.toString( exception ) );
          else
            result.append( "keine" );
          getFeatureInfoResultProcessor.write( result.toString() );
          System.out.println( featureInfo );
        }
        catch( Exception e )
        {
          final Document wmsException = ((WMSFeatureInfoResponse) response).getException();
          if( wmsException != null )
            System.out.println( "OGC_WMS_Exception:\n" + XMLHelper.toString( wmsException ) );
        }
      }
    };

    final OGCWebServiceEvent ogcWSEvent = new OGCWebServiceEvent_Impl( this, // source
        getFeatureInfoRequest, // request
        null, // message
        client );

    m_remoteWMS.doService( ogcWSEvent );
  }

  public boolean isSupportingGetFeatureInfoRequest( )
  {
    final Operation operation = m_wmsCaps.getCapability().getRequest().getOperation( Operation.GETFEATUREINFO_NAME );
    return operation != null;
  }

  /**
   * @return supported formats
   */
  public synchronized String[] getFeatureInfoRequestFormats( )
  {
    final List<String> result = new ArrayList<String>();
    final Operation operation = m_wmsCaps.getCapability().getRequest().getOperation( Operation.GETFEATUREINFO_NAME );
    final Format[] formats = operation.getFormats();
    for( int i = 0; i < formats.length; i++ )
    {
      final Format format = formats[i];
      if( format.getName() != null )
        result.add( format.getName() );
    }
    return result.toArray( new String[result.size()] );
  }
}
