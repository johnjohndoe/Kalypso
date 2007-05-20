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
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Properties;

import javax.media.jai.PlanarImage;
import javax.media.jai.TiledImage;

import org.deegree.services.OGCWebServiceClient;
import org.deegree.services.OGCWebServiceEvent;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.OGCWebServiceResponse;
import org.deegree.services.wms.capabilities.Format;
import org.deegree.services.wms.capabilities.Operation;
import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.deegree.services.wms.protocol.WMSFeatureInfoRequest;
import org.deegree.services.wms.protocol.WMSFeatureInfoResponse;
import org.deegree.services.wms.protocol.WMSGetMapRequest;
import org.deegree.services.wms.protocol.WMSGetMapResponse;
import org.deegree_impl.services.OGCWebServiceEvent_Impl;
import org.deegree_impl.services.wms.protocol.WMSProtocolFactory;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.kalypso.commons.java.util.PropertiesHelper;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.core.runtime.jobs.MutexRule;
import org.kalypso.contribs.eclipse.jface.viewers.ITooltipProvider;
import org.kalypso.contribs.java.xml.XMLHelper;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.ogc.gml.AbstractKalypsoTheme;
import org.kalypso.ogc.gml.IGetFeatureInfoResultProcessor;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.ct.GeoTransformer;
import org.kalypsodeegree_impl.tools.WMSHelper;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Document;

import com.sun.xml.stream.writers.WriterUtility;

/**
 * @author Doemming, Kuepferle
 */
public class KalypsoWMSTheme extends AbstractKalypsoTheme implements ITooltipProvider
{
  public final static String KEY_LAYERS = "LAYERS";

  public final static String KEY_URL = "URL";

  public final static String KEY_STYLES = "STYLES";

  /** type name */
  public final static String TYPE_NAME = "wms";

  protected final static ISchedulingRule m_jobMutexRule = new MutexRule();

  /** source key */
  private final String m_source;

  private final Job m_initJob;

  private final IJobChangeListener m_jobListener = new JobChangeAdapter()
  {
    /**
     * @see org.eclipse.core.runtime.jobs.JobChangeAdapter#done(org.eclipse.core.runtime.jobs.IJobChangeEvent)
     */
    @Override
    public void done( final IJobChangeEvent event )
    {
      handleJobDone( event.getResult() );
    }
  };

  /** remote WMS */
  private KalypsoRemoteWMService m_remoteWMS;

  /** the local CS */
  private final CS_CoordinateSystem m_localSRS;

  /** max envelope of layer on WMS (local CS) */
  private GM_Envelope m_maxEnvLocalSRS = null;

  /** bbox that is requested by last paint call */
  protected GM_Envelope m_requestedEnvLocalSRS = null;

  /** buffered image */
  private Image m_buffer = null;

  /** envelope of buffered image (local SRS) */
  private GM_Envelope m_bufferEnvLocalSRS = null;

  /** temporary locked request bbox to aviod multi requests on same bbox */
  private GM_Envelope m_lockRequestEnvLocalSRS = null;

  private int m_lastWidth;

  private int m_lastHeight;

  private final URL m_serviceURL;

  public KalypsoWMSTheme( final String linktype, final String themeName, final String source, final CS_CoordinateSystem localSRS, final IMapModell mapModel )
  {
    super( themeName, linktype.toUpperCase(), mapModel );
    final Properties sourceProps = PropertiesHelper.parseFromString( source, '#' );
    m_localSRS = localSRS;
    m_source = source;

    final String layers = sourceProps.getProperty( KEY_LAYERS, null );
    final String styles = sourceProps.getProperty( KEY_STYLES, null );
    final String service = sourceProps.getProperty( KEY_URL, null );

    final URL serviceURL = parseServiceUrl( service );

    m_serviceURL = serviceURL;

    m_initJob = new Job( "Capabilities laden" )
    {
      @Override
      protected IStatus run( final IProgressMonitor monitor )
      {
        handleJobDone( StatusUtilities.createInfoStatus( "lade Capabilities..." ) );
        setWms( null, null );

        try
        {
          final KalypsoRemoteWMService wms = KalypsoRemoteWMService.initializeService( serviceURL, localSRS, layers, styles, monitor );
          final GM_Envelope maxExtent = wms.getMaxExtend( localSRS );
          setWms( wms, maxExtent );

          return Status.OK_STATUS;
        }
        catch( final CoreException e )
        {
          return e.getStatus();
        }
      }
    };

    /* Try to load capabilities immediately */
    if( m_serviceURL != null )
    {
      m_initJob.addJobChangeListener( m_jobListener );
      m_initJob.setRule( m_jobMutexRule );
      m_initJob.schedule();
    }
  }

  private URL parseServiceUrl( final String service )
  {
    try
    {
      return new URL( service );
    }
    catch( final MalformedURLException e )
    {
      final String message = String.format( "Service URL fehlerhaft: %s (%s)", service, e.getLocalizedMessage() );
      setStatus( StatusUtilities.statusFromThrowable( e, message ) );
    }

    return null;
  }

  protected void setWms( final KalypsoRemoteWMService wms, final GM_Envelope maxExtent )
  {
    /* Prepare for exceptions */
    m_remoteWMS = wms;
    m_maxEnvLocalSRS = maxExtent;

    // TODO: check
    if( m_buffer != null )
      m_buffer.flush();
    m_buffer = null;
    m_bufferEnvLocalSRS = null;
    m_lockRequestEnvLocalSRS = null;

    /* Force repaint */
    fireModellEvent( null );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, double,
   *      org.kalypsodeegree.model.geometry.GM_Envelope, boolean)
   */
  public void paint( final Graphics g, final GeoTransform geoTransform, final double scale, final GM_Envelope bbox, final boolean selected )
  {
    if( selected ) // image can not be selected
      return;

    /* If the service url was bad, we can do nothing at all */
    if( m_serviceURL == null )
      return;

    /* If wms could not connect but we have a valid url, reschedule getting the capabilites. */
    if( m_remoteWMS == null && m_initJob.getState() != Job.RUNNING )
      m_initJob.schedule();

    m_requestedEnvLocalSRS = bbox;
    m_lastWidth = (int) g.getClip().getBounds().getWidth();
    m_lastHeight = (int) g.getClip().getBounds().getHeight();

    if( m_buffer != null && m_bufferEnvLocalSRS.equals( m_requestedEnvLocalSRS ) )
      g.drawImage( m_buffer, 0, 0, null );
    else
    {
      try
      {
        updateImage( geoTransform, m_requestedEnvLocalSRS );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        final IStatus status = StatusUtilities.statusFromThrowable( e, "Fehler beim Zeichnen" );
        handleJobDone( status );
        // simply do not paint it
      }
    }
  }

  private void updateImage( final GeoTransform geoTransformToLocalSRS, final GM_Envelope envRequestLocalSRS ) throws Exception
  {
    // check if nothing to request
    if( envRequestLocalSRS == null )
      return;

    final KalypsoRemoteWMService remoteWMS = m_remoteWMS;
    /* Work locally against a copy of the reference, because it may change any time... */
    if( remoteWMS == null )
      return;

    // check if bbox is locked for request
    if( m_lockRequestEnvLocalSRS != null && m_lockRequestEnvLocalSRS.equals( envRequestLocalSRS ) )
      return;

    // lock it now
    final GM_Envelope targetEnvLocalSRS = (GM_Envelope) envRequestLocalSRS.clone();
    m_lockRequestEnvLocalSRS = targetEnvLocalSRS;

    final String id = "KalypsoWMSRequest" + getName() + new Date().getTime();

    final HashMap<String, String> parameterMap = remoteWMS.createGetMapRequestParameter( m_lastWidth, m_lastHeight, m_requestedEnvLocalSRS, m_localSRS );
    final WMSGetMapRequest request = WMSProtocolFactory.createGetMapRequest( id, parameterMap );

    final int width = m_lastWidth;
    final int height = m_lastHeight;
    final IJobChangeListener jobListener = m_jobListener;
    final OGCWebServiceClient client = new OGCWebServiceClient()
    {
      public void write( final Object responseEvent )
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
        final Job renderJob = new Job( "Loading map from WMS " + getName() )
        {
          /**
           * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
           */
          @Override
          protected IStatus run( final IProgressMonitor monitor )
          {
            handleJobDone( StatusUtilities.createInfoStatus( "lade Karte..." ) );
            try
            {
              final WMSGetMapResponse mapResponse = (WMSGetMapResponse) response;

              final RenderedImage resultImage = (RenderedImage) mapResponse.getMap();
              if( resultImage == null )
              {
                final OGCWebServiceRequest mapRequest = mapResponse.getRequest();
                final Document exception = mapResponse.getException();
                final StringWriter stringWriter = new StringWriter();
                XMLHelper.writeDOM( exception, WriterUtility.DEFAULT_ENCODING, stringWriter );

                final MultiStatus status = new MultiStatus( KalypsoCorePlugin.getID(), 0, "Zugriffsfehler", null );
                status.add( StatusUtilities.createErrorStatus( "Request: '" + mapRequest + "'" ) );
                status.add( StatusUtilities.createErrorStatus( "Exception-Dokument: " ) );
                status.add( StatusUtilities.createMultiStatusFromMessage( IStatus.ERROR, KalypsoCorePlugin.getID(), 0, stringWriter.toString(), "\n", null ) );

                return status;
              }

              final PlanarImage remoteImage = PlanarImage.wrapRenderedImage( resultImage );
              setImage( new TiledImage( remoteImage, true ), targetEnvLocalSRS, width, height, geoTransformToLocalSRS );
              return Status.OK_STATUS;
            }
            catch( final Throwable e )
            {
              return StatusUtilities.statusFromThrowable( e, "Zugriffsfehler" );
            }
          }
        };

        renderJob.setRule( m_jobMutexRule );
        renderJob.addJobChangeListener( jobListener );
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

    remoteWMS.doService( ogcWSEvent );
  }

  protected synchronized void setImage( final TiledImage image, final GM_Envelope targetEnvLocalSRS, final int width, final int height, final GeoTransform geoTransform ) throws Exception
  {
    try
    {
      final KalypsoRemoteWMService remoteWms = m_remoteWMS;
      if( m_remoteWMS == null )
        return;

      final CS_CoordinateSystem remoteSRS = remoteWms.getSRS();

      final Image buffer = new BufferedImage( width, height, BufferedImage.TYPE_INT_ARGB );
      final GeoTransformer geoTransformerToRemoteSRS = new GeoTransformer( remoteSRS );
      final GM_Envelope remoteEnv = geoTransformerToRemoteSRS.transformEnvelope( targetEnvLocalSRS, m_localSRS );
      // paint image on buffer
      WMSHelper.transformImage( image, remoteEnv, m_localSRS, remoteSRS, geoTransform, buffer.getGraphics() );
      if( m_buffer != null )
        m_buffer.flush();
      m_buffer = buffer;
      m_bufferEnvLocalSRS = targetEnvLocalSRS;
    }
    catch( final Exception e )
    {
      m_buffer.flush();
      m_buffer = null;
      m_bufferEnvLocalSRS = null;
      throw e;
    }
    // inform to paint new image
    fireModellEvent( null );
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#isLoaded()
   */
  @Override
  public boolean isLoaded( )
  {
    return m_remoteWMS != null;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#dispose()
   */
  @Override
  public void dispose( )
  {
    if( m_buffer != null )
      m_buffer.flush();
    m_buffer = null;

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
    final KalypsoRemoteWMService remoteWMS = m_remoteWMS;
    if( remoteWMS == null )
      return;

    // check if nothing to request
    if( m_maxEnvLocalSRS == null )
      return;

    final String id = "KalypsoWMSGetFeatureInfoRequest" + getName() + new Date().getTime();

    final HashMap<String, String> parameterMap = remoteWMS.createGetFeatureinfoRequest( pointOfInterest, format );

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
    final WMSCapabilities wmsCaps = m_remoteWMS.getWMSCapabilities();
    final Operation operation = wmsCaps.getCapability().getRequest().getOperation( Operation.GETFEATUREINFO_NAME );
    return operation != null;
  }

  /**
   * @return supported formats
   */
  public synchronized String[] getFeatureInfoRequestFormats( )
  {
    final WMSCapabilities wmsCaps = m_remoteWMS.getWMSCapabilities();
    final List<String> result = new ArrayList<String>();
    final Operation operation = wmsCaps.getCapability().getRequest().getOperation( Operation.GETFEATUREINFO_NAME );
    final Format[] formats = operation.getFormats();
    for( final Format format : formats )
    {
      if( format.getName() != null )
        result.add( format.getName() );
    }
    return result.toArray( new String[result.size()] );
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.viewers.ITooltipProvider#getTooltip(java.lang.Object)
   */
  public String getTooltip( final Object element )
  {
    Assert.isTrue( element == this, "'Element' must be this" );

    if( getStatus().isOK() )
      return "WMS Thema: " + m_serviceURL;

    return getStatus().getMessage();
  }

  /**
   * If any of the jobs finishes, set my status to its result value.
   */
  protected void handleJobDone( final IStatus status )
  {
    setStatus( status );

    if( status.matches( IStatus.WARNING | IStatus.ERROR | IStatus.CANCEL ) )
    {
      /* Reset wms access on error in order to try reload */
      setWms( null, null );

      /* Deaktivate on error. */
      // TODO: make this optional?
      getMapModell().enableTheme( this, false );
    }
  }
}
