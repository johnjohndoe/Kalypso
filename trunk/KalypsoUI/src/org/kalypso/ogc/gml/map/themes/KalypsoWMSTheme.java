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
import java.net.URL;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Font;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.core.runtime.jobs.MutexRule;
import org.kalypso.contribs.eclipse.jface.viewers.ITooltipProvider;
import org.kalypso.ogc.gml.AbstractKalypsoTheme;
import org.kalypso.ogc.gml.IGetFeatureInfoResultProcessor;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.wms.loader.images.KalypsoImageLoader;
import org.kalypso.ogc.gml.wms.provider.images.IKalypsoImageProvider;
import org.kalypso.ogc.gml.wms.provider.legends.IKalypsoLegendProvider;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.Debug;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * This class implements the a theme, which loads images from a given provider.
 * 
 * @author Doemming, Kuepferle
 * @author Holger Albert
 */
public class KalypsoWMSTheme extends AbstractKalypsoTheme implements ITooltipProvider
{
  /**
   * This rule lets only one job run at a time.
   */
  private static ISchedulingRule m_jobMutexRule = new MutexRule();

  /**
   * This variable stores the buffered image.
   */
  private Image m_buffer;

  /**
   * This variable stores the legend, if any.
   */
  private org.eclipse.swt.graphics.Image m_legend;

  /**
   * This variable stores the max envelope of layer on WMS (local SRS).
   */
  protected GM_Envelope m_maxEnvLocalSRS;

  /**
   * This variable stores the image provider.
   */
  protected IKalypsoImageProvider m_provider;

  /**
   * This variable stores the loader for loading the images.
   */
  protected KalypsoImageLoader m_loader;

  /**
   * True, if the imageloader has finished.
   */
  protected boolean m_isFinished;

  /**
   * This variable stores a listener, which will be notified about job events.
   */
  private JobChangeAdapter m_adapter = new JobChangeAdapter()
  {
    /**
     * @see org.eclipse.core.runtime.jobs.JobChangeAdapter#aboutToRun(org.eclipse.core.runtime.jobs.IJobChangeEvent)
     */
    @Override
    public void aboutToRun( IJobChangeEvent event )
    {
      /* Set a status for the user. */
      setStatus( StatusUtilities.createInfoStatus( "Lade ..." ) );
    }

    /**
     * @see org.eclipse.core.runtime.jobs.JobChangeAdapter#done(org.eclipse.core.runtime.jobs.IJobChangeEvent)
     */
    @Override
    public void done( IJobChangeEvent event )
    {
      /* Get the result. */
      IStatus result = event.getResult();

      /* Set a status for the user. */
      setStatus( result );

      /* If ok, set the image. On error, set the image to null. */
      if( !result.matches( IStatus.WARNING | IStatus.ERROR | IStatus.CANCEL ) )
      {
        /* Debug-Information. */
        Debug.WMS.printf( "WMS (" + getName() + "): Setting new image with extent -> " + m_extent + " ... \n" );

        /* Set the newly loaded image. */
        setImage( m_loader.getBuffer() );
      }
      else
      {
        /* Reset the image. */
        setImage( null );

        /* Deactivate the theme. */
        setVisible( false );
      }

      /* Finished loading. */
      m_isFinished = true;
    }
  };

  /**
   * This is the stored extent from the last time, a loader was started (call to
   * {@link #setExtent(int, int, GM_Envelope)}).
   */
  protected GM_Envelope m_extent;

  /**
   * The source string. Do not remove this, because it is needed, when the template is saved.
   */
  private String m_source;

  /**
   * The constructor.
   * 
   * @param linktype
   *            The link type.
   * @param themeName
   *            The name of the theme.
   * @param imageProvider
   *            The image provider, which should be used. If it has also the type {@link IKalypsoLegendProvider} also a
   *            legend can be shown.
   * @param mapModel
   *            The map modell.
   */
  public KalypsoWMSTheme( String source, String linktype, String themeName, IKalypsoImageProvider imageProvider, IMapModell mapModel, String legendIcon, URL context, boolean shouldShowChildren )
  {
    super( themeName, linktype.toUpperCase(), mapModel, legendIcon, context, shouldShowChildren );

    m_source = source;
    m_provider = imageProvider;

    m_buffer = null;
    m_legend = null;
    m_loader = null;
    m_isFinished = false;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getFullExtent()
   */
  public GM_Envelope getFullExtent( )
  {
    if( m_maxEnvLocalSRS == null )
      m_maxEnvLocalSRS = m_provider.getFullExtent();

    return m_maxEnvLocalSRS;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, double,
   *      org.kalypsodeegree.model.geometry.GM_Envelope, boolean, org.eclipse.core.runtime.IProgressMonitor)
   */
  public void paint( Graphics g, GeoTransform world2screen, double scale, GM_Envelope bbox, boolean selected, IProgressMonitor monitor )
  {
    /* The image can not be selected. */
    if( selected )
      return;

    if( m_buffer != null )
      g.drawImage( m_buffer, 0, 0, null );
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#dispose()
   */
  @Override
  public void dispose( )
  {
    /* Dispose the buffered image. */
    if( m_buffer != null )
    {
      m_buffer.flush();
      m_buffer = null;
    }

    /* Dispose the legend. */
    if( m_legend != null )
    {
      m_legend.dispose();
      m_legend = null;
    }

    super.dispose();
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#isLoaded()
   */
  @Override
  public boolean isLoaded( )
  {
    return m_isFinished;
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#setExtent(int, int, org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  @Override
  public void setExtent( int width, int height, GM_Envelope extent )
  {
    /* If the theme is not visible, do not restart the loading. */
    if( isVisible() == false )
    {
      /* Debug-Information. */
      Debug.WMS.printf( "WMS (" + getName() + "): Theme is not visible ... \n" );

      /* Set to finished, else this theme never 'isLoaded'. */
      m_isFinished = true;
      return;
    }

    /* Debug-Information. */
    Debug.WMS.printf( "WMS (" + getName() + "): Got new extent ... \n" );

    /* If it is the same extent than the last time, do not load again. */
    if( m_extent != null && m_extent.equals( extent ) )
      return;

    /* Debug-Information. */
    Debug.WMS.printf( "WMS (" + getName() + "): Different extent means, reload ... \n" );

    /* If there was a loader working, cancel it. */
    if( m_loader != null )
    {
      /* Debug-Information. */
      Debug.WMS.printf( "WMS (" + getName() + "): Cancel old loader ... \n" );

      m_loader.removeJobChangeListener( m_adapter );
      m_loader.dispose();
      m_loader = null;
    }

    /* Reset the buffer. */
    if( m_buffer != null )
    {
      /* Debug-Information. */
      Debug.WMS.printf( "WMS (" + getName() + "): Clear the buffer ... \n" );

      m_buffer.flush();
      m_buffer = null;
    }

    /* Create a new loader. */
    if( width > 0 && height > 0 && extent != null && extent.getWidth() > 0 && extent.getHeight() > 0 )
    {
      /* Debug-Information. */
      Debug.WMS.printf( "WMS (" + getName() + "): Creating a new loader ... \n" );

      /* Create a new loader. */
      m_loader = new KalypsoImageLoader( getName(), m_provider, width, height, extent );

      /* Make sure, only one job is running at a time. */
      m_loader.setRule( m_jobMutexRule );

      /* The adapter makes sure, the loaded image is set. */
      m_loader.addJobChangeListener( m_adapter );

      /* Schedule it. */
      m_loader.schedule( 250 );

      /* Debug-Information. */
      Debug.WMS.printf( "WMS (" + getName() + "): Extent -> " + extent + " ... \n" );

      /* Memorize the extent, so it can be compared the next time, this method is called. */
      m_extent = extent;
    }
    else
    {
      /* Extent is null. */
      m_extent = null;

      /* Set to true, else this theme never 'isLoaded'. */
      m_isFinished = true;
    }
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#getLegendGraphic(org.eclipse.swt.graphics.Font)
   */
  @Override
  public org.eclipse.swt.graphics.Image getLegendGraphic( Font font ) throws CoreException
  {
    if( m_provider == null )
      return super.getLegendGraphic( font );

    if( !(m_provider instanceof IKalypsoLegendProvider) )
      return super.getLegendGraphic( font );

    if( m_legend == null )
    {
      IKalypsoLegendProvider legendProvider = (IKalypsoLegendProvider) m_provider;
      m_legend = legendProvider.getLegendGraphic( font );
    }

    if( m_legend != null )
      return m_legend;

    return super.getLegendGraphic( font );
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.viewers.ITooltipProvider#getTooltip(java.lang.Object)
   */
  public String getTooltip( Object element )
  {
    Assert.isTrue( element == this, "'Element' must be this" );

    if( getStatus().isOK() )
      return m_provider.getLabel();

    return getStatus().getMessage();
  }

  /**
   * This function sets the newly loaded image.
   * 
   * @param image
   *            The newly loaded image.
   */
  protected synchronized void setImage( Image image )
  {
    /* Reset buffer. */
    if( m_buffer != null )
      m_buffer.flush();

    /* Set the new values. */
    m_buffer = image;

    /* If an image is missing, there can be no extent. */
    if( image == null )
    {
      m_extent = null;
      invalidate( null );
      return;
    }

    /* Inform to paint new image. */
    invalidate( getFullExtent() );
  }

  /**
   * This function returns the image provider of this theme.
   * 
   * @return The image provider of this theme.
   */
  public IKalypsoImageProvider getImageProvider( )
  {
    return m_provider;
  }

  /**
   * This function currently does nothing, because the info functionality of themes has to be refactored completely.
   * 
   * @param pointOfInterest
   * @param format
   * @param getFeatureInfoResultProcessor
   * @throws Exception
   */
  @SuppressWarnings("unused")
  public void performGetFeatureinfoRequest( Point pointOfInterest, String format, IGetFeatureInfoResultProcessor getFeatureInfoResultProcessor ) throws Exception
  {
// KalypsoRemoteWMService remoteWMS = m_remoteWMS;
// if( remoteWMS == null )
// return;
//
// // check if nothing to request
// if( m_maxEnvLocalSRS == null )
// return;
//
// String id = "KalypsoWMSGetFeatureInfoRequest" + getName() + new Date().getTime();
//
// HashMap<String, String> parameterMap = remoteWMS.createGetFeatureinfoRequest( pointOfInterest, format );
// parameterMap.put( "ID", id );
//
// // TODO: the WMSFeatureInfoRequest does not support Base URLs with query part. Fix this.
// GetFeatureInfo getInfo = GetFeatureInfo.create( parameterMap );
//
// Object responseEvent = m_remoteWMS.doService( getInfo );
// if( responseEvent == null )
// return;
//
// if( !(responseEvent instanceof GetFeatureInfoResult) )
// return;
//
// try
// {
// GetFeatureInfoResult featureInfoResponse = (GetFeatureInfoResult) responseEvent;
// StringBuffer result = new StringBuffer();
// String featureInfo = featureInfoResponse.getFeatureInfo();
// if( featureInfo != null )
// {
// // String xsl="";
// //
// // XMLHelper.xslTransform(new InputSource(featureInfo), null);
// result.append( featureInfo );
// }
// else
// result.append( " keine oder fehlerhafte Antwort vom Server" );
// OGCWebServiceException exception = featureInfoResponse.getException();
// result.append( "\n\nFehlerMeldung: " );
// if( exception != null )
// result.append( "\n" + exception.toString() );
// else
// result.append( "keine" );
// getFeatureInfoResultProcessor.write( result.toString() );
// System.out.println( featureInfo );
// }
// catch( Exception e )
// {
// GetFeatureInfoResult featureInfoResponse = (GetFeatureInfoResult) responseEvent;
// OGCWebServiceException exception = featureInfoResponse.getException();
// if( exception != null )
// System.out.println( "OGC_WMS_Exception:\n" + exception.toString() );
// }

    // This thing is disabled !!!
    getFeatureInfoResultProcessor.write( "FIX ME" );
  }

  public String getSource( )
  {
    return m_source;
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#getDefaultIcon()
   */
  @Override
  protected ImageDescriptor getDefaultIcon( )
  {
    return KalypsoGisPlugin.getImageProvider().getImageDescriptor( ImageProvider.DESCRIPTORS.IMAGE_THEME_WMS );
  }
}