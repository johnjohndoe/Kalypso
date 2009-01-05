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
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.kalypso.commons.i18n.I10nString;
import org.kalypso.contribs.eclipse.jface.viewers.ITooltipProvider;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.AbstractKalypsoTheme;
import org.kalypso.ogc.gml.IGetFeatureInfoResultProcessor;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.wms.loader.images.KalypsoImageLoader;
import org.kalypso.ogc.gml.wms.provider.images.IKalypsoImageProvider;
import org.kalypso.ogc.gml.wms.provider.legends.IKalypsoLegendProvider;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
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
   * This variable stores the legend, if any.
   */
  private org.eclipse.swt.graphics.Image m_legend = null;

  /**
   * This variable stores the max envelope of layer on WMS (local SRS).
   */
  protected GM_Envelope m_maxEnvLocalSRS;

  /**
   * This variable stores the image provider.
   */
  protected IKalypsoImageProvider m_provider;

  /**
   * This is the stored extent from the last time, a loader was started (call to
   * {@link #setExtent(int, int, GM_Envelope)}).
   */
  protected GM_Envelope m_extent;

  /**
   * The source string. Do not remove this, because it is needed, when the template is saved.
   */
  private final String m_source;

  /**
   * The constructor.
   *
   * @param linktype
   *          The link type.
   * @param themeName
   *          The name of the theme.
   * @param imageProvider
   *          The image provider, which should be used. If it has also the type {@link IKalypsoLegendProvider} also a
   *          legend can be shown.
   * @param mapModel
   *          The map modell.
   */
  public KalypsoWMSTheme( final String source, final String linktype, final I10nString themeName, final IKalypsoImageProvider imageProvider, final IMapModell mapModel, final String legendIcon, final URL context, final boolean shouldShowChildren )
  {
    super( themeName, linktype.toUpperCase(), mapModel, legendIcon, context, shouldShowChildren );

    m_source = source;
    m_provider = imageProvider;
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
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, java.lang.Boolean,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  public void paint( final Graphics g, final GeoTransform p, final Boolean selected, final IProgressMonitor monitor ) throws CoreException
  {
    /* The image can not be selected. */
    if( selected != null && selected )
      return;

    final int width = (int) p.getDestWidth();
    final int height = (int) p.getDestHeight();
    final GM_Envelope extent = p.getSourceRect();
    final KalypsoImageLoader loader = new KalypsoImageLoader( getLabel(), m_provider, width, height, extent );
    final IStatus status = loader.run( monitor );
    if( !status.isOK() )
      throw new CoreException( status );

    final Image buffer = loader.getBuffer();
    g.drawImage( buffer, 0, 0, null );
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#dispose()
   */
  @Override
  public void dispose( )
  {
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
    return true;
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#getLegendGraphic(org.eclipse.swt.graphics.Font)
   */
  @Override
  public org.eclipse.swt.graphics.Image getLegendGraphic( final Font font ) throws CoreException
  {
    if( m_provider == null )
      return super.getLegendGraphic( font );

    if( !(m_provider instanceof IKalypsoLegendProvider) )
      return super.getLegendGraphic( font );

    if( m_legend == null )
    {
      final IKalypsoLegendProvider legendProvider = (IKalypsoLegendProvider) m_provider;
      m_legend = legendProvider.getLegendGraphic( font );
    }

    if( m_legend != null )
    {
      // Clone this image, the returned image will be disposed outside!
      return new org.eclipse.swt.graphics.Image( font.getDevice(), m_legend, SWT.IMAGE_COPY );
    }

    return super.getLegendGraphic( font );
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.viewers.ITooltipProvider#getTooltip(java.lang.Object)
   */
  public String getTooltip( final Object element )
  {
    Assert.isTrue( element == this, Messages.getString( "org.kalypso.ogc.gml.map.themes.KalypsoWMSTheme.19" ) ); //$NON-NLS-1$

    if( getStatus().isOK() )
      return m_provider.getLabel();

    return getStatus().getMessage();
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
  public void performGetFeatureinfoRequest( final Point pointOfInterest, final String format, final IGetFeatureInfoResultProcessor getFeatureInfoResultProcessor ) throws Exception
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
    getFeatureInfoResultProcessor.write( Messages.getString( "org.kalypso.ogc.gml.map.themes.KalypsoWMSTheme.21" ) ); //$NON-NLS-1$
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