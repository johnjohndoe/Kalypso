/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 *
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 *
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 *
 * and
 *
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 *
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 * Contact:
 *
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 *
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ogc.gml;

import java.awt.Graphics;
import java.net.URL;
import java.util.logging.Logger;

import javax.media.jai.TiledImage;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.template.types.ObjectFactory;
import org.kalypso.template.types.StyledLayerType;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;
import org.kalypsodeegree_impl.tools.TransformationUtilities;

/**
 * KalypsoPictureTheme
 * <p>
 * created by
 * 
 * @author kuepfer (20.05.2005)
 */
abstract public class KalypsoPictureTheme extends AbstractKalypsoTheme
{
  // TODO: use tracing instead
  private static final Logger LOGGER = Logger.getLogger( KalypsoPictureTheme.class.getName() );

  public static IKalypsoTheme getPictureTheme( final StyledLayerType layerType, final URL context, final IMapModell modell, final String legendGraphic, final boolean shouldShowChildren ) throws Exception
  {
    final String[] arrWorldTypes = new String[] { "tif", "jpg", "png", "gif" };
    final String system = KalypsoCorePlugin.getDefault().getCoordinatesSystem();
    if( ArrayUtils.contains( arrWorldTypes, layerType.getLinktype().toLowerCase() ) )
      return new KalypsoPictureThemeWorldFile( layerType, context, modell, system, legendGraphic, shouldShowChildren );
    else if( "gmlpic".equals( layerType.getLinktype().toLowerCase() ) )
      return new KalypsoPictureThemeGml( layerType, context, modell, legendGraphic, shouldShowChildren );

    throw new IllegalStateException( "not supported layerType: " + layerType.getLinktype() );
  }

  private TiledImage m_image = null;

  private RectifiedGridDomain m_domain;

  private final StyledLayerType m_layerType;

  private final URL m_context;

  private final IMapModell m_modell;

  public KalypsoPictureTheme( final StyledLayerType layerType, final URL context, final IMapModell modell, final String legendIcon, final boolean shouldShowChildren ) throws Exception
  {
    super( layerType.getName(), layerType.getLinktype(), modell, legendIcon, context, shouldShowChildren );

    m_layerType = layerType;
    m_context = context;
    m_modell = modell;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#dispose()
   */
  @Override
  public void dispose( )
  {
    if( m_image != null )
    {
      m_image.dispose();
      m_image = null;
    }

    super.dispose();
  }

  public void fillLayerType( final StyledLayerType layer, final String id, final boolean visible )
  {
    final ObjectFactory extentFac = new ObjectFactory();

    layer.setName( m_layerType.getName() );
    layer.setFeaturePath( "" );

    layer.setVisible( visible );
    layer.setId( id );
    layer.setHref( m_layerType.getHref() );
    layer.setLinktype( m_layerType.getLinktype() );
    layer.setActuate( "onRequest" );
    layer.setType( "simple" );
    layer.setLegendicon( extentFac.createStyledLayerTypeLegendicon( getLegendIcon() ) );
    layer.setShowChildren( extentFac.createStyledLayerTypeShowChildren( shouldShowChildren() ) );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getBoundingBox()
   */
  public GM_Envelope getFullExtent( )
  {
    final GM_Envelope bbox = null;
    try
    {
      return m_domain.getGM_Envelope( m_domain.getCoordinateSystem() );
    }
    catch( final Exception e2 )
    {
      e2.printStackTrace();
      KalypsoPictureTheme.LOGGER.warning( "Transformation of bbox for full extend failed" );
    }
    return bbox;
  }

  protected TiledImage getImage( )
  {
    return m_image;
  }

  protected RectifiedGridDomain getRectifiedGridDomain( )
  {
    return m_domain;
  }

  protected StyledLayerType getStyledLayerType( )
  {
    return m_layerType;
  }

  protected URL getURLContext( )
  {
    return m_context;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, double,
   *      org.kalypsodeegree.model.geometry.GM_Envelope, boolean, org.eclipse.core.runtime.IProgressMonitor)
   */
  public void paint( final Graphics g, final GeoTransform p, final double scale, final GM_Envelope bbox, final boolean selected, final IProgressMonitor monitor )
  {
    if( selected )
      return;

    try
    {
      final GM_Envelope envelope = m_domain.getGM_Envelope( m_domain.getCoordinateSystem() );
      final String crs = m_domain.getCoordinateSystem();
      // transform from crs to crs? optimisation possible?
      TransformationUtilities.transformImage( m_image, envelope, crs, crs, p, g );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

  }

  protected void setImage( final TiledImage image )
  {
    m_image = image;
  }

  protected void setRectifiedGridDomain( final RectifiedGridDomain domain )
  {
    m_domain = domain;
  }
}