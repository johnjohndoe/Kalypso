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
package org.kalypso.ogc.gml;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Display;
import org.kalypso.commons.i18n.I10nString;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModellListener;
import org.kalypso.ogc.gml.mapmodel.MapModellAdapter;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory;
import org.kalypsodeegree_impl.graphics.transformation.WorldToScreenTransform;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author doemming
 */
public class KalypsoLegendTheme extends AbstractKalypsoTheme
{
  private final Color backColor = new Color( 240, 240, 240 );

  private final int m_styleHeight = 50;

  private final Font m_font = new Font( "SansSerif", Font.BOLD, m_styleHeight / 5 );

  private final IMapModellListener m_modellListener = new MapModellAdapter()
  {
    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeAdded(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme)
     */
    @Override
    public void themeAdded( IMapModell source, IKalypsoTheme theme )
    {
      invalidateLegend();
    }

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeRemoved(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme)
     */
    @Override
    public void themeRemoved( IMapModell source, IKalypsoTheme theme, boolean lastVisibility )
    {
      invalidateLegend();
    }

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeOrderChanged(org.kalypso.ogc.gml.mapmodel.IMapModell)
     */
    @Override
    public void themeOrderChanged( IMapModell source )
    {
      invalidateLegend();
    }

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeVisibilityChanged(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme, boolean)
     */
    @Override
    public void themeVisibilityChanged( IMapModell source, IKalypsoTheme theme, boolean visibility )
    {
      invalidateLegend();
    }
  };

  private Image m_image = null;

  /**
   * Holds the descriptor for the default icon of this theme. Is used in legends, such as the outline.
   */
  private org.eclipse.swt.graphics.Image m_legendThemeIcon;

  public KalypsoLegendTheme( final I10nString name, final IMapModell mapModell, final String legendIcon, final URL context, final boolean shouldShowChildren )
  {
    super( name, "legend", mapModell, legendIcon, context, shouldShowChildren );

    m_legendThemeIcon = null;

    mapModell.addMapModelListener( m_modellListener );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#dispose()
   */
  @Override
  public void dispose( )
  {
    getMapModell().removeMapModelListener( m_modellListener );

    if( m_legendThemeIcon != null )
      m_legendThemeIcon.dispose();

    super.dispose();
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, double,
   *      org.kalypsodeegree.model.geometry.GM_Envelope, boolean, org.eclipse.core.runtime.IProgressMonitor)
   */
  public void paint( final Graphics g, final GeoTransform p, final double scale, final GM_Envelope bbox, final boolean selected, final IProgressMonitor monitor ) throws CoreException
  {
    if( selected )
      return;
    final int wMax = g.getClipBounds().width;
    final int hMax = g.getClipBounds().height;
// System.out.println( "w:" + wMax + "\nh:" + hMax );
    if( m_image == null )
      updateLegend( wMax, hMax );
    if( m_image != null )
    {
      g.setPaintMode();
      final int widthIamge = m_image.getWidth( null );
      final int heightImage = m_image.getHeight( null );
      g.drawImage( m_image, wMax - widthIamge, hMax - heightImage, widthIamge, heightImage, null );
    }
  }

  protected final void invalidateLegend( )
  {
    m_image = null;
    invalidate( getFullExtent() );
  }

  private void updateLegend( int widthPerLegend, final int hMax ) throws CoreException
  {
    final List<Image> stylesCol = new ArrayList<Image>();
    final IMapModell mapModell = getMapModell();
    final int maxThems = mapModell.getThemeSize();
    final List<IKalypsoFeatureTheme> visibleThemes = new ArrayList<IKalypsoFeatureTheme>();
    for( int i = 0; i < maxThems; i++ )
    {
      final IKalypsoTheme theme = mapModell.getTheme( i );

      if( theme.isVisible() && theme instanceof IKalypsoFeatureTheme )
        visibleThemes.add( (IKalypsoFeatureTheme) theme );
    }
    final int legendSize = visibleThemes.size();
    if( legendSize == 0 )
      return;

    int heightPerLegend = hMax / legendSize;
    if( heightPerLegend > 40 )
      heightPerLegend = 40;
    if( heightPerLegend < 30 )
      heightPerLegend = 30;
    if( widthPerLegend > heightPerLegend * 4 )
      widthPerLegend = (heightPerLegend * 4);
    // widthPerLegend = 100;
    // heightPerLegend = 50;
    for( final IKalypsoFeatureTheme featureTheme : visibleThemes )
    {
      final UserStyle[] styles = featureTheme.getStyles();
      if( styles.length == 0 )
        return;

      final IFeatureType ft = featureTheme.getFeatureType();
      final int width = widthPerLegend / styles.length;
      for( int n = 0; n < styles.length; n++ )
      {
        final UserStyle style = styles[n];
        final Image styleImage = getLegend( ft, style, width, heightPerLegend );

        final Graphics g = styleImage.getGraphics();
        g.setPaintMode();
        g.setColor( backColor );
        g.setColor( Color.black );
        if( n == 0 )
        {
          g.setFont( m_font );
          g.setColor( Color.black );
          final String title = featureTheme.getLabel();
          if( title != null )
            g.drawString( title, 2, m_font.getSize() );
        }
        stylesCol.add( styleImage );
      }
    }

    if( stylesCol.isEmpty() )
      return;

    // draw bufferedImage...

    final int xMax = (legendSize * heightPerLegend) / hMax + 1;
    int yMax = hMax / heightPerLegend;
    if( yMax > legendSize )
      yMax = legendSize;
    System.out.println( "Image: " + xMax * widthPerLegend + "x" + hMax );
    final Image tmpImage = new BufferedImage( xMax * widthPerLegend, yMax * heightPerLegend, BufferedImage.TYPE_INT_RGB );
    final Graphics g = tmpImage.getGraphics();
    g.setPaintMode();
    g.setColor( backColor );
    // g.setColor( new Color( 1, 240, 240, 0 ) );
    g.fillRect( 0, 0, xMax * widthPerLegend, yMax * heightPerLegend );
    for( int x = 0; x < xMax; x++ )
    {
      for( int y = 0; y < yMax; y++ )
      {
        System.out.println( "x" + x + " y" + y );
        final int legendIndex = x * yMax + y;
        if( legendIndex < stylesCol.size() )
        {
          final Image styleImage = stylesCol.get( legendSize - legendIndex - 1 );
          g.drawImage( styleImage, x * widthPerLegend, heightPerLegend * y, widthPerLegend - 1, heightPerLegend - 1, null );
          g.setColor( Color.black );
          g.drawRect( x * widthPerLegend, heightPerLegend * y, widthPerLegend - 1, hMax - 2 );
        }
      }
    }
    // border
    g.setColor( Color.DARK_GRAY );
    g.drawRect( 0, 0, xMax * widthPerLegend - 1, yMax * heightPerLegend - 1 );
    m_image = tmpImage;
  }

  private Image getLegend( final IFeatureType ft, final UserStyle style, final int width, final int height ) throws CoreException
  {
    final double yborder = m_font.getSize() + 3;
    final double xborder = width / 3;
    final GM_Envelope srcEnv = GeometryFactory.createGM_Envelope( 0, 0, 1, 1, null );
    final GM_Envelope destEnv = GeometryFactory.createGM_Envelope( xborder, yborder, width - xborder, height - yborder, null );
    final GeoTransform transform = new WorldToScreenTransform( srcEnv, destEnv );

    final Image image = new BufferedImage( width, height, BufferedImage.TYPE_INT_RGB );
    final Graphics g = image.getGraphics();
    g.setColor( backColor );
    g.setPaintMode();
    g.fillRect( 0, 0, width, height );
    // test
    g.clipRect( 0, 0, width, height );
    final Feature feature = FeatureFactory.createFeature( null, null, "legende", ft, false );
    KalypsoLegendUtilities.updatePropertiesForLegend( feature );
    final DisplayElement[] des = DisplayElementFactory.createDisplayElement( feature, style );
    for( final DisplayElement de : des )
    {
      de.paint( g, transform, new NullProgressMonitor() );
    }
    return image;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getBoundingBox()
   */
  public GM_Envelope getFullExtent( )
  {
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#getDefaultIcon()
   */
  @Override
  protected ImageDescriptor getDefaultIcon( )
  {
    if( m_legendThemeIcon == null )
      m_legendThemeIcon = new org.eclipse.swt.graphics.Image( Display.getCurrent(), getClass().getResourceAsStream( "resources/legendTheme.gif" ) );

    return ImageDescriptor.createFromImage( m_legendThemeIcon );
  }
}