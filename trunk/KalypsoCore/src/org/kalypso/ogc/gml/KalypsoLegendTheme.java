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
import java.util.ArrayList;
import java.util.List;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree.model.feature.event.ModellEventProviderAdapter;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory;
import org.kalypsodeegree_impl.graphics.transformation.WorldToScreenTransform;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author doemming
 */
public class KalypsoLegendTheme implements IKalypsoTheme, ModellEventListener
{
  private ModellEventProviderAdapter myEventProvider = new ModellEventProviderAdapter();

  private Image m_Image = null;

  private Color backColor = new Color( 240, 240, 240 );

  private int m_styleWidth = 170;

  private int m_styleHeight = 50;

  private Font m_font = new Font( "SansSerif", Font.BOLD, m_styleHeight / 5 );

  private int m_imageHeight = 0;

  private int m_imageWidth = 0;

  private String m_name;

  private final IMapModell m_mapModell;

  public KalypsoLegendTheme( IMapModell mapModell )
  {
    m_mapModell = mapModell;
    m_mapModell.addModellListener( this );
    m_name = "Legende";
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#dispose()
   */
  public void dispose()
  {
    m_mapModell.removeModellListener( this );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getName()
   */
  public String getName()
  {
    return m_name;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#setName(java.lang.String)
   */
  public void setName( String name )
  {
    m_name = name;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, double,
   *      org.kalypsodeegree.model.geometry.GM_Envelope, boolean)
   */
  public void paint( Graphics g, GeoTransform p, double scale, GM_Envelope bbox, final boolean selected )
  {
    if( selected )
      return;

    int w = g.getClipBounds().width;
    int h = g.getClipBounds().height;
    if( m_Image == null )
      updateLegend();
    if( m_Image != null )
    {
      g.setPaintMode();
      g.drawImage( m_Image, w - m_imageWidth, h - m_imageHeight, m_imageWidth, m_imageHeight, null );
    }
  }

  public void addModellListener( final ModellEventListener listener )
  {
    myEventProvider.addModellListener( listener );
  }

  public void fireModellEvent( final ModellEvent event )
  {
    myEventProvider.fireModellEvent( event );
  }

  public void removeModellListener( ModellEventListener listener )
  {
    myEventProvider.removeModellListener( listener );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    if( modellEvent != null && modellEvent.isType( ModellEvent.LEGEND_UPDATED ) )
      return;
    m_Image = null;
    //updateLegend();
  }

  private void updateLegend()
  {
    final List<Image> stylesCol = new ArrayList<Image>();

    int max = m_mapModell.getThemeSize();
    for( int i = 0; i < max; i++ )
    {
      final IKalypsoTheme theme = m_mapModell.getTheme( i );

      if( m_mapModell.isThemeEnabled( theme ) && theme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme featureTheme = (IKalypsoFeatureTheme)theme;

        final UserStyle[] styles = featureTheme.getStyles();
        final IFeatureType ft = featureTheme.getFeatureType();

        for( int n = 0; n < styles.length; n++ )
        {
          final UserStyle style = styles[n];
          final Image styleImage = getLegend( ft, style, m_styleWidth, m_styleHeight );
          final Graphics g = styleImage.getGraphics();
          g.setPaintMode();
          g.setColor( backColor );
          g.setColor( Color.black );

          if( n == 0 )
          {
            g.setFont( m_font );
            g.setColor( Color.black );
            final String title = theme.getName();
            g.drawString( title, 2, m_font.getSize() );

          }
          stylesCol.add( styleImage );
        }
      }
    }

    if( stylesCol.isEmpty() )
      return;

    // draw bufferedImage...
    final Image tmpImage = new BufferedImage( m_styleWidth, m_styleHeight * stylesCol.size(),
        BufferedImage.TYPE_INT_RGB );
    final Graphics g = tmpImage.getGraphics();
    g.setPaintMode();
    g.setColor( backColor );
    g.fillRect( 0, 0, m_styleWidth, m_styleHeight * stylesCol.size() );

    for( int i = 0; i < stylesCol.size(); i++ )
    {
      final Image styleImage = stylesCol.get( i );
      int pos = i;
      g.drawImage( styleImage, 0, m_styleHeight * pos, m_styleWidth - 1, m_styleHeight - 1, null );
      g.setColor( Color.black );
      g.drawRect( 0, m_styleHeight * pos, m_styleWidth - 1, m_styleWidth - 2 );
    }
    // rahmen
    g.setColor( Color.DARK_GRAY );
    m_imageHeight = m_styleHeight * stylesCol.size();
    m_imageWidth = m_styleWidth;
    g.drawRect( 0, 0, m_imageWidth - 1, m_imageHeight - 1 );
    m_Image = tmpImage;
    fireModellEvent( new ModellEvent( null, ModellEvent.LEGEND_UPDATED ) );
  }

  private Image getLegend( IFeatureType ft, UserStyle style, int width, int height )
  {
    double yborder = m_font.getSize() + 3;
    double xborder = width / 3;
    GM_Envelope srcEnv = GeometryFactory.createGM_Envelope( 0, 0, 1, 1 );
    GM_Envelope destEnv = GeometryFactory.createGM_Envelope( xborder, yborder, width - xborder, height - yborder );
    GeoTransform transform = new WorldToScreenTransform( srcEnv, destEnv );

    Image image = new BufferedImage( width, height, BufferedImage.TYPE_INT_RGB );
    Graphics g = image.getGraphics();
    g.setColor( backColor );
    g.setPaintMode();
    g.fillRect( 0, 0, width, height );
    final Feature feature = FeatureFactory.createFeature( "legende", ft, false );
    KalypsoLegendUtilities.updatePropertiesForLegend( feature );
    DisplayElement[] des = DisplayElementFactory.createDisplayElement( feature, new UserStyle[]
    { style }, null );
    for( int i = 0; i < des.length; i++ )
    {
      DisplayElement de = des[i];
      de.paint( g, transform );
    }
    return image;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getBoundingBox()
   */
  public GM_Envelope getBoundingBox()
  {
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getType()
   */
  public String getType()
  {
    return "";
  }
}