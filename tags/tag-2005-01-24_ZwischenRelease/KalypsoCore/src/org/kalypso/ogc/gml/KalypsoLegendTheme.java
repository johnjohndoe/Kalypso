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

import org.deegree.graphics.displayelements.DisplayElement;
import org.deegree.graphics.displayelements.IncompatibleGeometryTypeException;
import org.deegree.graphics.sld.UserStyle;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.deegree.model.feature.event.ModellEventProviderAdapter;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Position;
import org.deegree_impl.graphics.displayelements.DisplayElementFactory;
import org.deegree_impl.graphics.transformation.WorldToScreenTransform;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.kalypso.ogc.gml.mapmodel.IMapModell;

/**
 * @author sbad0205
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
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paintSelected(java.awt.Graphics,
   *      org.deegree.graphics.transformation.GeoTransform, double,
   *      org.deegree.model.geometry.GM_Envelope, int)
   */
  public void paintSelected( Graphics g, GeoTransform p, double scale, GM_Envelope bbox,
      int selectionId )
  {
    int w = g.getClipBounds().width;
    int h = g.getClipBounds().height;
    if( m_Image == null )
      updateLegend();
    if( selectionId < 0 && m_Image != null )
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
   * @see org.deegree.model.feature.event.ModellEventListener#onModellChange(org.deegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {

    if( modellEvent != null && modellEvent.getType() == ModellEvent.LEGEND_UPDATED )
      return;
    m_Image = null;
    //updateLegend();
  }

  private void updateLegend()
  {
    List stylesCol = new ArrayList();

    int max = m_mapModell.getThemeSize();
    for( int i = 0; i < max; i++ )
    {
      final IKalypsoTheme theme = m_mapModell.getTheme( i );

      if( m_mapModell.isThemeEnabled( theme ) && theme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme featureTheme = (IKalypsoFeatureTheme)theme;

        final UserStyle[] styles = featureTheme.getStyles();
        final FeatureType ft = featureTheme.getFeatureType();

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
      final Image styleImage = (Image)stylesCol.get( i );
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

  private Image getLegend( FeatureType ft, UserStyle style, int width, int height )
  {
    double yborder = m_font.getSize() + 3;
    double xborder = width / 3;
    GM_Envelope srcEnv = GeometryFactory.createGM_Envelope( 0, 0, 1, 1 );
    GM_Envelope destEnv = GeometryFactory.createGM_Envelope( xborder, yborder, width - xborder,
        height - yborder );
    GeoTransform transform = new WorldToScreenTransform( srcEnv, destEnv );

    Image image = new BufferedImage( width, height, BufferedImage.TYPE_INT_RGB );
    Graphics g = image.getGraphics();
    g.setColor( backColor );
    g.setPaintMode();
    g.fillRect( 0, 0, width, height );
    Feature feature = createDefaultFeature( ft );
    try
    {
      DisplayElement[] des = DisplayElementFactory.createDisplayElement( feature, new UserStyle[]
      { style } );
      for( int i = 0; i < des.length; i++ )
      {
        DisplayElement de = des[i];
        de.paint( g, transform );
      }
    }
    catch( IncompatibleGeometryTypeException e )
    {
      e.printStackTrace();
    }
    return image;
  }

  private Feature createDefaultFeature( FeatureType ft )
  {
    FeatureTypeProperty[] propTypes = ft.getProperties();
    FeatureProperty[] props = createDefaultFeatureProperty( propTypes );
    Feature feature = FeatureFactory.createFeature( "default", ft, props );
    return feature;
  }

  private FeatureProperty[] createDefaultFeatureProperty( FeatureTypeProperty[] propTypes )
  {
    List results = new ArrayList();
    for( int i = 0; i < propTypes.length; i++ )
    {
      FeatureTypeProperty ftp = propTypes[i];
      Object value = ftp.getName();
      String type = ftp.getType();
      if( "java.lang.String".equals( type ) )
        value = ftp.getName();
      if( "org.deegree.model.geometry.GM_Point".equals( type ) )
        value = DEFAULT_POINT;
      if( "org.deegree.model.geometry.GM_LineString".equals( type ) )
        value = DEFAULT_LINESTRING;
      if( "org.deegree.model.geometry.GM_Polygon".equals( type ) )
        value = DEFAULT_POLYGONE;
      // TODO if type=Feature ... createDeafultFeature
      results.add( FeatureFactory.createFeatureProperty( ftp.getName(), value ) );
    }
    return (FeatureProperty[])results.toArray( new FeatureProperty[results.size()] );
  }

  private static GM_Object DEFAULT_POINT = GeometryFactory.createGM_Point( 0.5, 0.5, null );

  private static GM_Position[] DEFAULT_LINEPOSITIONS = new GM_Position[]
  {
      GeometryFactory.createGM_Position( 0.00, 0.3 ),
      GeometryFactory.createGM_Position( 0.33, 0.7 ),
      GeometryFactory.createGM_Position( 0.66, 0.3 ),
      GeometryFactory.createGM_Position( 1.00, 0.7 ), };

  private static GM_Envelope DEFAULT_ENVELOPE = GeometryFactory.createGM_Envelope( 0, 0, 1, 1 );

  private static GM_Object DEFAULT_LINESTRING = null;

  private static GM_Object DEFAULT_POLYGONE = null;
  static
  {
    try
    {
      DEFAULT_LINESTRING = GeometryFactory.createGM_Curve( DEFAULT_LINEPOSITIONS, null );
    }
    catch( GM_Exception e )
    {
      DEFAULT_LINESTRING = null;
      e.printStackTrace();
    }
  }
  static
  {
    try
    {
      DEFAULT_POLYGONE = GeometryFactory.createGM_Surface( DEFAULT_ENVELOPE, null );
    }
    catch( GM_Exception e )
    {
      e.printStackTrace();
    }
  }
  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getBoundingBox()
   */
  public GM_Envelope getBoundingBox()
  {
    return null;
  }
}