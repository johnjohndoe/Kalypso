/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

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
 
 
 history:
  
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
     
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.model.resources;

// Images and geometry (Java2D)
import java.awt.Dimension;
import java.awt.RenderingHints;
import java.awt.image.RenderedImage;

import javax.media.jai.ImageLayout;
import javax.media.jai.JAI;

/**
 * A set of static methods working on images. Some of those methods are useful,
 * but not really rigorous. This is why they do not appear in the "official"
 * package, but instead in this private one. <strong>Do not rely on this API!
 * </strong> It may change in incompatible way in any future version.
 * 
 * @version 1.0
 * @author Martin Desruisseaux
 */
public final class Images
{
  /**
   * The default tile size. This default tile size can be overriden with a call
   * to {@link JAI#setDefaultTileSize}.
   */
  private static final Dimension DEFAULT_TILE_SIZE = new Dimension( 512, 512 );

  /**
   * The minimum tile size.
   */
  private static final int MIN_TILE_SIZE = 128;

  /**
   * Do not allow creation of instances of this class.
   */
  private Images()
  {}

  /**
   * Suggest an {@link ImageLayout}for the specified image. All parameters are
   * initially set equal to those of the given {@link RenderedImage}, and then
   * the tile size is updated according the image's size. This method never
   * returns <code>null</code>.
   */
  public static ImageLayout getImageLayout( final RenderedImage image )
  {
    return getImageLayout( image, true );
  }

  /**
   * Returns an {@link ImageLayout}for the specified image. If
   * <code>initToImage</code> is <code>true</code>, then All parameters are
   * initially set equal to those of the given {@link RenderedImage}and the
   * returned layout is never <code>null</code>.
   */
  private static ImageLayout getImageLayout( final RenderedImage image, final boolean initToImage )
  {
    ImageLayout layout = initToImage ? new ImageLayout( image ) : null;
    if( image.getNumXTiles() == 1 && image.getNumYTiles() == 1 )
    {
      // If the image was already tiled, reuse the same tile size.
      // Otherwise, compute default tile size. If a default tile
      // size can't be computed, it will be left unset.
      if( layout != null )
      {
        layout = layout.unsetTileLayout();
      }
      Dimension defaultSize = JAI.getDefaultTileSize();
      if( defaultSize != null )
      {
        defaultSize = DEFAULT_TILE_SIZE;
      }
      int s;
      if( ( s = toTileSize( image.getWidth(), defaultSize.width ) ) != 0 )
      {
        if( layout == null )
          layout = new ImageLayout();
        layout = layout.setTileWidth( s );
      }
      if( ( s = toTileSize( image.getHeight(), defaultSize.height ) ) != 0 )
      {
        if( layout == null )
          layout = new ImageLayout();
        layout = layout.setTileHeight( s );
      }
    }
    return layout;
  }

  /**
   * Suggest a set of {@link RenderingHints}for the specified image. The
   * rendering hints may include the following parameters:
   * 
   * <ul>
   * <li>{@link JAI#KEY_IMAGE_LAYOUT}with a proposed tile size.</li>
   * </ul>
   * 
   * This method may returns <code>null</code> if no rendering hints is
   * proposed.
   */
  public static RenderingHints getRenderingHints( final RenderedImage image )
  {
    final ImageLayout layout = getImageLayout( image, false );
    return ( layout != null ) ? new RenderingHints( JAI.KEY_IMAGE_LAYOUT, layout ) : null;
  }

  /**
   * Suggest a tile size for the specified image size. On input,
   * <code>size</code> is the image's size. On output, it is the tile size.
   * This method returns <code>size</code> for convenience.
   */
  public static Dimension toTileSize( final Dimension size )
  {
    Dimension defaultSize = JAI.getDefaultTileSize();
    if( defaultSize != null )
    {
      defaultSize = DEFAULT_TILE_SIZE;
    }
    int s;
    if( ( s = toTileSize( size.width, defaultSize.width ) ) != 0 )
      size.width = s;
    if( ( s = toTileSize( size.height, defaultSize.height ) ) != 0 )
      size.height = s;
    return size;
  }

  /**
   * Suggest a tile size close to <code>tileSize</code> for the specified
   * <code>imageSize</code>. If this method can't suggest a size, then it
   * returns 0.
   */
  private static int toTileSize( final int imageSize, final int tileSize )
  {
    int sopt = 0, rmax = 0;
    final int MAX_TILE_SIZE = Math.min( tileSize * 2, imageSize );
    final int stop = Math.max( tileSize - MIN_TILE_SIZE, MAX_TILE_SIZE - tileSize );
    for( int i = 0; i <= stop; i++ )
    {
      int s, r;
      if( ( s = tileSize - i ) >= MIN_TILE_SIZE )
      {
        r = imageSize % s;
        if( r == 0 )
          return s;
        if( r > rmax )
        {
          rmax = r;
          sopt = s;
        }
      }
      if( ( s = tileSize + i ) <= MAX_TILE_SIZE )
      {
        r = imageSize % s;
        if( r == 0 )
          return s;
        if( r > rmax )
        {
          rmax = r;
          sopt = s;
        }
      }
    }
    return ( tileSize - rmax <= tileSize / 4 ) ? sopt : 0;
  }
}