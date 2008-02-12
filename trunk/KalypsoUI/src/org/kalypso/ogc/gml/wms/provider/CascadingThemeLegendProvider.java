/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.wms.provider;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.util.ArrayList;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.ogc.gml.IKalypsoCascadingTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;

/**
 * This class provides the legend for a cascading theme. It will ask all childs and creates an summarized legend of
 * available legends.
 * 
 * @author Holger Albert
 */
public class CascadingThemeLegendProvider implements IKalypsoLegendProvider
{
  /**
   * The gap between two images.
   */
  private static int GAP = 20;

  /**
   * This variable stores the cascading theme.
   */
  private IKalypsoCascadingTheme m_theme;

  /**
   * The constructor.
   */
  public CascadingThemeLegendProvider( IKalypsoCascadingTheme theme )
  {
    m_theme = theme;
  }

  /**
   * @see org.kalypso.ogc.gml.wms.provider.IKalypsoLegendProvider#getLegendGraphic(java.awt.Font, java.lang.String)
   */
  public Image getLegendGraphic( Font font, String layerName ) throws CoreException
  {
    /* No theme, no legend. */
    if( m_theme == null )
      return null;

    /* Memory for all legends. */
    ArrayList<Image> legends = new ArrayList<Image>();

    /* All childs available. */
    IKalypsoTheme[] childs = m_theme.getChildThemes();

    /* Get the legend of each child. */
    for( int i = 0; i < childs.length; i++ )
    {
      /* Get the theme. */
      IKalypsoTheme kalypsoTheme = childs[i];

      if( kalypsoTheme == null )
        continue;

      /* Get the legend. */
      legends.add( kalypsoTheme.getLegendGraphic( font, kalypsoTheme.getName() ) );
    }

    if( legends.size() == 0 )
      return null;

    /* Compute the size for the image. */
    Rectangle2D computeSize = computeSize( legends );

    /* Create the image. */
    BufferedImage image = new BufferedImage( (int) computeSize.getWidth(), (int) computeSize.getHeight(), BufferedImage.TYPE_INT_RGB );

    /* Get the graphic context. */
    Graphics graphics = image.getGraphics();

    /* Change the color. */
    graphics.setColor( Color.WHITE );

    /* Draw on the context. */
    graphics.fillRect( 0, 0, (int) computeSize.getWidth(), (int) computeSize.getHeight() );

    /* Change the color. */
    graphics.setColor( Color.BLACK );

    double heightSoFar = 0;
    for( Image legend : legends )
    {
      /* Draw the legend. */
      graphics.drawImage( legend, 0, (int) heightSoFar, null );

      /* Increase the height. */
      heightSoFar = heightSoFar + legend.getHeight( null ) + GAP;

      /* Change the color. */
      graphics.setColor( Color.BLUE );

      /* Draw a line. */
      graphics.drawLine( 0, (int) (heightSoFar - GAP / 2), (int) computeSize.getWidth(), (int) (heightSoFar - GAP / 2) );

      /* Reset the color. */
      graphics.setColor( Color.BLACK );
    }

    return image;
  }

  /**
   * This function computes the size for an image with the given elements and the given font.
   * 
   * @param legends
   *            The list of legends.
   */
  private Rectangle2D computeSize( ArrayList<Image> legends )
  {
    /* The width of the legend. */
    double width = 0;

    /* The height of the legend. */
    double height = 0;

    /* Memory for storing the longest width so far temporarly. */
    double temp = 0;

    for( Image image : legends )
    {
      if( image == null )
        continue;

      /* The width will be largest one. */
      if( image.getWidth( null ) > temp )
        temp = image.getWidth( null );

      /* The height will be summarized. */
      height = height + image.getHeight( null ) + GAP;
    }

    /* Store the longest width. */
    width = width + temp;

    /* After the last image, there is no need for a gap. */
    height = height - GAP;

    return new Rectangle2D.Double( 0, 0, width, height );
  }
}