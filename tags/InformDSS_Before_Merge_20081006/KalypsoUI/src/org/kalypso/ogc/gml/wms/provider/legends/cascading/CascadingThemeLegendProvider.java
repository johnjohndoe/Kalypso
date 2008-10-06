/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ogc.gml.wms.provider.legends.cascading;

import java.util.ArrayList;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;
import org.kalypso.ogc.gml.AbstractCascadingLayerTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.outline.GisMapOutlineLabelProvider;
import org.kalypso.ogc.gml.wms.provider.legends.IKalypsoLegendProvider;
import org.kalypso.ogc.gml.wms.provider.legends.feature.LegendElement;

/**
 * This class provides the legend for a cascading theme. It will ask all childs and creates an summarized legend of
 * available legends.
 * 
 * @author Holger Albert
 */
public class CascadingThemeLegendProvider implements IKalypsoLegendProvider
{
  /**
   * The border, left free in the image.
   */
  private static int BORDER = 0;

  /**
   * The size of the icon.
   */
  private static int ICON_SIZE = LegendElement.ICON_SIZE;

  /**
   * The gap between the single rows.
   */
  private static int GAP = LegendElement.GAP;

  /**
   * The gap between two images.
   */
  private static int IMAGE_GAP = 5;

  /**
   * This variable stores the cascading theme.
   */
  private AbstractCascadingLayerTheme m_theme;

  /**
   * The constructor.
   */
  public CascadingThemeLegendProvider( AbstractCascadingLayerTheme theme )
  {
    m_theme = theme;
  }

  /**
   * @see org.kalypso.ogc.gml.wms.provider.IKalypsoLegendProvider#getLegendGraphic(org.eclipse.swt.graphics.Font)
   */
  public Image getLegendGraphic( Font font ) throws CoreException
  {
    /* No theme, no legend. */
    if( m_theme == null )
      return null;

    /* Memory for all legends. */
    ArrayList<Image> legends = new ArrayList<Image>();

    /* All childs available. */
    IKalypsoTheme[] childs = m_theme.getAllThemes();

    /* Get the legend of each child. */
    for( int i = 0; i < childs.length; i++ )
    {
      /* Get the theme. */
      IKalypsoTheme kalypsoTheme = childs[i];

      if( kalypsoTheme == null )
        continue;

      /* Get the legend. */
      Image legendGraphic = kalypsoTheme.getLegendGraphic( font );
      if( legendGraphic != null )
        legends.add( legendGraphic );
    }

    /* Create a legend element for this theme. */
    LegendElement legendElement = new LegendElement( font, 0, m_theme, new GisMapOutlineLabelProvider( false ) );

    /* Compute the size for the image. */
    Rectangle computeSize = computeSize( legends, legendElement );

    /* Create the image. */
    Image image = new Image( Display.getCurrent(), computeSize.width, computeSize.height );

    /* Need a graphical context. */
    GC gc = new GC( image );

    /* Set the font. */
    gc.setFont( font );

    /* Change the color. */
    gc.setForeground( gc.getDevice().getSystemColor( SWT.COLOR_WHITE ) );

    /* Draw on the context. */
    gc.fillRectangle( 0, 0, computeSize.width, computeSize.height );

    /* Change the color. */
    gc.setForeground( gc.getDevice().getSystemColor( SWT.COLOR_BLACK ) );

    /* Get the icon. */
    Image icon = legendElement.getImage();

    /* Draw for the theme itself. */
    gc.drawImage( icon, BORDER, BORDER );

    /* Draw the text. */
    gc.drawString( legendElement.getText(), BORDER + ICON_SIZE + GAP, BORDER );

    int heightSoFar = BORDER + legendElement.getSize().height + GAP;
    for( Image legend : legends )
    {
      /* Draw the legend. */
      gc.drawImage( legend, BORDER + ICON_SIZE + GAP, heightSoFar );

      /* Increase the height. */
      heightSoFar = heightSoFar + legend.getBounds().height + IMAGE_GAP;
    }

    return image;
  }

  /**
   * This function computes the size for an image with the given elements and the given font.
   * 
   * @param legends
   *            The list of legends.
   * @param legendElement
   *            The legend element for this theme.
   */
  private Rectangle computeSize( ArrayList<Image> legends, LegendElement legendElement )
  {
    /* The size for this theme. */
    Rectangle size = legendElement.getSize();

    /* If there are no other legends, the width and height will only represent the size for this theme. */
    if( legends.size() == 0 )
      return new Rectangle( 0, 0, size.width, size.height );

    /* Start width. */
    int width = 2 * BORDER;

    /* Start height. */
    int height = 2 * BORDER;

    /* The height needs to be increased by the height for the theme and a gap. */
    height = height + size.height + GAP;

    /* Memory for storing the longest width so far temporarly (the first value will be the width of the theme). */
    int temp = size.width;

    /* No collect the legends. */
    for( Image image : legends )
    {
      if( image == null )
        continue;

      /* The width will be largest one. */
      if( image.getBounds().width > temp )
        temp = image.getBounds().width;

      /* The height will be summarized. */
      height = height + image.getBounds().height + IMAGE_GAP;
    }

    /* Store the longest width. */
    width = width + temp + ICON_SIZE + GAP;

    /* After the last image, there is no need for a gap. */
    height = height - IMAGE_GAP;

    return new Rectangle( 0, 0, width, height );
  }
}