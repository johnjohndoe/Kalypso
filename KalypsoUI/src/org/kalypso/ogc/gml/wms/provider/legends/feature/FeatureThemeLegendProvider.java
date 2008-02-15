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
package org.kalypso.ogc.gml.wms.provider.legends.feature;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.outline.GisMapOutlineContentProvider;
import org.kalypso.ogc.gml.outline.GisMapOutlineLabelProvider;
import org.kalypso.ogc.gml.wms.provider.legends.IKalypsoLegendProvider;

/**
 * This class provides the legend for a feature theme.
 * 
 * @author Holger Albert
 */
public class FeatureThemeLegendProvider implements IKalypsoLegendProvider
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
   * This variable stores the feature theme.
   */
  private IKalypsoFeatureTheme m_theme;

  /**
   * The constructor.
   */
  public FeatureThemeLegendProvider( IKalypsoFeatureTheme theme )
  {
    m_theme = theme;
  }

  /**
   * @see org.kalypso.ogc.gml.wms.provider.IKalypsoLegendProvider#getLegendGraphic(org.eclipse.swt.graphics.Font)
   */
  public Image getLegendGraphic( Font font )
  {
    /* No theme, no legend. */
    if( m_theme == null )
      return null;

    /* All elements in this theme. */
    ArrayList<LegendElement> elements = collectElements( font );

    if( elements.size() == 0 )
      return null;

    /* Compute the size for the image. */
    Rectangle computeSize = computeSize( elements );

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

    int heightSoFar = BORDER;
    for( int i = 0; i < elements.size(); i++ )
    {
      /* Get the legend element. */
      LegendElement legendElement = elements.get( i );

      /* Get the icon. */
      Image icon = legendElement.getImage();

      /* Draw the icon. */
      gc.drawImage( icon, BORDER + legendElement.getLevel() * (ICON_SIZE + GAP), heightSoFar );

      /* Draw the text. */
      gc.drawString( legendElement.getText(), BORDER + (ICON_SIZE + GAP) + legendElement.getLevel() * (ICON_SIZE + GAP), heightSoFar );

      /* Add the height of the element and increase by gap. */
      heightSoFar = heightSoFar + legendElement.getSize().height + GAP;
    }

    /* Return the image. */

    return image;
  }

  /**
   * This function collects all elements, contained in the theme.
   * 
   * @param font
   *            The font, to use.
   * @return A list, containing all elements.
   */
  private ArrayList<LegendElement> collectElements( Font font )
  {
    /* The label provider. */
    GisMapOutlineLabelProvider labelProvider = new GisMapOutlineLabelProvider( false );

    /* Memory for the elements. */
    ArrayList<LegendElement> elements = new ArrayList<LegendElement>();
    elements.add( new LegendElement( font, 0, m_theme, labelProvider ) );

    /* The content provider. */
    GisMapOutlineContentProvider contentProvider = new GisMapOutlineContentProvider();

    /* Collect all elements. */
    collect( contentProvider, labelProvider, font, m_theme, elements, 1 );

    return elements;
  }

  /**
   * This function collects all elements in an one level array.
   * 
   * @param contentProvider
   *            The content provider, with which the elements can be retrieved.
   * @param font
   *            The font, to use.
   * @param startElement
   *            The element, where the search begins.
   * @param elements
   *            Every element will be added to this list.
   * @param level
   *            The level of recursion.
   */
  private void collect( GisMapOutlineContentProvider contentProvider, GisMapOutlineLabelProvider labelProvider, Font font, Object startElement, ArrayList<LegendElement> elements, int level )
  {
    Object[] children = contentProvider.getChildren( startElement );
    for( Object element : children )
    {
      /* Add the element. */
      elements.add( new LegendElement( font, level, element, labelProvider ) );

      /* If it has children, call recursive and add them, too. */
      if( contentProvider.hasChildren( element ) )
        collect( contentProvider, labelProvider, font, element, elements, level + 1 );
    }
  }

  /**
   * This function computes the size for an image with the given elements and the given font.
   * 
   * @param elements
   *            The list of elements.
   */
  private Rectangle computeSize( ArrayList<LegendElement> elements )
  {
    /* Start width. */
    int width = 2 * BORDER;

    /* Start height. */
    int height = 2 * BORDER;

    /* Memory for storing the longest width so far temporarly. */
    int temp = 0;

    /* Memory for the highest level. */
    int amount = 0;

    /* Loop for finding the longest width and for summarizing the height. */
    for( LegendElement legendElement : elements )
    {
      /* Get the size of the element. */
      Rectangle size = legendElement.getSize();

      /* For the longest width. */
      if( size.width > temp )
        temp = size.width;

      if( legendElement.getLevel() > amount )
        amount = legendElement.getLevel();

      /* Add the height an a small space. */
      height = height + size.height + GAP;
    }

    /* Store the longest width. */
    width = width + temp + amount * (ICON_SIZE + GAP);

    /* After the last image, there is no need for a gap. */
    height = height - GAP;

    return new Rectangle( 0, 0, width, height );
  }
}