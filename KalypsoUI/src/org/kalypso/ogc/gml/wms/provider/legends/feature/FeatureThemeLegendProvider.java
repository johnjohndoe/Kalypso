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
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Device;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.PaletteData;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
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
  private final IKalypsoFeatureTheme m_theme;

  /**
   * The constructor.
   */
  public FeatureThemeLegendProvider( final IKalypsoFeatureTheme theme )
  {
    m_theme = theme;
  }

  /**
   * @see org.kalypso.ogc.gml.wms.provider.IKalypsoLegendProvider#getLegendGraphic(org.eclipse.swt.graphics.Font)
   */
  public Image getLegendGraphic( final Font font )
  {
    /* No theme, no legend. */
    if( m_theme == null )
      return null;

    // TODO: this is too much copy paste; reuse code from MapUtilities

    /* All elements in this theme. */
    final List<LegendElement> elements = collectElements( font );

    if( elements.size() == 0 )
      return null;

    /* Compute the size for the image. */
    final Rectangle computeSize = computeSize( elements );

    /* Create the image. */
    // HM: quite complicated to create a transparent image; any other ideas?
    final Device device = font.getDevice();
    final ImageData id = new ImageData( computeSize.width, computeSize.height, 32, new PaletteData( 0xFF, 0xFF00, 0xFF0000 ) );
    id.transparentPixel = 0xfffffe;
    final Image image = new Image( device, id );

    /* Need a graphical context. */
    final GC gc = new GC( image );

    final Color transparentColor = new Color( device, new RGB( 0xfe, 0xff, 0xff ) );
    gc.setBackground( transparentColor );
    gc.fillRectangle( image.getBounds() );
    transparentColor.dispose();

    /* Set the font. */
    gc.setFont( font );

    /* Change the color. */
    gc.setForeground( gc.getDevice().getSystemColor( SWT.COLOR_BLACK ) );

    int heightSoFar = BORDER;
    for( int i = 0; i < elements.size(); i++ )
    {
      /* Get the legend element. */
      final LegendElement legendElement = elements.get( i );

      /* Get the icon. */
      final Image icon = legendElement.getImage();

      /* Draw the icon. */
      if( icon != null )
      gc.drawImage( icon, BORDER + legendElement.getLevel() * (ICON_SIZE + GAP), heightSoFar );

      /* Draw the text. */
      gc.drawString( legendElement.getText(), BORDER + (ICON_SIZE + GAP) + legendElement.getLevel() * (ICON_SIZE + GAP), heightSoFar, true );

      // TODO: images should be disposed here (but getLEgendGeaphic returns
      // sometimes images than can be disposed, sometimes not)

      /* Add the height of the element and increase by gap. */
      heightSoFar = heightSoFar + legendElement.getSize().height + GAP;
    }

    gc.dispose();

    return image;
  }

  /**
   * This function collects all elements, contained in the theme.
   *
   * @param font
   *            The font, to use.
   * @return A list, containing all elements.
   */
  private ArrayList<LegendElement> collectElements( final Font font )
  {
    /* The label provider. */
    final GisMapOutlineLabelProvider labelProvider = new GisMapOutlineLabelProvider( false, false );

    /* Memory for the elements. */
    final ArrayList<LegendElement> elements = new ArrayList<LegendElement>();
    elements.add( new LegendElement( font, 0, m_theme, labelProvider ) );

    /* The content provider. */
    final GisMapOutlineContentProvider contentProvider = new GisMapOutlineContentProvider( labelProvider );

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
  private void collect( final GisMapOutlineContentProvider contentProvider, final GisMapOutlineLabelProvider labelProvider, final Font font, final Object startElement, final ArrayList<LegendElement> elements, final int level )
  {
    final Object[] children = contentProvider.getChildren( startElement );
    for( final Object element : children )
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
  private Rectangle computeSize( final List<LegendElement> elements )
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
    for( final LegendElement legendElement : elements )
    {
      /* Get the size of the element. */
      final Rectangle size = legendElement.getSize();

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