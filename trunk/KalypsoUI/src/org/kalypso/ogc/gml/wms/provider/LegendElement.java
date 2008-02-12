/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import java.awt.Font;
import java.awt.font.FontRenderContext;
import java.awt.geom.Rectangle2D;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

/**
 * This class wraps an object for the legend graphic.
 * 
 * @author Holger Albert
 */
public class LegendElement
{
  /**
   * This is the size of the icon. This size is used for summarizing the width of the legend item.
   */
  public static int ICON_SIZE = 16;

  /**
   * This is the gap after the icon. This size is used for summarizing the width of the legend item.
   */
  public static int GAP = 4;

  /**
   * The font, to use for this legend element.
   */
  private Font m_font;

  /**
   * The level of this legend element.
   */
  private int m_level;

  /**
   * The object of this legend element.
   */
  private Object m_object;

  /**
   * The content provider will be used for determining the text for this element.
   */
  private LabelProvider m_labelProvider;

  /**
   * The constructor.
   * 
   * @param font
   *            The font, to use for this legend element.
   * @param level
   *            The level of this legend element.
   * @param object
   *            The object of this legend element.
   */
  public LegendElement( Font font, int level, Object object, LabelProvider labelProvider )
  {
    m_font = font;
    m_level = level;
    m_object = object;
    m_labelProvider = labelProvider;
  }

  /**
   * This function returns the dimension of this legend element.
   * 
   * @return The dimension of this element.
   */
  public Rectangle2D getSize( )
  {
    FontRenderContext context = new FontRenderContext( m_font.getTransform(), true, false );

    Rectangle2D bounds = m_font.getStringBounds( getText(), context );

    /* Width. */
    double width = bounds.getWidth() + (ICON_SIZE + GAP) + m_level * (ICON_SIZE + GAP);

    /* Height. */
    double height = ICON_SIZE;
    double temp = bounds.getHeight();
    if( temp > ICON_SIZE )
      height = temp;

    return new Rectangle2D.Double( 0, 0, width, height );
  }

  /**
   * This function returns the text for this legend item.
   * 
   * @return The text of this legend item.
   */
  public String getText( )
  {
    if( m_labelProvider == null )
      return "Item " + String.valueOf( m_level );

    return m_labelProvider.getText( m_object );
  }

  /**
   * This function returns the image for this legend item.
   * 
   * @return The image of this legend item.
   */
  public Image getImage( )
  {
    if( m_labelProvider == null )
      return null;

    return m_labelProvider.getImage( m_object );
  }

  /**
   * This function returns the font, to use for this legend element.
   * 
   * @return The font, to use for this legend element.
   */
  public Font getFont( )
  {
    return m_font;
  }

  /**
   * This function returns the level of this legend element.
   * 
   * @return The level of this legend element.
   */
  public int getLevel( )
  {
    return m_level;
  }

  /**
   * This function returns the object of this legend element.
   * 
   * @return The object of this legend element.
   */
  public Object getObject( )
  {
    return m_object;
  }
}