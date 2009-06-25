/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.contribs.java.awt;

import java.awt.Color;

/**
 * A {@link org.kalypso.contribs.java.awt.IHighlightColors}, which just stores the colors.
 * 
 * @author belger
 */
public class DefaultHighlightColors implements IHighlightColors
{
  private static final Color COLOR_FILL = ColorUtilities.createTransparent( Color.YELLOW, 255 );
  private static final Color COLOR_TEXT = ColorUtilities.createTransparent( Color.RED, 255 );
  private static final Color COLOR_LINE = ColorUtilities.createTransparent( Color.YELLOW, 255 );
  private static final Color COLOR_BORDER = ColorUtilities.createTransparent( Color.RED, 255 );
  private static final Color COLOR_BACKGROUND = ColorUtilities.createTransparent( Color.lightGray, 5 );

  private Color m_fillColor = COLOR_FILL;
  private Color m_textColor = COLOR_TEXT;
  private Color m_lineColor = COLOR_LINE;
  private Color m_borderColor = COLOR_BORDER;
  private Color m_backgroundColor = COLOR_BACKGROUND;

  /**
   * @see org.kalypso.contribs.java.awt.IHighlightColors#getFillColor()
   */
  public Color getFillColor()
  {
    return m_fillColor;
  }

  public void setFillColor( final Color fillColor )
  {
    m_fillColor = fillColor;
  }

  public Color getBackgroundColor()
  {
    return m_backgroundColor;
  }

  public void setBackgroundColor( final Color backgroundColor )
  {
    m_backgroundColor = backgroundColor;
  }

  public Color getBorderColor()
  {
    return m_borderColor;
  }

  public void setBorderColor( final Color borderColor )
  {
    m_borderColor = borderColor;
  }

  public Color getLineColor()
  {
    return m_lineColor;
  }

  public void setLineColor( final Color lineColor )
  {
    m_lineColor = lineColor;
  }

  public Color getTextColor()
  {
    return m_textColor;
  }

  public void setTextColor( final Color textColor )
  {
    m_textColor = textColor;
  }

  /**
   * Helper method, which returns the color if it is NOT the defautl color.
   * 
   * @return null, if the color equals the default color
   */
  public Color getUndefaultFillColor()
  {
    return COLOR_FILL.equals( m_fillColor ) ? null : m_fillColor;
  }

  /**
   * Helper method, which returns the color if it is NOT the defautl color.
   * 
   * @return null, if the color equals the default color
   */
  public Color getUndefaultTextColor()
  {
    return COLOR_TEXT.equals( m_textColor ) ? null : m_textColor;
  }

  /**
   * Helper method, which returns the color if it is NOT the defautl color.
   * 
   * @return null, if the color equals the default color
   */
  public Color getUndefaultLineColor()
  {
    return COLOR_LINE.equals( m_lineColor ) ? null : m_lineColor;
  }

  /**
   * Helper method, which returns the color if it is NOT the defautl color.
   * 
   * @return null, if the color equals the default color
   */
  public Color getUndefaultBorderColor()
  {
    return COLOR_BORDER.equals( m_borderColor ) ? null : m_borderColor;
  }

  /**
   * Helper method, which returns the color if it is NOT the defautl color.
   * 
   * @return null, if the color equals the default color
   */
  public Color getUndefaultBackgroundColor()
  {
    return COLOR_BACKGROUND.equals( m_backgroundColor ) ? null : m_backgroundColor;
  }
}
