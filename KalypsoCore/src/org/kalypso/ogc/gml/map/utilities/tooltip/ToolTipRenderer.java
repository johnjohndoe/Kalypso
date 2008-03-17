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
package org.kalypso.ogc.gml.map.utilities.tooltip;

import java.awt.Color;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.geom.Rectangle2D;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.Assert;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.GC;

/**
 * @author Dirk Kuch
 */
public class ToolTipRenderer
{
  public static final Color DEFAULT_TEXT_COLOR = Color.BLACK;

  public static final Color DEFAULT_BORDER_COLOR = Color.DARK_GRAY;

  public static final Color DEFAULT_BACKGROUND_COLOR = new Color( 1f, 1f, 0.6f, 0.90f );

  public static final Point DEFAULT_OFFSET = new Point( 2, 2 );

  public static final Insets DEFAULT_INSETS = new Insets( 4, 5, 4, 5 );

  private static Pattern LINE_SPLIT = Pattern.compile( "$", Pattern.MULTILINE );

  private final Insets m_insets;

  /** Offset from paint-point to actual tooltip */
  private final Point m_offset;

  /** content of tooltip (rows which will be rendered) */
  private String[] m_rows;

  private Color m_backgroundColor;

  private final Color m_borderColor;

  private final Color m_textColor;

  public ToolTipRenderer( )
  {
    this( DEFAULT_INSETS, DEFAULT_OFFSET, DEFAULT_BACKGROUND_COLOR, DEFAULT_BORDER_COLOR, DEFAULT_TEXT_COLOR );
  }

  public ToolTipRenderer( final Insets insets, final Point offset, final Color backgroundColor, final Color borderColor, final Color textColor )
  {
    m_insets = insets;
    m_offset = offset;
    m_backgroundColor = backgroundColor;
    m_borderColor = borderColor;
    m_textColor = textColor;
  }

  private Rectangle2D calculateBoxWidth( final Graphics g )
  {
    final FontMetrics fontMetrics = g.getFontMetrics();

    final Rectangle2D maxRect = new Rectangle2D.Double();
    for( final String row : m_rows )
    {
      final Rectangle2D rowBounds = fontMetrics.getStringBounds( row, g );
      Rectangle2D.union( maxRect, rowBounds, maxRect );
    }

    return maxRect;
  }

  /**
   * Paints a tooltip to the given position.<br>
   * If a <code>screenRect</code> is given, the position may be adjusted in order to show the whole tooltip inside the
   * given rectangle.
   */
  public void paintToolTip( final Point point, final Graphics g, final Rectangle screenRect )
  {
    Assert.isNotNull( point );
    Assert.isNotNull( g );

    /* Do not show empty tooltips */
    if( m_rows == null || m_rows.length == 0 )
      return;

    final Rectangle2D maxRect = calculateBoxWidth( g );

    final int textHeight = (int) (maxRect.getHeight()); // height of one line
    final int textboxWidth = (int) maxRect.getWidth(); // width of textbox
    final int textboxHeight = textHeight * m_rows.length; // height of textbox

    final int insetsHeigth = m_insets.bottom + m_insets.top;
    final int insetsWidth = m_insets.left + m_insets.right;

    final Point basePoint = new Point( point.x + m_offset.x, point.y - m_offset.y - textboxHeight - insetsHeigth );

    final int outlineWidth = textboxWidth + insetsWidth;
    final int outlineHeight = textboxHeight + insetsHeigth;

    if( screenRect != null )
    {
      /* Adjust basePoint in order to show the whole tooltip */
      if( basePoint.x + outlineWidth > screenRect.getMaxX() )
        basePoint.x = (int) screenRect.getMaxX() - outlineWidth - 1;

      if( basePoint.y + outlineHeight > screenRect.getMaxY() )
        basePoint.y = (int) screenRect.getMaxY() - outlineHeight;

      if( basePoint.x < screenRect.x )
        basePoint.x = screenRect.x;

      if( basePoint.y < screenRect.y )
        basePoint.y = screenRect.y;
    }

    /* draw outer rectangle */
    g.setColor( m_backgroundColor );
    g.fillRect( basePoint.x, basePoint.y, outlineWidth, outlineHeight );

    /* Draw Border */
    g.setColor( m_borderColor );
    g.drawRect( basePoint.x, basePoint.y, outlineWidth, outlineHeight );

    // debug: draw textbox
// g.drawRect( basePoint.x + m_insets.left, basePoint.y + m_insets.top, textboxWidth, textboxHeight );

    /* draw tooltip labels */
    g.setColor( m_textColor );
    final int magicNumber = 3; // move text a bit to the top, in order to nicely center it
    int baseLineY = basePoint.y + m_insets.top + +textHeight - magicNumber;
    for( final String element : m_rows )
    {
      g.drawString( element, basePoint.x + m_insets.left, baseLineY );
      baseLineY += textHeight;
    }
  }

  public void paintTooltip( final org.eclipse.swt.graphics.Point point, final GC gc, final org.eclipse.swt.graphics.Rectangle screenRect )
  {
    Assert.isNotNull( point );
    Assert.isNotNull( gc );

    /* Do not show empty tooltips */
    if( m_rows == null || m_rows.length == 0 )
      return;

    final StringBuffer text = new StringBuffer();
    for( int i = 0; i < m_rows.length; i++ )
    {
      text.append( m_rows[i] );
      if( i != m_rows.length - 1 )
        text.append( '\n' );
    }
    final String tooltip = text.toString();

    final int drawFlags = SWT.DRAW_DELIMITER; // SWT.DRAW_DELIMITER | SWT.DRAW_MNEMONIC | SWT.DRAW_TAB /* |
    // SWT.DRAW_TRANSPARENT
    // */;
    final org.eclipse.swt.graphics.Point textBoxSize = gc.textExtent( tooltip, drawFlags );

    final int insetsHeigth = m_insets.bottom + m_insets.top;
    final int insetsWidth = m_insets.left + m_insets.right;

    final Point basePoint = new Point( point.x + m_offset.x, point.y - m_offset.y - textBoxSize.y - insetsHeigth );

    final int outlineWidth = textBoxSize.x + insetsWidth;
    final int outlineHeight = textBoxSize.y + insetsHeigth;

    if( screenRect != null )
    {
      /* Adjust basePoint in order to show the whole tooltip */

      if( basePoint.x + outlineWidth > screenRect.x + screenRect.width )
        basePoint.x = screenRect.x + screenRect.width - outlineWidth - 1;

      if( basePoint.y + outlineHeight > screenRect.y + screenRect.height )
        basePoint.y = screenRect.y - outlineHeight;

      if( basePoint.x < screenRect.x )
        basePoint.x = screenRect.x;

      if( basePoint.y < screenRect.y )
        basePoint.y = screenRect.y;
    }

    final org.eclipse.swt.graphics.Color textColor = gc.getDevice().getSystemColor( SWT.COLOR_INFO_FOREGROUND );
    final org.eclipse.swt.graphics.Color bgColor = gc.getDevice().getSystemColor( SWT.COLOR_INFO_BACKGROUND );
    final org.eclipse.swt.graphics.Color borderColor = gc.getDevice().getSystemColor( SWT.COLOR_BLACK );

    gc.setAlpha( 255 );

    /* draw outer rectangle */
    gc.setBackground( bgColor );
    gc.fillRectangle( basePoint.x, basePoint.y, outlineWidth, outlineHeight );

    /* Draw Border */
    gc.setForeground( borderColor );
    gc.drawRectangle( basePoint.x, basePoint.y, outlineWidth, outlineHeight );

    /* draw tooltip labels */
    gc.setForeground( textColor );
    gc.drawText( tooltip, basePoint.x + m_insets.left, basePoint.y + m_insets.top, drawFlags );

// // debug: draw textbox and basepoint
// gc.drawRectangle( basePoint.x + m_insets.left, basePoint.y + m_insets.top, textBoxSize.x, textBoxSize.y );
// gc.drawOval( basePoint.x, basePoint.y, 2, 2 );
  }

  public void setInputData( final String[] rows )
  {
    if( rows == null )
    {
      m_rows = null;
      return;
    }

    // Strip any carriage returns from the input
    m_rows = new String[rows.length];
    for( int i = 0; i < rows.length; i++ )
    {
      m_rows[i] = rows[i].replaceAll( "\n|\r", "" );
    }
  }

  public void setTooltip( final String tooltip )
  {
    setInputData( tooltip == null ? null : LINE_SPLIT.split( tooltip ) );
  }

  public void setBackgroundColor( final Color backgroundColor )
  {
    m_backgroundColor = backgroundColor;
  }
}
