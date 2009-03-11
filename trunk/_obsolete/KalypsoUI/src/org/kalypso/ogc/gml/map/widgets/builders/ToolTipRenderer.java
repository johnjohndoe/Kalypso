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
package org.kalypso.ogc.gml.map.widgets.builders;

import java.awt.Color;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.List;

/**
 * TODO: throw away and merge with {@link org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer}.
 * 
 * @author Dirk Kuch
 */
public class ToolTipRenderer
{
  private static final int MAX_TEXT_LENGTH = 60;

  private static final Color m_backgroundColor = new Color( 237, 213, 76, 200 );

  private static final Color m_borderColor = new Color( 0, 0, 0, 255 );

  private static final Color m_textColor = new Color( 0, 0, 0, 255 );

  private final IGeometryBuilderExtensionProvider m_tooltip;

  public ToolTipRenderer( final IGeometryBuilderExtensionProvider tooltip )
  {
    m_tooltip = tooltip;
  }

  private Rectangle2D calculateBoxWidth( final Graphics g, final String[] tooltip )
  {
    final FontMetrics fontMetrics = g.getFontMetrics();

    final Rectangle2D maxRect = new Rectangle2D.Double();
    for( final String row : tooltip )
    {
      final Rectangle2D rowBounds = fontMetrics.getStringBounds( row, g );
      Rectangle2D.union( maxRect, rowBounds, maxRect );
    }

    return maxRect;
  }

  @SuppressWarnings("deprecation") //$NON-NLS-1$
  public void paint( final Graphics g )
  {
    if( m_tooltip == null )
      return;

    final String[] tooltip = adjustTooltip( m_tooltip.getTooltip() );

    if( tooltip == null || tooltip.length == 0 )
      return;

    final Rectangle2D maxRectangle = calculateBoxWidth( g, tooltip );

    final int textHeight = (int) (maxRectangle.getHeight()); // height of one line
    final int textboxWidth = (int) maxRectangle.getWidth() + 10; // width of textbox
    final int textboxHeight = textHeight * tooltip.length + 10; // height of textbox

    g.setColor( m_backgroundColor );
    g.fillRect( 5, 5, textboxWidth, textboxHeight );

    g.setColor( m_borderColor );
    g.drawRect( 5, 5, textboxWidth, textboxHeight );

    int baseLineY = 22;
    for( final String row : tooltip )
    {
      g.drawString( row, 10, baseLineY );
      baseLineY += textHeight;
    }
  }

  private String[] adjustTooltip( final String[] tooltips )
  {
    final List<String> myTooltips = new ArrayList<String>();

    for( final String tooltip : tooltips )
    {
      if( tooltip.length() > MAX_TEXT_LENGTH )
      {
        String myString = ""; //$NON-NLS-1$
        final String[] splitted = tooltip.split( " " ); //$NON-NLS-1$

        for( final String s : splitted )
        {
          myString += s + " "; //$NON-NLS-1$

          if( myString.length() > MAX_TEXT_LENGTH )
          {
            myTooltips.add( myString );
            myString = ""; //$NON-NLS-1$
          }
        }

        myTooltips.add( myString );
      }
      else
        myTooltips.add( tooltip );
    }

    return myTooltips.toArray( new String[] {} );
  }
}
