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
package org.kalypso.ogc.gml.map.themes;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.geom.Rectangle2D;
import java.net.URL;
import java.util.LinkedList;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.kalypso.ogc.gml.AbstractKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * This theme is able to create a small image, displaying the scale of the map.
 * 
 * @author Holger Albert
 */
public class KalypsoScaleTheme extends AbstractKalypsoTheme
{
  /**
   * The constructor.
   */
  public KalypsoScaleTheme( String name, String type, IMapModell mapModel, String legendIcon, URL context, boolean shouldShowChildren )
  {
    super( name, type, mapModel, legendIcon, context, shouldShowChildren );
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#setExtent(int, int, org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  @Override
  public void setExtent( int width, int height, GM_Envelope extent )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#isLoaded()
   */
  @Override
  public boolean isLoaded( )
  {
    return true;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getFullExtent()
   */
  public GM_Envelope getFullExtent( )
  {
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#getDefaultIcon()
   */
  @Override
  protected ImageDescriptor getDefaultIcon( )
  {
    return KalypsoGisPlugin.getImageProvider().getImageDescriptor( ImageProvider.DESCRIPTORS.IMAGE_THEME_SCALE );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, double,
   *      org.kalypsodeegree.model.geometry.GM_Envelope, boolean, org.eclipse.core.runtime.IProgressMonitor)
   */
  public void paint( Graphics g, GeoTransform p, double scale, GM_Envelope bbox, boolean selected, IProgressMonitor monitor )
  {
    if( selected == true )
      return;

    /* The number of sub rectangles. */
    int NUMBER_SUBS = 5;

    /* Determine the offsets. */
    int offset_x = -30;
    int offset_y = -5;

    /* The maximal available width (25% of map) for drawing the scale. */
    Rectangle bounds = g.getClipBounds();
    double max_width = (bounds.getWidth() / 4) + offset_x;

    /* Determine the distance from offset_x to max_width and check its value. */
    double offsetX = p.getSourceX( offset_x );
    double offsetY = p.getSourceY( offset_y );

    double maxX = p.getSourceX( max_width );
    double maxY = p.getSourceY( offset_y );

    GM_Point startGMPoint = GeometryFactory.createGM_Point( offsetX, offsetY, getMapModell().getCoordinatesSystem() );
    GM_Point coordGMPoint = GeometryFactory.createGM_Point( maxX, maxY, getMapModell().getCoordinatesSystem() );

    double distance = startGMPoint.distance( coordGMPoint );

    /* Round the distance to a good value. */
    double roundedDistance = round( distance );
    double subDistance = roundedDistance / NUMBER_SUBS;

    /* Determine the width for a sub rectangle. */
    int width = (int) p.getDestX( offsetX + subDistance ) - offset_x;

    /* Calculate the values for each sub rectangle. */
    LinkedList<Double> values = new LinkedList<Double>();

    for( int i = 0; i <= NUMBER_SUBS; i++ )
      values.add( subDistance * i );

    /* Draw the scale with the evaluated unit, width and values. */
    paintScale( g, offset_x, offset_y, determineUnit( values ), values, width, scale );
  }

  /**
   * This function will round the given distance to a specific value.
   * 
   * @param distance
   *            The original distance.
   * @return The modified distance.
   */
  private double round( double distance )
  {
    int n = 0;
    double remaining = distance;
    while( remaining >= 10 )
    {
      remaining = remaining / 10;
      n++;
    }

    if( n > 0 )
      n = n - 1;

    double rest = distance % Math.pow( 10, n );

    return distance - rest;
  }

  /**
   * This function will determine the unit, which would be the best to be used in the scale bar.
   * 
   * @param values
   *            The current values.
   * @return The unit, that should be used.
   */
  private ScaleUnit determineUnit( LinkedList<Double> values )
  {
    /* A one value scale makes no sense. */
    if( values.size() <= 1 )
      return new ScaleUnit( "Meter", 1 );

    /* Need for kilometers? */
    Double firstValue = values.get( 0 );
    if( firstValue.doubleValue() > 1000 )
      return new ScaleUnit( "Kilometer", 1000 );

    Double secondValue = values.get( 1 );
    if( secondValue.doubleValue() > 1000 )
      return new ScaleUnit( "Kilometer", 1000 );

    return new ScaleUnit( "Meter", 1 );
  }

  /**
   * This function paints the scale.
   * 
   * @param g
   *            The graphic context.
   * @param offset_x
   *            Offset from the left border.
   * @param offset_y
   *            Offset from the upper border.
   * @param unit
   *            The unit to be displayed.
   * @param values
   *            The list of scale values. Its size will determine the number of sub rectangles.
   * @param witdh
   *            The width of each sub rectangle. Make sure that the (number of sub recangles * width of a subretangle)
   *            will be smaller than (the width of the graphic context / 2).
   * @param scale
   *            The global scale which represents 1 to xxx.
   */
  public void paintScale( Graphics g, int offset_x, int offset_y, ScaleUnit scaleUnit, LinkedList<Double> values, int width, double scale )
  {
    /* Setup the graphics context. */
    g.setColor( Color.BLACK );

    /* The gap beetween each lines. */
    int GAP = 5;

    /* The number of sub rectangles. */
    int NUMBER_SUB = values.size() - 1;

    /* The width for the sub rectangles. */
    int WIDTH_SUB_RECT = width;

    /* The height for the base and sub rectangles. */
    int HEIGHT_SCALE = 10;

    /* The width of the base rectangle. */
    int WIDTH_SCALE = WIDTH_SUB_RECT * NUMBER_SUB;

    /* The font height. */
    int FONT_HEIGHT = g.getFontMetrics().getHeight();

    int MAX_HEIGHT = HEIGHT_SCALE + 2 * FONT_HEIGHT + 1 * GAP;

    /* Get the clip bounds. */
    Rectangle clipBounds = g.getClipBounds();

    /* Offset to the border of the map in screen pixel. */
    int START_X = offset_x;
    int START_Y = offset_y;

    /* Correct the offset, if negative. */
    if( offset_x < 0 )
      START_X = clipBounds.width - Math.abs( offset_x ) - WIDTH_SCALE;

    if( offset_y < 0 )
      START_Y = clipBounds.height - Math.abs( offset_y ) - MAX_HEIGHT;

    /* Is the draw area too small? */
    if( clipBounds.height < Math.abs( offset_y ) + MAX_HEIGHT )
      return;

    /* Is the draw area too small? */
    if( Math.abs( offset_x ) + (clipBounds.width / 4) < Math.abs( offset_x ) + WIDTH_SCALE )
      return;

    /* The position and bounds of the texts. */
    LinkedList<Rectangle2D> bounds = new LinkedList<Rectangle2D>();
    for( int i = 0; i < values.size(); i++ )
    {
      Rectangle2D stringBounds = g.getFontMetrics().getStringBounds( String.format( "%,.1f%n", values.get( i ) ), g );

      int x = START_X + (i * WIDTH_SUB_RECT) - (int) stringBounds.getWidth() / 2;
      int y = START_Y + MAX_HEIGHT;

      stringBounds.setRect( x, y, stringBounds.getWidth(), stringBounds.getHeight() );

      bounds.add( stringBounds );
    }

    /* Draw the unit name. */
    if( scale > 0 )
    {
      g.drawString( scaleUnit.getName(), START_X, START_Y + FONT_HEIGHT );
      String scaleString = "1:" + String.format( "%,.0f%n", scale );
      int stringWidth = g.getFontMetrics().stringWidth( scaleString );
      g.drawString( scaleString, START_X + WIDTH_SCALE - stringWidth, START_Y + FONT_HEIGHT );
    }
    else
    {
      int stringWidth = g.getFontMetrics().stringWidth( scaleUnit.getName() );
      g.drawString( scaleUnit.getName(), START_X + (WIDTH_SCALE / 2) - (stringWidth / 2), START_Y + FONT_HEIGHT );
    }

    /* Draw the base rectangle. */
    g.drawRect( START_X, START_Y + FONT_HEIGHT + GAP, WIDTH_SCALE, HEIGHT_SCALE );

    /* Draw the sub rectangles. */
    for( int i = 0; i < NUMBER_SUB; i++ )
    {
      /* Only draw (fill) every second sub rectangle. */
      if( i == 0 || i % 2 == 0 )
        g.fillRect( START_X + (i * WIDTH_SUB_RECT), START_Y + FONT_HEIGHT + GAP, WIDTH_SUB_RECT, HEIGHT_SCALE );
    }

    /* Draw the values (values from list). */
    double endPointDrawnText = 0;
    double startPointLastText = bounds.getLast().getX();
    for( int i = 0; i < values.size(); i++ )
    {
      /* Get the positions and dimensions of the text. */
      Rectangle2D stringBounds = bounds.get( i );

      /* The text is not drawn, if it is overlapping the last drawn text or the last text. */
      if( ((stringBounds.getX() > endPointDrawnText) && (stringBounds.getMaxX() < startPointLastText)) || i == values.size() - 1 )
      {
        /* Draw the text. */
        g.drawString( String.format( "%,.1f%n", values.get( i ).doubleValue() / scaleUnit.getFactor() ), (int) stringBounds.getX(), (int) stringBounds.getY() );

        /* Store the last point to see, if the next text is overlapping. */
        endPointDrawnText = stringBounds.getX() + stringBounds.getWidth();
      }
    }
  }
}