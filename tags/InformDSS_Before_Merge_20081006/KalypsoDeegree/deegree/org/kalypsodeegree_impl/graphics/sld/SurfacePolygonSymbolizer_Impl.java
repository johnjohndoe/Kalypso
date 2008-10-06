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
package org.kalypsodeegree_impl.graphics.sld;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.LineAttributes;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.Geometry;
import org.kalypsodeegree.graphics.sld.PolygonColorMapEntry;
import org.kalypsodeegree.graphics.sld.SurfacePolygonSymbolizer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.xml.Marshallable;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * @author Thomas Jung
 */
public class SurfacePolygonSymbolizer_Impl extends Symbolizer_Impl implements SurfacePolygonSymbolizer, Marshallable
{
  private PolygonColorMap m_colorMap;

  /**
   * Creates a new PolygonSymbolizer_Impl object.
   */
  public SurfacePolygonSymbolizer_Impl( )
  {
    this( new PolygonColorMap_Impl(), null, 0, 99E9, UOM.pixel );
  }

  /**
   * constructor initializing the class with the <PolygonSymbolizer>
   */
  public SurfacePolygonSymbolizer_Impl( final PolygonColorMap colorMap, final Geometry geometry, final double min, final double max, final UOM uom )
  {
    super( geometry, uom );
    setColorMap( colorMap );
    setMinScaleDenominator( min );
    setMaxScaleDenominator( max );
  }

  /**
   * @see org.kalypsodeegree_impl.graphics.sld.Symbolizer_Impl#paint(org.eclipse.swt.graphics.GC,
   *      org.kalypsodeegree.model.feature.Feature)
   */
  @Override
  public void paint( final GC gc, final Feature feature ) throws FilterEvaluationException
  {
    final Rectangle clipping = gc.getClipping();

    gc.setForeground( gc.getDevice().getSystemColor( SWT.COLOR_BLACK ) );
    gc.setBackground( gc.getDevice().getSystemColor( SWT.COLOR_WHITE ) );
    gc.setLineAttributes( new LineAttributes( 1 ) );

    /* we draw 2 rects in the colors of the color map and a black rectangle around it */
    final PolygonColorMapEntry[] colorMapEntries = m_colorMap.getColorMap();

    if( colorMapEntries.length == 0 )
      return;

    final Fill startFill = colorMapEntries[0].getFill();
    final double startOpacity = startFill.getOpacity( null );
    final java.awt.Color startColor = startFill.getFill( null );

    final Color fillColorStart = new Color( gc.getDevice(), startColor.getRed(), startColor.getGreen(), startColor.getBlue() );

    final int startAlpha = (int) (startOpacity * 255);
    gc.setAlpha( startAlpha );
    gc.setBackground( fillColorStart );
    gc.fillRectangle( clipping.x, clipping.y, clipping.width - 1, clipping.height / 2 );

    final Fill endFill = colorMapEntries[colorMapEntries.length - 1].getFill();
    final double endOpacity = endFill.getOpacity( null );
    final java.awt.Color endColor = endFill.getFill( null );

    final Color fillColorEnd = new Color( gc.getDevice(), endColor.getRed(), endColor.getGreen(), endColor.getBlue() );

    final int endAlpha = (int) (endOpacity * 255);
    gc.setAlpha( endAlpha );
    gc.setBackground( fillColorEnd );
    gc.fillRectangle( clipping.x, clipping.height / 2, clipping.width - 1, clipping.height - 1 );

    // the black border
    gc.setForeground( gc.getDevice().getSystemColor( SWT.COLOR_BLACK ) );
    gc.drawRectangle( clipping.x, clipping.y, clipping.width - 1, clipping.height - 1 );

    fillColorStart.dispose();
    fillColorEnd.dispose();
  }

  /**
   * @see org.kalypsodeegree.graphics.sld.SurfacePolygonSymbolizer#getColorMap()
   */
  public PolygonColorMap getColorMap( )
  {
    return m_colorMap;
  }

  /**
   * @see org.kalypsodeegree.graphics.sld.SurfacePolygonSymbolizer#setColorMap(org.kalypsodeegree_impl.graphics.sld.PolygonColorMap)
   */
  public void setColorMap( final PolygonColorMap colorMap )
  {
    m_colorMap = colorMap;
  }

  /**
   * Produces a textual representation of this object.
   * 
   * @return the textual representation
   */
  @Override
  public String toString( )
  {
    final StringBuffer sb = new StringBuffer();
    sb.append( "scale constraint:  >=" + getMinScaleDenominator() + " AND <" + getMaxScaleDenominator() + "\n" );
    sb.append( "<sldExt:SurfacePolygonSymbolizer xmlns:sldExt=\"" + SLDFactory.SLDNS_EXT + "\"" );

    final UOM uom = getUom();

    if( uom != null )
    {
      sb.append( " uom=\"" + uom.name() + "\">" );
    }
    else
      sb.append( ">\n" );

    if( getGeometry() != null )
    {
      sb.append( getGeometry() ).append( "\n" );
    }

    if( getColorMap() != null )
    {
      sb.append( getColorMap() ).append( "\n" );
    }

    sb.append( "</sldExt:SurfacePolygonSymbolizer>\n" );

    return sb.toString();
  }

  /**
   * exports the content of the PolygonSymbolizer as XML formated String
   * 
   * @return xml representation of the PolygonSymbolizer
   */
  public String exportAsXML( )
  {
    Debug.debugMethodBegin();

    final StringBuffer sb = new StringBuffer( 1000 );

    sb.append( "<SurfacePolygonSymbolizer xmlns:sldExt=\"" + SLDFactory.SLDNS_EXT + "\"" );

    final UOM uom = getUom();

    if( uom != null )
    {
      sb.append( " uom=\"" + uom.name() + "\">" );
    }
    else
      sb.append( ">\n" );

    final Geometry geometry = getGeometry();
    if( geometry != null )
    {
      sb.append( ((Marshallable) geometry).exportAsXML() );
    }
    if( m_colorMap != null )
    {
      sb.append( ((Marshallable) m_colorMap).exportAsXML() );
    }
    sb.append( "</SurfacePolygonSymbolizer>" );

    Debug.debugMethodEnd();
    return sb.toString();
  }

}
