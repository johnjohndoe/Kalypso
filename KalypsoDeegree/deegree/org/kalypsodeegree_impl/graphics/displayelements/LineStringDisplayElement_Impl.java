/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always.
 * 
 * If you intend to use this software in other ways than in kalypso
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree,
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.graphics.displayelements;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.io.Serializable;

import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.displayelements.LineStringDisplayElement;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree_impl.graphics.sld.LineSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.Symbolizer_Impl.UOM;
import org.kalypsodeegree_impl.graphics.sld.awt.StrokePainter;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * DisplayElement that encapsulates a linestring (<tt>GM_Curve</tt>) or multi-linestring geometry (
 * <tt>GM_MultiCurve</tt>) and a <tt>LineStringSymbolizer</tt>.
 * <p>
 * It can be rendered using a solid stroke or a graphics stroke.
 * <p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
class LineStringDisplayElement_Impl extends GeometryDisplayElement_Impl implements LineStringDisplayElement, Serializable
{
  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = -4657962592230618248L;

  /**
   * Creates a new LineStringDisplayElement_Impl object.
   */
  protected LineStringDisplayElement_Impl( final Feature feature, final GM_Curve[] curves )
  {
    this( feature, curves, new LineSymbolizer_Impl() );
  }

  /**
   * Creates a new LineStringDisplayElement_Impl object.
   */
  protected LineStringDisplayElement_Impl( final Feature feature, final GM_Curve[] curves, final LineSymbolizer symbolizer )
  {
    super( feature, curves, symbolizer );
  }

  /**
   * renders the DisplayElement to the submitted graphic context
   */
  @Override
  public void paint( final Graphics g, final GeoTransform projection, final IProgressMonitor monitor )
  {
    Debug.debugMethodBegin( this, "paint" );

    final Graphics2D g2 = (Graphics2D) g;

    final LineSymbolizer sym = (LineSymbolizer) getSymbolizer();
    final Stroke stroke = sym.getStroke();
    final UOM uom = sym.getUom();

    // no stroke defined -> don't draw anything
    if( stroke == null )
      return;

    // GraphicStroke label
    final GM_Curve[] curves = (GM_Curve[]) getGeometry();

    try
    {
      final StrokePainter painter = new StrokePainter( stroke, getFeature(), uom, projection );

      for( final GM_Curve curve : curves )
        paintCurve( g2, projection, curve, painter );
    }
    catch( final FilterEvaluationException e )
    {
      e.printStackTrace();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    Debug.debugMethodEnd();
  }

  private void paintCurve( final Graphics2D g2, final GeoTransform projection, final GM_Curve curve, final StrokePainter painter ) throws GM_Exception
  {
    final int[][] pos = LabelFactory.calcScreenCoordinates( projection, curve );
    painter.paintPoses( g2, pos );
    painter.paintAdditionals( g2, curve, pos );
  }

  public double getDistance( final double x1, final double y1, final double x2, final double y2 )
  {
    final double dx = x2 - x1;
    final double dy = y2 - y1;
    return Math.sqrt( dx * dx + dy * dy );
  }
}