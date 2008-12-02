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

import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.graphics.sld.Symbolizer_Impl.UOM;
import org.kalypsodeegree_impl.graphics.sld.awt.FillPainter;
import org.kalypsodeegree_impl.graphics.sld.awt.StrokePainter;

/**
 * @author Thomasd Jung
 */
public class ColorMapConverterData
{
  private final String m_label;

  private double m_quantity;

  private double m_from;

  private double m_to;

  private double m_classValueList;

  private final StrokePainter m_linePainter;

  private final FillPainter m_polygonPainter;

  public ColorMapConverterData( final Stroke stroke, final Feature feature, final UOM uom, final GeoTransform projection, final String label, final double quantity ) throws FilterEvaluationException
  {
    m_label = label;
    m_quantity = quantity;

    m_polygonPainter = null;
    m_linePainter = new StrokePainter( stroke, feature, uom, projection );
  }

  public ColorMapConverterData( final Fill fill, final Stroke stroke, final Feature feature, final UOM uom, final GeoTransform projection, final String label, final double from, final double to ) throws FilterEvaluationException
  {
    m_label = label;
    m_from = from;
    m_to = to;

    m_linePainter = new StrokePainter( stroke, feature, uom, projection );
    m_polygonPainter = new FillPainter( fill, feature, uom, projection );
  }

  public String getLabel( )
  {
    return m_label;
  }

  public double getQuantity( )
  {
    return m_quantity;
  }

  public double getFrom( )
  {
    return m_from;
  }

  public double getTo( )
  {
    return m_to;
  }

  public double getClassValueList( )
  {
    return m_classValueList;
  }

  public StrokePainter getLinePainter( )
  {
    return m_linePainter;
  }

  public FillPainter getPolygonPainter( )
  {
    return m_polygonPainter;
  }

}
