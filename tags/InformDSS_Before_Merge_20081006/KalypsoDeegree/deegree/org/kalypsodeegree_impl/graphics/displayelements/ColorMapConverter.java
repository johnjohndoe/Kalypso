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
package org.kalypsodeegree_impl.graphics.displayelements;

import java.util.LinkedList;
import java.util.List;

import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.LineColorMapEntry;
import org.kalypsodeegree.graphics.sld.PolygonColorMapEntry;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.graphics.sld.LineColorMap;
import org.kalypsodeegree_impl.graphics.sld.PolygonColorMap;
import org.kalypsodeegree_impl.graphics.sld.Symbolizer_Impl.UOM;
import org.kalypsodeegree_impl.graphics.sld.awt.FillPainter;
import org.kalypsodeegree_impl.graphics.sld.awt.StrokePainter;

/**
 * Converts a LineColorMap or a PolygonColorMap into an ElevationColorModel TODO: zwei klassen draus machen, f�r
 * isolines und isofl�chen!
 * 
 * @author Thomas Jung
 */
public class ColorMapConverter
{
  private final List<ColorMapConverterData> m_lister = new LinkedList<ColorMapConverterData>();

  public ColorMapConverter( final LineColorMap colorMap, final Feature feature, final UOM uom, final GeoTransform projection )
  {
    try
    {
      convertLineColorMap( colorMap, feature, uom, projection );
    }
    catch( final FilterEvaluationException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  public ColorMapConverter( final PolygonColorMap colorMap, final Feature feature, final UOM uom, final GeoTransform projection )
  {
    try
    {
      convertPolygonColorMap( colorMap, feature, uom, projection );
    }
    catch( final FilterEvaluationException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  private void convertLineColorMap( final LineColorMap colorMap, final Feature feature, final UOM uom, final GeoTransform projection ) throws FilterEvaluationException
  {
    final LineColorMapEntry[] entries = colorMap.getColorMap();
    for( final LineColorMapEntry element : entries )
    {
      final Stroke stroke = element.getStroke();
      final String label = element.getLabel( feature );
      final double quantity = element.getQuantity( feature );
      final ColorMapConverterData data = new ColorMapConverterData( stroke, feature, uom, projection, label, quantity );
      m_lister.add( data );
    }

  }

  private void convertPolygonColorMap( final PolygonColorMap colorMap, final Feature feature, final UOM uom, final GeoTransform projection ) throws FilterEvaluationException
  {
    final PolygonColorMapEntry[] entries = colorMap.getColorMap();
    for( final PolygonColorMapEntry element : entries )
    {
      final Fill fill = element.getFill();
      final Stroke stroke = element.getStroke();
      final String label = element.getLabel( feature );
      final double from = element.getFrom( feature );
      final double to = element.getTo( feature );
      final ColorMapConverterData data = new ColorMapConverterData( fill, stroke, feature, uom, projection, label, from, to );
      m_lister.add( data );
    }
  }

  /**
   * @see org.kalypsodeegree_impl.graphics.displayelements.IElevationColorModel#getNumOfClasses()
   */
  public int getNumOfClasses( )
  {
    return m_lister.size();
  }

  /**
   * @see org.kalypsodeegree_impl.graphics.displayelements.IElevationColorModel#getFrom(int)
   */
  public double getFrom( final int currentClass )
  {
    return m_lister.get( currentClass ).getFrom();
  }

  /**
   * @see org.kalypsodeegree_impl.graphics.displayelements.IElevationColorModel#getTo(int)
   */
  public double getTo( final int currentClass )
  {
    return m_lister.get( currentClass ).getTo();
  }

  /**
   * @see org.kalypsodeegree_impl.graphics.displayelements.IElevationColorModel#getClassValue(int)
   */
  public double getClassValue( final int currentClass )
  {
    return m_lister.get( currentClass ).getQuantity();
  }

  /**
   * @see org.kalypsodeegree_impl.graphics.displayelements.IElevationColorModel#getPainter(int)
   */
  public StrokePainter getLinePainter( final int currentClass )
  {
    return m_lister.get( currentClass ).getLinePainter();
  }

  /**
   * @see org.kalypsodeegree_impl.graphics.displayelements.IElevationColorModel#getFillPolygonPainter(int)
   */
  public FillPainter getFillPolygonPainter( final int currentClass )
  {
    return m_lister.get( currentClass ).getPolygonPainter();
  }

}
