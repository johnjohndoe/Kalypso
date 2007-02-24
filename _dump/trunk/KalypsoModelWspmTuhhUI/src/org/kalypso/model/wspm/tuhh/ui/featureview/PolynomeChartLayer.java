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
package org.kalypso.model.wspm.tuhh.ui.featureview;

import java.util.ArrayList;

import org.eclipse.swt.graphics.Device;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.swtchart.chart.axis.IAxis;
import org.kalypso.swtchart.chart.layer.AbstractChartLayer;
import org.kalypso.swtchart.chart.layer.IChartLayer;
import org.kalypso.swtchart.chart.layer.IDataRange;
import org.kalypso.swtchart.chart.layer.impl.DataRange;
import org.kalypso.swtchart.chart.legend.ILegendItem;
import org.kalypso.swtchart.chart.styles.StyledLine;
import org.kalypso.swtchart.chart.styles.StyledPoint;
import org.kalypso.swtchart.chart.styles.IStyleConstants.SE_TYPE;
import org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D;

/**
 * A chart layer which displays Polynomial1D's.
 * <p>
 * All polynomial given in the constructor are displayed according to their validity.
 * </p>
 * 
 * @author Gernot Belger
 */
public class PolynomeChartLayer extends AbstractChartLayer implements IChartLayer
{
  private final IPolynomial1D[] m_polyArray;

  private final int m_pixelsPerTick;

  private final boolean m_showPoints;

  /**
   * @param pixelsPerTick:
   *          Determines the resolution how the polynomes are rendered. 1 means: for every pixel in x-diretion, a
   *          polynome value is calculated and rendered.
   */
  public PolynomeChartLayer( final IPolynomial1D[] polyArray, final IAxis domainAxis, final IAxis valueAxis, final int pixelsPerTick, final boolean showPoints )
  {
    super( null, null, domainAxis, valueAxis );

    m_polyArray = polyArray;
    m_pixelsPerTick = pixelsPerTick;
    m_showPoints = showPoints;
  }

  /**
   * @see org.kalypso.swtchart.chart.layer.IChartLayer#drawIcon(org.eclipse.swt.graphics.Image, int, int)
   */
  public void drawIcon( final Image img, final int width, final int height )
  {
    // TODO Auto-generated method stub
  }

  /**
   * Calculates the domain range from the domain-ranges of the polynomes. May range from
   * {@link Double#NEGATIVE_INFINITY} to {@link Double#POSITIVE_INFINITY}.
   * 
   * @see org.kalypso.swtchart.chart.layer.IChartLayer#getDomainRange()
   */
  public IDataRange getDomainRange( )
  {
    double min = Double.POSITIVE_INFINITY;
    double max = Double.NEGATIVE_INFINITY;

    for( final IPolynomial1D poly : m_polyArray )
    {
      final double rangeMin = poly.getRangeMin();
      final double rangeMax = poly.getRangeMax();

      /* Real paranoic: make sure min always less than max */
      final double rangeNormMin = Math.min( rangeMin, rangeMax );
      final double rangeNormMax = Math.max( rangeMin, rangeMax );

      if( rangeNormMin < min )
        min = rangeNormMin;

      if( rangeNormMax > max )
        max = rangeNormMax;
    }

    return new DataRange<Double>( min, max );
  }

  /**
   * @see org.kalypso.swtchart.chart.layer.IChartLayer#getValueRange()
   */
  public IDataRange getValueRange( )
  {
    double min = Double.POSITIVE_INFINITY;
    double max = Double.NEGATIVE_INFINITY;

    final IDataRange domainRange = getDomainRange();
    final double domainMin = (Double) domainRange.getMin();
    final double domainMax = (Double) domainRange.getMax();

    // TODO: handle case where domainMin == domainMax

    final int tickCount = m_polyArray.length * 10;
    final double tick = (domainMax - domainMin) / tickCount;

    for( double pos = domainMin; pos < domainMax; pos += tick )
    {
      final IPolynomial1D poly = getPoly( pos );
      if( poly == null )
        continue;

      final double value = poly.computeResult( pos );

      if( value < min )
        min = value;

      if( value > max )
        max = value;
    }

    return new DataRange<Double>( min, max );
  }

  /**
   * @see org.kalypso.swtchart.chart.layer.IChartLayer#paint(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper,
   *      org.eclipse.swt.graphics.Device)
   */
  @SuppressWarnings("unchecked")
  public void paint( final GCWrapper gc, final Device dev )
  {
    final IDataRange domainRange = getDomainRange();
    final double min = (Double) domainRange.getMin();
    final double max = (Double) domainRange.getMax();

    final StyledLine sl = (StyledLine) getStyle().getElement( SE_TYPE.LINE, 0 );
    final StyledPoint sp = m_showPoints ? (StyledPoint) getStyle().getElement( SE_TYPE.POINT, 0 ) : null;

    final ArrayList<Point> path = new ArrayList<Point>();

    final IAxis<Number> domainAxis = getDomainAxis();
    final IAxis<Number> valueAxis = getValueAxis();

    final double logical0 = domainAxis.screenToLogical( 0 ).doubleValue();
    final double logical1 = domainAxis.screenToLogical( 1 ).doubleValue();
    final double logicalPixelWidth = Math.abs( logical0 - logical1 );

    final double tick = logicalPixelWidth * m_pixelsPerTick;

    for( double pos = min; pos < max; pos += tick )
    {
      final IPolynomial1D poly = getPoly( pos );
      if( poly == null )
        continue;

      final double value = poly.computeResult( pos );

      final int x = domainAxis.logicalToScreen( pos );
      final int y = valueAxis.logicalToScreen( value );
      path.add( new Point( x, y ) );
    }

    sl.setPath( path );
    sl.paint( gc, dev );

    if( sp != null )
    {
      sp.setPath( path );
      sp.paint( gc, dev );
    }

  }

  private IPolynomial1D getPoly( final double pos )
  {
    // TODO: potential performance bug. Use binary search to find poly instead
    for( final IPolynomial1D poly : m_polyArray )
    {
      if( poly.getRangeMin() < pos && pos < poly.getRangeMax() )
        return poly;
    }

    return null;
  }

  /**
   * @see org.kalypso.swtchart.chart.layer.IChartLayer#getLegendItem()
   */
  @SuppressWarnings("deprecation")
  public ILegendItem getLegendItem( )
  {
    // TODO Auto-generated method stub
    return null;
  }

}
