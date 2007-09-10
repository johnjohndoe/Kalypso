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
package org.kalypso.model.wspm.ui.featureview;

import java.util.ArrayList;

import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.kalypso.chart.framework.model.data.IDataRange;
import org.kalypso.chart.framework.model.layer.AbstractChartLayer;
import org.kalypso.chart.framework.model.mapper.IAxis;
import org.kalypso.chart.framework.model.styles.IStyledElement;
import org.kalypso.chart.framework.model.styles.IStyleConstants.SE_TYPE;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D;
import org.kalypsodeegree_impl.gml.binding.math.PolynomialUtilities;

/**
 * A chart layer which displays Polynomial1D's.
 * <p>
 * All polynomial given in the constructor are displayed according to their validity.
 * </p>
 * 
 * @author Gernot Belger
 */
public class PolynomeChartLayer extends AbstractChartLayer<Number, Number>
{
  private final int m_pixelsPerTick;

  private final boolean m_showPoints;

  /**
   * @param pixelsPerTick:
   *            Determines the resolution how the polynomes are rendered. 1 means: for every pixel in x-diretion, a
   *            polynome value is calculated and rendered.
   */
  public PolynomeChartLayer( final PolynomDataContainer data, final IAxis<Number> domainAxis, final IAxis<Number> valueAxis, final int pixelsPerTick, final boolean showPoints )
  {
    super( domainAxis, valueAxis );
    setDataContainer( data );
    m_pixelsPerTick = pixelsPerTick;
    m_showPoints = showPoints;
  }

  /**
   * @see org.kalypso.swtchart.chart.layer.IChartLayer#drawIcon(org.eclipse.swt.graphics.Image, int, int)
   */
  public void drawIcon( final Image img )
  {
    // TODO Auto-generated method stub
  }

  /**
   * @see org.kalypso.swtchart.chart.layer.IChartLayer#paint(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper,
   *      org.eclipse.swt.graphics.Device)
   */
  @SuppressWarnings("unchecked")
  public void paint( final GCWrapper gc )
  {
    final IDataRange domainRange = getDataContainer().getDomainRange();
    final double min = (Double) domainRange.getMin();
    final double max = (Double) domainRange.getMax();

    final IStyledElement styledLine = getStyle().getElement( SE_TYPE.LINE, 0 );
    final IStyledElement styledPoint = m_showPoints ? getStyle().getElement( SE_TYPE.POINT, 0 ) : null;

    final ArrayList<Point> path = new ArrayList<Point>();

    final IAxis<Number> domainAxis = getDomainAxis();
    final IAxis<Number> targetAxis = getTargetAxis();

    final double logical0 = domainAxis.screenToLogical( 0 ).doubleValue();
    final double logical1 = domainAxis.screenToLogical( 1 ).doubleValue();
    final double logicalPixelWidth = Math.abs( logical0 - logical1 );

    final double tick = logicalPixelWidth * m_pixelsPerTick;

    for( double pos = min; pos < max; pos += tick )
    {
      final IPolynomial1D poly = PolynomialUtilities.getPoly( getDataContainer().getPolyArray(), pos );
      if( poly == null )
        continue;

      final double value = poly.computeResult( pos );

      final int x = domainAxis.logicalToScreen( pos );
      final int y = targetAxis.logicalToScreen( value );
      path.add( new Point( x, y ) );
    }

    styledLine.setPath( path );
    styledLine.paint( gc );

    if( styledPoint != null )
    {
      styledPoint.setPath( path );
      styledPoint.paint( gc );
    }
  }

  @Override
  public PolynomDataContainer getDataContainer( )
  {
    return (PolynomDataContainer) super.getDataContainer();
  }

}
