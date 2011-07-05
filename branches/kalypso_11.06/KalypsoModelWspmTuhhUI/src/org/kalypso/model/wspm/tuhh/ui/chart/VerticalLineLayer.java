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
package org.kalypso.model.wspm.tuhh.ui.chart;

import java.math.BigDecimal;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;

import de.openali.odysseus.chart.ext.base.layer.AbstractLineLayer;
import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.data.impl.DataRange;
import de.openali.odysseus.chart.framework.model.figure.impl.PolylineFigure;
import de.openali.odysseus.chart.framework.model.layer.ILayerProvider;
import de.openali.odysseus.chart.framework.model.mapper.IAxis;
import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.model.style.IPointStyle;
import de.openali.odysseus.chart.framework.model.style.IStyleSet;

/**
 * A vertical line layer.
 * 
 * @author Holger Albert
 */
public class VerticalLineLayer extends AbstractLineLayer
{
  /**
   * A list of values at the domain axis, where the vertical lines should be drawn.
   */
  private final BigDecimal[] m_points;

  /**
   * The constructor.
   * 
   * @param points
   *          A list of values at the domain axis, where the vertical lines should be drawn.
   */
  public VerticalLineLayer( final ILayerProvider provider, final ILineStyle lineStyle, final IPointStyle pointStyle, final BigDecimal[] points )
  {
    super( provider, lineStyle, pointStyle );

    m_points = points;
  }

  /**
   * The constructor.
   * 
   * @param points
   *          A list of values at the domain axis, where the vertical lines should be drawn.
   */
  public VerticalLineLayer( final ILayerProvider provider, final IStyleSet styleSet, final BigDecimal[] points )
  {
    super( provider, styleSet );

    m_points = points;
  }

  /**
   * @see de.openali.odysseus.chart.factory.layer.AbstractChartLayer#paint(org.eclipse.swt.graphics.GC)
   */
  @Override
  public void paint( final GC gc )
  {
    final IAxis domainAxis = getDomainAxis();
    final IAxis targetAxis = getTargetAxis();

    for( final BigDecimal point : m_points )
    {
      final Integer x = domainAxis.numericToScreen( point.doubleValue() );
      final IDataRange<Number> numericRange = targetAxis.getNumericRange();
      final Integer yMin = targetAxis.numericToScreen( numericRange.getMin() );
      final Integer yMax = targetAxis.numericToScreen( numericRange.getMax() );

      final PolylineFigure polylineFigure = getPolylineFigure();
      polylineFigure.setPoints( new Point[] { new Point( x, yMin ), new Point( x, yMax ) } );
      polylineFigure.paint( gc );
    }
  }

  /**
   * @see de.openali.odysseus.chart.factory.layer.AbstractChartLayer#getDomainRange()
   */
  @Override
  public IDataRange<Number> getDomainRange( )
  {
    return new DataRange<Number>( m_points[0].doubleValue() - 0.1, m_points[0].doubleValue() + 0.1 );
  }
}