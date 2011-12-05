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
package org.kalypso.kalypsomodel1d2d.ui.chart;

import java.math.BigDecimal;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Path;

import de.openali.odysseus.chart.factory.layer.AbstractChartLayer;
import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.data.impl.DataRange;
import de.openali.odysseus.chart.framework.model.layer.ILegendEntry;
import de.openali.odysseus.chart.framework.model.mapper.IAxis;

/**
 * A layer which renders a King-Profile.
 * 
 * @author Gernot Belger
 */
public class KingLayer extends AbstractChartLayer
{

  private final KingDataContainer m_data;

  public KingLayer( final KingDataContainer data )
  {
    super( null );

    m_data = data;
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
  @Override
  public void paint( final GC gc )
  {
    final IAxis domainAxis = getDomainAxis();
    final IAxis valueAxis = getTargetAxis();

    final Path path = new Path( gc.getDevice() );

    try
    {
      /* Draw the bottom */

      /* Minimum requirement is the width */
      final BigDecimal width = m_data.getKingRelation().getWidth();
      if( width == null )
        return;

      final double widthDouble = width.doubleValue();

      final int startBottom = domainAxis.numericToScreen( -widthDouble / 2 );
      final int stopBottom = domainAxis.numericToScreen( widthDouble / 2 );
      final int bottom = valueAxis.numericToScreen( 0.0 );
      path.moveTo( startBottom, bottom );
      path.lineTo( stopBottom, bottom );

      gc.drawPath( path );
    }
    finally
    {
      path.dispose();
    }

  }

  /**
   * @see org.kalypso.swtchart.chart.layer.IChartLayer#getDomainRange()
   */
  @Override
  public IDataRange<Number> getDomainRange( )
  {
    final BigDecimal width = m_data.getKingRelation().getWidth();
    final BigDecimal widthStorage = m_data.getKingRelation().getWidthStorage();

    double widthProfile = 0.0;
    if( width != null )
      widthProfile += width.doubleValue();
    if( widthStorage != null )
      widthProfile += widthStorage.doubleValue();

    /* 10% insets */
    widthProfile *= 1.10;

    IDataRange<Number> dr = null;
    dr = new DataRange<Number>( -widthProfile / 2, widthProfile / 2 );
    return dr;

  }

  /**
   * @see de.openali.odysseus.chart.ext.base.layer.AbstractChartLayer#createLegendEntries()
   */
  @Override
  protected ILegendEntry[] createLegendEntries( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IChartLayer#dispose()
   */
  @Override
  public void dispose( )
  {
    // nothing to do

  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IChartLayer#getTargetRange()
   */
  @Override
  public IDataRange<Number> getTargetRange( final IDataRange<Number> range )
  {
    // TODO Auto-generated method stub
    return null;
  }

}
