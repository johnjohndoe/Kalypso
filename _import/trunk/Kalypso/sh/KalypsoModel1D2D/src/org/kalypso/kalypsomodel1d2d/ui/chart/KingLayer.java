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

import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Path;
import org.kalypso.chart.framework.exception.ZeroSizeDataRangeException;
import org.kalypso.chart.framework.model.data.IDataRange;
import org.kalypso.chart.framework.model.data.impl.DataRange;
import org.kalypso.chart.framework.model.layer.AbstractChartLayer;
import org.kalypso.chart.framework.model.mapper.IAxis;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;

/**
 * A layer which renders a King-Profile.
 * 
 * @author Gernot Belger
 */
public class KingLayer extends AbstractChartLayer<Number, Number>
{

  public KingLayer( final KingDataContainer data, final IAxis<Number> domAxis, final IAxis<Number> valAxis )
  {
    super( domAxis, valAxis );
    setDataContainer( data );
  }

  /**
   * @see org.kalypso.swtchart.chart.layer.IChartLayer#drawIcon(org.eclipse.swt.graphics.Image, int, int)
   */
  public void drawIcon( final Image img, final int width, final int height )
  {
    // TODO Auto-generated method stub
  }

  /**
   * @see org.kalypso.swtchart.chart.layer.IChartLayer#paint(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper,
   *      org.eclipse.swt.graphics.Device)
   */
  public void paint( final GCWrapper gc )
  {
    final IAxis<Number> domainAxis = getDomainAxis();
    final IAxis<Number> valueAxis = getTargetAxis();

    final Path path = new Path( gc.getDevice() );

    try
    {
      /* Draw the bottom */

      /* Minimum requirement is the width */
      final BigDecimal width = getDataContainer().getKingRelation().getWidth();
      if( width == null )
        return;

      final double widthDouble = width.doubleValue();


      final int startBottom = domainAxis.logicalToScreen( -widthDouble / 2 );
      final int stopBottom = domainAxis.logicalToScreen( widthDouble / 2 );
      final int bottom = valueAxis.logicalToScreen( 0.0 );
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
  public IDataRange<Double> getDomainRange( )
  {
    final BigDecimal width = getDataContainer().getKingRelation().getWidth();
    final BigDecimal widthStorage = getDataContainer().getKingRelation().getWidthStorage();

    double widthProfile = 0.0;
    if( width != null )
      widthProfile += width.doubleValue();
    if( widthStorage != null )
      widthProfile += widthStorage.doubleValue();

    /* 10% insets */
    widthProfile *= 1.10;
    
    IDataRange<Double> dr=null;
    try
    {
      dr = new DataRange<Double>( -widthProfile / 2, widthProfile / 2 );
    }
    catch( ZeroSizeDataRangeException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return dr; 
    
  }

  @Override
  public KingDataContainer getDataContainer()
  {
    return (KingDataContainer) super.getDataContainer();
  }
}
