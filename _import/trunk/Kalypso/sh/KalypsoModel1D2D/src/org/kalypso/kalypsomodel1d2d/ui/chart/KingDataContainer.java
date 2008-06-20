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
package org.kalypso.kalypsomodel1d2d.ui.chart;

import java.math.BigDecimal;

import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IKingFlowRelation;
import org.kalypsodeegree.model.feature.Feature;

import de.openali.odysseus.chart.framework.model.data.IDataContainer;
import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.data.impl.DataRange;

/**
 * @author burtscher1
 * 
 */
public class KingDataContainer implements IDataContainer<Number, Number>
{
  private final IKingFlowRelation m_kingRelation;

  public KingDataContainer( final Feature kingFeature )
  {
    m_kingRelation = (IKingFlowRelation) kingFeature.getAdapter( IKingFlowRelation.class );
  }

  /**
   * @see org.kalypso.chart.framework.model.data.IDataContainer#close()
   */
  public void close( )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.chart.framework.model.data.IDataContainer#getDomainRange()
   */
  public IDataRange<Number> getDomainRange( )
  {
    final BigDecimal width = m_kingRelation.getWidth();
    final BigDecimal widthStorage = m_kingRelation.getWidthStorage();

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
   * @see org.kalypso.chart.framework.model.data.IDataContainer#getDomainValues()
   */
  public Number[] getDomainValues( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.chart.framework.model.data.IDataContainer#getTargetRange()
   */
  public IDataRange<Number> getTargetRange( )
  {
    final double height = 10 * 1.10;

    IDataRange<Number> dr = null;
    dr = new DataRange<Number>( -1.0, height );
    return dr;
  }

  /**
   * @see org.kalypso.chart.framework.model.data.IDataContainer#getTargetValues()
   */
  public Number[] getTargetValues( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.chart.framework.model.data.IDataContainer#isOpen()
   */
  public boolean isOpen( )
  {
    // TODO Auto-generated method stub
    return false;
  }

  /**
   * @see org.kalypso.chart.framework.model.data.IDataContainer#open()
   */
  public void open( )
  {
    // TODO Auto-generated method stub

  }

  public IKingFlowRelation getKingRelation( )
  {
    return m_kingRelation;
  }

}
