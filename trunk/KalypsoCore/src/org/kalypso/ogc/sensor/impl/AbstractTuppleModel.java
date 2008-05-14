/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.impl;

import java.util.HashMap;
import java.util.Map;

import org.kalypso.contribs.java.util.DoubleComparator;
import org.kalypso.core.i18n.Messages;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IAxisRange;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;

/**
 * Provides common functionnality:
 * <ul>
 * <li>getPositionFor( IAxis )</li>
 * <li>mapAxisToPos( IAxis, int )</li>
 * <li>getRangeFor( IAxis )</li>
 * </ul>
 * 
 * @author schlienger
 */
public abstract class AbstractTuppleModel implements ITuppleModel
{
  /** maps an axis to its position in this tupple model */
  private final Map<IAxis, Integer> m_axes2pos = new HashMap<IAxis, Integer>();

  private final IAxis[] m_axes;

  public AbstractTuppleModel( final IAxis[] axes )
  {
    for( int ia = 0; ia < axes.length; ia++ )
      mapAxisToPos( axes[ia], ia );

    m_axes = axes;
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getAxisList()
   */
  public IAxis[] getAxisList( )
  {
    return m_axes;
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getPositionFor(org.kalypso.ogc.sensor.IAxis)
   */
  public int getPositionFor( final IAxis axis ) throws SensorException
  {
    if( !m_axes2pos.containsKey( axis ) )
      throw new SensorException( Messages.getString("org.kalypso.ogc.sensor.impl.AbstractTuppleModel.0") + axis ); //$NON-NLS-1$

    return m_axes2pos.get( axis ).intValue();
  }

  /**
   * Maps an axis to its position in this model
   */
  protected void mapAxisToPos( final IAxis axis, final int pos )
  {
    m_axes2pos.put( axis, new Integer( pos ) );
  }

  /**
   * Clears the maps
   */
  protected void clearAxesPositions( )
  {
    m_axes2pos.clear();
  }

  /**
   * This needs refactoring... which might be undertaken once the whole IObservation and ITuppleModel stuff is
   * refactored.
   * <p>
   * The assuption here that the order is already ok is tricky. And the case where the axis denotes numbers is not so
   * nice, even not really performant.
   * 
   * @see org.kalypso.ogc.sensor.ITuppleModel#getRangeFor(org.kalypso.ogc.sensor.IAxis)
   */
  public IAxisRange getRangeFor( final IAxis axis ) throws SensorException
  {
    if( getCount() > 0 )
    {
      // for numbers we need to step through all the
      // rows in order to find the range
      if( Number.class.isAssignableFrom( axis.getDataClass() ) )
      {
        Number lower = new Double( Double.MAX_VALUE );
        Number upper = new Double( Double.MIN_VALUE );

        final DoubleComparator dc = new DoubleComparator( 0.000001 );
        for( int i = 0; i < getCount(); i++ )
        {
          final Number value = (Number) getElement( i, axis );

          if( dc.compare( value, lower ) < 0 )
            lower = value;

          if( dc.compare( value, upper ) > 0 )
            upper = value;
        }

        return new DefaultAxisRange( lower, upper );
      }

      // else we assume that the order is already correct
      // and simply take the first and the last element
      Object begin = getElement( 0, axis );
      Object end = getElement( getCount() - 1, axis );

      return new DefaultAxisRange( begin, end );
    }

    return null;
  }
}
