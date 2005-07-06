/*--------------- Kalypso-Header ------------------------------------------

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

--------------------------------------------------------------------------*/

package org.kalypso.ogc.sensor.view.wq.diagram;

import java.util.Arrays;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.AbstractTuppleModel;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQPair;


public class WQPairTuppleModel extends AbstractTuppleModel
{
  private final WQPair[] m_pairs;
  private final Double[] m_W;
  private final Double[] m_Q;

  public WQPairTuppleModel( final WQPair[] pairs )
  {
    super( new IAxis[]
    {
        TimeserieUtils.createDefaulAxis( TimeserieConstants.TYPE_WATERLEVEL, true ),
        TimeserieUtils.createDefaulAxis( TimeserieConstants.TYPE_RUNOFF, false ) } );

    m_pairs = pairs;
    m_W = new Double[pairs.length];
    m_Q = new Double[pairs.length];
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getCount()
   */
  public int getCount() throws SensorException
  {
    return m_pairs.length;
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getElement(int, org.kalypso.ogc.sensor.IAxis)
   */
  public Object getElement( int index, IAxis axis ) throws SensorException
  {
    int pos = getPositionFor( axis );

    switch( pos )
    {
      case 0:
        if( m_W[index] == null )
          m_W[index] = new Double( m_pairs[index].getW() );
        return m_W[index];

      case 1:
        if( m_Q[index] == null )
          m_Q[index] = new Double( m_pairs[index].getQ() );
        return m_Q[index];

      default:
        return null;
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#setElement(int, java.lang.Object, org.kalypso.ogc.sensor.IAxis)
   */
  public void setElement( int index, Object element, IAxis axis ) throws SensorException
  {
    throw new SensorException( "setElement is currently not supported" );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#indexOf(java.lang.Object, org.kalypso.ogc.sensor.IAxis)
   */
  public int indexOf( Object element, IAxis axis ) throws SensorException
  {
    if( getPositionFor( axis ) == 0 )
      return Arrays.binarySearch( m_W, element );
    else
      return -1;
  }
}