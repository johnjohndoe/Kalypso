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
package org.kalypso.ogc.sensor.zml.values;

import java.util.Map;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.AbstractTuppleModel;

/**
 * A specific TuppleModel that can deal with values coming from Zml-Files.
 * 
 * @author schlienger
 */
public class ZmlTuppleModel extends AbstractTuppleModel
{
  private final Map<IAxis, IZmlValues> m_valuesMap;

  /**
   * Constructor
   * 
   * @param valuesMap
   */
  public ZmlTuppleModel( final Map<IAxis, IZmlValues> valuesMap )
  {
    super( valuesMap.keySet().toArray( new IAxis[0] ) );

    m_valuesMap = valuesMap;
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getCount()
   */
  public int getCount( ) throws SensorException
  {
    if( m_valuesMap.size() == 0 )
      return 0;

    return m_valuesMap.values().iterator().next().getCount();
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#indexOf(java.lang.Object, org.kalypso.ogc.sensor.IAxis)
   */
  public int indexOf( final Object element, final IAxis axis ) throws SensorException
  {
    if( m_valuesMap.size() == 0 )
      throw new IllegalStateException( "No Axis" );

    final IZmlValues values = m_valuesMap.get( axis );
    if( values == null )
      return -1;

    return values.indexOf( element );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getElement(int, org.kalypso.ogc.sensor.IAxis)
   */
  public Object getElement( final int index, final IAxis axis ) throws SensorException
  {
    if( m_valuesMap.size() == 0 )
      throw new IllegalStateException( "No Axis" );

    final IZmlValues values = m_valuesMap.get( axis );
    if( values == null )
      return new Double( 0 );

    return values.getElement( index );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#setElement(int, java.lang.Object, org.kalypso.ogc.sensor.IAxis)
   */
  public void setElement( final int index, final Object element, final IAxis axis ) throws SensorException
  {
    if( m_valuesMap.size() == 0 )
      throw new IllegalStateException( "No Axis" );

    m_valuesMap.get( axis ).setElement( index, element );
  }
}