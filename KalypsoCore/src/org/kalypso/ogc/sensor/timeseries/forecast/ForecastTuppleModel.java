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
package org.kalypso.ogc.sensor.timeseries.forecast;

import java.util.Date;

import org.kalypso.java.util.DateUtilities;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.AbstractTuppleModel;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;

/**
 * MergeTuppleModel
 * 
 * @author schlienger
 */
public class ForecastTuppleModel extends AbstractTuppleModel
{
  private SimpleTuppleModel m_model;

  /**
   * Constructor
   * 
   * @param models
   * 
   * @throws SensorException
   */
  public ForecastTuppleModel( final ITuppleModel[] models )
      throws SensorException
  {
    super( models[0].getAxisList() );
    
    Date lastDate = DateUtilities.getMinimum();

    m_model = new SimpleTuppleModel( models[0].getAxisList() );

    for( int i = 0; i < models.length; i++ )
    {
      final IAxis[] axes = models[i].getAxisList();
      final IAxis[] dateAxes = ObservationUtilities.findAxisByClass( axes,
          Date.class );
      if( dateAxes.length == 0 )
        throw new IllegalArgumentException( "no date axis" );

      final IAxis dateAxis = dateAxes[0];

      for( int rowIx = 0; rowIx < models[i].getCount(); rowIx++ )
      {
        final Date date = (Date) models[i].getElement( rowIx, dateAxis );

        if( date.compareTo( lastDate ) > 0 )
        {
          final Object[] tupple = new Object[axes.length];

          for( int colIx = 0; colIx < axes.length; colIx++ )
            tupple[m_model.getPositionFor( axes[colIx] )] = models[i]
                .getElement( rowIx, axes[colIx] );

          m_model.addTupple( tupple );
        }

        lastDate = date;
      }
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getCount()
   */
  public int getCount( )
  {
    return m_model.getCount();
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getElement(int,
   *      org.kalypso.ogc.sensor.IAxis)
   */
  public Object getElement( int index, IAxis axis ) throws SensorException
  {
    return m_model.getElement( index, axis );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#setElement(int, java.lang.Object,
   *      org.kalypso.ogc.sensor.IAxis)
   */
  public void setElement( int index, Object element, IAxis axis )
      throws SensorException
  {
    m_model.setElement( index, element, axis );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#indexOf(java.lang.Object,
   *      org.kalypso.ogc.sensor.IAxis)
   */
  public int indexOf( Object element, IAxis axis ) throws SensorException
  {
    return m_model.indexOf( element, axis );
  }
}