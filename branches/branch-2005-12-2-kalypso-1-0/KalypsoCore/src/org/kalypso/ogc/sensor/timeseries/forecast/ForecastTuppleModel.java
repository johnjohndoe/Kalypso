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

import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;

import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.AbstractTuppleModel;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;

/**
 * TODO: die Reihenfolge der models[] sollte von auﬂerhalb der Klasse entschieden werden. Somit kann der "client"
 * entscheiden ob:
 * <ul>
 * <li>die models[] sortiert sind
 * <li>die models[] eine "andere" Reihenfolge haben
 * </ul>
 * <p>
 * TODO Rename class to something like MultiTimeseriesTuppleModel
 * 
 * @author schlienger
 */
public class ForecastTuppleModel extends AbstractTuppleModel
{
  private SimpleTuppleModel m_model;

  /**
   * Constructor
   */
  public ForecastTuppleModel( final ITuppleModel[] models ) throws SensorException
  {
    super( models[0].getAxisList() );

    Date lastDate = DateUtilities.getMinimum();

    m_model = new SimpleTuppleModel( models[0].getAxisList() );

    // let them sort, so order does not matter (TODO siehe Klassenkommentar)
    Arrays.sort( models, new Comparator()
    {
      public boolean equals( Object obj )
      {
        return false;
      }

      public int compare( Object o1, Object o2 )
      {
        ITuppleModel t1 = (ITuppleModel)o1;
        ITuppleModel t2 = (ITuppleModel)o2;
        IAxis t1axis = ObservationUtilities.findAxisByClass( t1.getAxisList(), Date.class );
        IAxis t2axis = ObservationUtilities.findAxisByClass( t2.getAxisList(), Date.class );
        Date d1 = null, d2 = null;
        boolean statusD1 = false, statusD2 = false;
        try
        {
          d1 = (Date)t1.getElement( 0, t1axis );
          if( d1 != null )
            statusD1 = true;
        }
        catch( Exception e )
        {
          // nothing
        }
        try
        {
          d2 = (Date)t2.getElement( 0, t2axis );
          if( d2 != null )
            statusD2 = true;
        }
        catch( Exception e )
        {
          // nothing
        }
        if( !statusD1 && statusD2 )
          return -1;
        if( !statusD1 && !statusD2 )
          return 0;
        if( statusD1 && !statusD2 )
          return 1;
        return d1.compareTo( d2 );
      }
    } );

    for( int i = 0; i < models.length; i++ )
    {
      if( models[i] == null )
        continue;
      final IAxis[] modelAxes = models[i].getAxisList();
      final IAxis[] targetAxes = m_model.getAxisList();
      final IAxis modelDateAxis = ObservationUtilities.findAxisByClass( modelAxes, Date.class );
      final int[] map = ObservationUtilities.getAxisMapping( targetAxes, modelAxes );
      for( int rowIx = 0; rowIx < models[i].getCount(); rowIx++ )
      {
        final Date date = (Date)models[i].getElement( rowIx, modelDateAxis );
        if( date.after( lastDate ) )
        {
          final Object[] targetTupple = new Object[targetAxes.length];

          for( int colIx = 0; colIx < targetAxes.length; colIx++ )
          {
            if( targetAxes[colIx].isPersistable() )
            {
            if( map[colIx] > -1 )
              targetTupple[colIx] = models[i].getElement( rowIx, modelAxes[map[colIx]] );
            }
          }
          //            tupple[m_model.getPositionFor( axes[colIx] )] = models[i].getElement( rowIx, axes[colIx] );
          m_model.addTupple( targetTupple );
          lastDate = date;
        }
      }
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getCount()
   */
  public int getCount()
  {
    return m_model.getCount();
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getElement(int, org.kalypso.ogc.sensor.IAxis)
   */
  public Object getElement( int index, IAxis axis ) throws SensorException
  {
    return m_model.getElement( index, axis );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#setElement(int, java.lang.Object, org.kalypso.ogc.sensor.IAxis)
   */
  public void setElement( int index, Object element, IAxis axis ) throws SensorException
  {
    m_model.setElement( index, element, axis );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#indexOf(java.lang.Object, org.kalypso.ogc.sensor.IAxis)
   */
  public int indexOf( Object element, IAxis axis ) throws SensorException
  {
    return m_model.indexOf( element, axis );
  }
}