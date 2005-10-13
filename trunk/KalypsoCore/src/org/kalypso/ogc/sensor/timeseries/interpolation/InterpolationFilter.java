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
package org.kalypso.ogc.sensor.timeseries.interpolation;

import java.util.Calendar;
import java.util.Date;

import org.kalypso.commons.math.LinearEquation;
import org.kalypso.commons.math.LinearEquation.SameXValuesException;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.filters.AbstractObservationFilter;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;

/**
 * InterpolationFilter. This is a simple yet tricky interpolation filter. It steps through the time and eventually
 * interpolates the values at t, using the values at t-1 and t+1.
 * <p>
 * This filter can also deal with Kalypso Status Axes. In that case it does not perform an interpolation, but uses the
 * policy defined in KalypsoStatusUtils. When no status is available, it uses the default value for the status provided
 * in the constructor.
 * 
 * @author schlienger
 */
public class InterpolationFilter extends AbstractObservationFilter
{
  private final int m_calField;

  private final int m_amount;

  private final boolean m_fill;

  private final Double m_defValue;

  private final Integer m_defaultStatus;

  private boolean m_fillLastWithValid;

  /**
   * Constructor.
   * 
   * @param calendarField
   *          which field of the date will be used for steping through the timeserie
   * @param amount
   *          amount of time for the step
   * @param forceFill
   *          when true, fills the model with defaultValue when no base value
   * @param defaultValue
   *          default value to use when filling absent values
   * @param defaultStatus
   *          value of the default status when base status is absent or when status-interpolation cannot be proceeded
   * @param fillLastWithValid
   *          when true, the last tupples of the model get the last valid tupple from the original, not the default one
   */
  public InterpolationFilter( final int calendarField, final int amount, final boolean forceFill,
      final double defaultValue, final int defaultStatus, final boolean fillLastWithValid )
  {
    m_calField = calendarField;
    m_amount = amount;
    m_fill = forceFill;
    m_fillLastWithValid = fillLastWithValid;
    m_defaultStatus = new Integer( defaultStatus );
    m_defValue = new Double( defaultValue );
  }

  public InterpolationFilter( final int calendarField, final int amount, final boolean forceFill,
      final double defaultValue, final int defaultStatus )
  {
    this( calendarField, amount, forceFill, defaultValue, defaultStatus, false );
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getValues(org.kalypso.ogc.sensor.request.IRequest)
   */
  public ITuppleModel getValues( final IRequest request ) throws SensorException
  {
    final ITuppleModel values = super.getValues( request );

    DateRange dr = null;
    if( request != null )
      dr = request.getDateRange();

    final IAxis dateAxis = ObservationUtilities.findAxisByClass( values.getAxisList(), Date.class );
    final IAxis[] valueAxes = ObservationUtilities.findAxesByClass( values.getAxisList(), Number.class );

    final SimpleTuppleModel intModel = new SimpleTuppleModel( values.getAxisList() );

    final Calendar cal = Calendar.getInstance();

    if( values.getCount() == 0 )
    {
      // no values, and fill is not set, so return
      if( !m_fill )
        return values;

      // no values but fill is set, generate them
      if( dr != null )
      {
        cal.setTime( dr.getFrom() );

        while( cal.getTime().compareTo( dr.getTo() ) <= 0 )
          fillWithDefault( dateAxis, valueAxes, intModel, cal );

        return intModel;
      }
    }

    if( values.getCount() != 0 )
    {
      final Date begin = (Date)values.getElement( 0, dateAxis );

      Date d1 = null;
      Date d2 = null;
      final double[] v1 = new double[valueAxes.length + 1];
      final double[] v2 = new double[valueAxes.length + 1];

      int startIx = 0;

      // do we need to fill before the begining of the base model?
      if( dr != null && m_fill )
      {
        cal.setTime( dr.getFrom() );
        d1 = cal.getTime();

        for( int i = 0; i < valueAxes.length; i++ )
        {
          final Number nb = (Number)values.getElement( startIx, valueAxes[i] );
          v1[intModel.getPositionFor( valueAxes[i] )] = nb.doubleValue();
        }

        while( cal.getTime().compareTo( begin ) < 0 )
        {
          d1 = cal.getTime();
          fillWithDefault( dateAxis, valueAxes, intModel, cal );
        }
      }
      else
      {
        cal.setTime( begin );

        final Object[] tupple = new Object[valueAxes.length + 1];
        tupple[intModel.getPositionFor( dateAxis )] = cal.getTime();

        for( int i = 0; i < valueAxes.length; i++ )
        {
          final Number nb = (Number)values.getElement( startIx, valueAxes[i] );

          final int pos = intModel.getPositionFor( valueAxes[i] );
          tupple[pos] = nb;
          v1[pos] = nb.doubleValue();
        }

        intModel.addTupple( tupple );

        cal.add( m_calField, m_amount );

        startIx++;

        d1 = cal.getTime();
      }

      final LinearEquation eq = new LinearEquation();

      for( int ix = startIx; ix < values.getCount(); ix++ )
      {
        d2 = (Date)values.getElement( ix, dateAxis );

        for( int ia = 0; ia < valueAxes.length; ia++ )
        {
          final Number nb = (Number)values.getElement( ix, valueAxes[ia] );
          v2[intModel.getPositionFor( valueAxes[ia] )] = nb.doubleValue();
        }

        while( cal.getTime().compareTo( d2 ) <= 0 )
        {
          long ms = cal.getTimeInMillis();

          Object[] tupple = new Object[valueAxes.length + 1];
          tupple[intModel.getPositionFor( dateAxis )] = cal.getTime();

          for( int ia = 0; ia < valueAxes.length; ia++ )
          {
            final int pos = intModel.getPositionFor( valueAxes[ia] );

            if( KalypsoStatusUtils.isStatusAxis( valueAxes[ia] ) )
            {
              // this is the status axis: no interpolation
              tupple[pos] = new Integer( KalypsoStatusUtils.performInterpolation( (int)v1[pos], (int)v2[pos] ) );
            }
            else
            {
              // normal case: perform the interpolation
              try
              {
                eq.setPoints( d1.getTime(), v1[pos], d2.getTime(), v2[pos] );
                tupple[pos] = new Double( eq.computeY( ms ) );
              }
              catch( SameXValuesException e )
              {
                tupple[pos] = new Double( v1[pos] );
              }
            }
          }

          intModel.addTupple( tupple );

          cal.add( m_calField, m_amount );
        }

        d1 = d2;
        System.arraycopy( v2, 0, v1, 0, v2.length );
      }
    }

    // do we need to fill after the end of the base model?
    if( dr != null && m_fill )
    {
      // optionally remember the last interpolated values in order
      // to fill them till the end of the new model
      Object[] lastValidTupple = null;
      if( m_fillLastWithValid )
      {
        final int pos = intModel.getCount() - 1;

        lastValidTupple = new Object[valueAxes.length + 1];
        lastValidTupple[intModel.getPositionFor( dateAxis )] = intModel.getElement( pos, dateAxis );
        for( int i = 0; i < valueAxes.length; i++ )
        {
          if( KalypsoStatusUtils.isStatusAxis( valueAxes[i] ) )
            lastValidTupple[intModel.getPositionFor( valueAxes[i] )] = m_defaultStatus;
          else
            lastValidTupple[intModel.getPositionFor( valueAxes[i] )] = intModel.getElement( pos, valueAxes[i] );
        }
      }
      
      while( cal.getTime().compareTo( dr.getTo() ) <= 0 )
        fillWithDefault( dateAxis, valueAxes, intModel, cal, lastValidTupple );
    }

    return intModel;
  }

  /**
   * Fill the model with default values
   */
  private void fillWithDefault( final IAxis dateAxis, final IAxis[] valueAxes, final SimpleTuppleModel intModel,
      final Calendar cal ) throws SensorException
  {
    fillWithDefault( dateAxis, valueAxes, intModel, cal, null );
  }

  /**
   * Fills the model with default values
   * 
   * @param masterTupple
   *          if not null, the values from this tupple are used instead of the default one
   */
  private void fillWithDefault( final IAxis dateAxis, final IAxis[] valueAxes, final SimpleTuppleModel intModel,
      final Calendar cal, Object[] masterTupple ) throws SensorException
  {
    final Object[] tupple;

    if( masterTupple == null )
    {
      tupple = new Object[valueAxes.length + 1];
      tupple[intModel.getPositionFor( dateAxis )] = cal.getTime();

      for( int i = 0; i < valueAxes.length; i++ )
      {
        final int pos = intModel.getPositionFor( valueAxes[i] );

        if( KalypsoStatusUtils.isStatusAxis( valueAxes[i] ) )
          tupple[pos] = m_defaultStatus;
        else
          tupple[pos] = m_defValue;
      }
    }
    else
    {
      tupple = (Object[])masterTupple.clone();
      tupple[intModel.getPositionFor( dateAxis )] = cal.getTime();
    }

    intModel.addTupple( tupple );

    cal.add( m_calField, m_amount );
  }
}