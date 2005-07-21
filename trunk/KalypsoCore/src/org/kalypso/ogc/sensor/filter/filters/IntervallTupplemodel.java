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
package org.kalypso.ogc.sensor.filter.filters;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.kalypso.contribs.java.util.CalendarIterator;
import org.kalypso.contribs.java.util.CalendarUtilities;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IAxisRange;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.AbstractTuppleModel;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;

/**
 * @author doemming
 */
public class IntervallTupplemodel extends AbstractTuppleModel
{
  private static final int TODO_NOTHING = 0;

  private static final int TODO_GOTO_NEXT_TARGET = 1;

  private static final int TODO_GOTO_NEXT_SRC = 2;

  private static final int TODO_FINISHED = 3;

  private final ITuppleModel m_baseModel;

  private final int m_mode;

  private final IAxis[] m_valueAxis;

  private final IAxis m_dateAxis;

  private final IAxis[] m_statusAxis;

  private final int m_calendarField;

  private final int m_amount;

  private final Calendar m_from;

  private final Calendar m_to;

  private SimpleTuppleModel m_intervallModel;

  public IntervallTupplemodel( int mode, int calendarField, int amount,final int startCalendarValue, final String startCalendarField,
      ITuppleModel baseModel, Date from, Date to )
  {
    super( baseModel.getAxisList() );
    m_mode = mode;
    m_calendarField = calendarField;
    m_amount = amount;
    m_baseModel = baseModel;

    // check axis
    IAxis[] axisList = getAxisList();
    m_dateAxis = ObservationUtilities.findAxisByType( axisList, TimeserieConstants.TYPE_DATE );
    m_statusAxis = KalypsoStatusUtils.findStatusAxes( axisList );
    final List valueAxis = new ArrayList();
    for( int i = 0; i < axisList.length; i++ )
    {
      IAxis axis = axisList[i];
      boolean isValueAxis = true;
      if( axis == m_dateAxis )
        isValueAxis = false;
      for( int j = 0; j < m_statusAxis.length; j++ )
      {
        if( axis == m_statusAxis[j] )
          isValueAxis = false;
      }
      if( isValueAxis )
        valueAxis.add( axis );
    }
    m_valueAxis = (IAxis[])valueAxis.toArray( new IAxis[valueAxis.size()] );
    IAxisRange range = null;
    try
    {
      range = m_baseModel.getRangeFor( m_dateAxis );
    }
    catch( SensorException e )
    {
      // nothing
    }

    // count rows
    if( from != null )
      m_from = getDefaultCalendar( from );
    else
      m_from = getDefaultCalendar( (Date)range.getLower() );

    // correct from
    if( startCalendarField != null && startCalendarField.length()>0)
      m_from.set( CalendarUtilities.getCalendarField(startCalendarField), startCalendarValue );
    if( to != null )
      m_to = getDefaultCalendar( to );
    else
      m_to = getDefaultCalendar( (Date)range.getUpper() );

    try
    {
      initModell();
    }
    catch( SensorException e1 )
    {
      e1.printStackTrace();
    }
  }

  private int countRows()
  {
    int result = 0;
    final Iterator iterator = new CalendarIterator( m_from, m_to, m_calendarField, m_amount );
    while( iterator.hasNext() )
    {
      iterator.next();
      result++;
    }
    return result;
  }

  private void initModell() throws SensorException
  {
    // default values
    final int[] defaultStatus = new int[m_statusAxis.length];
    for( int i = 0; i < defaultStatus.length; i++ )
      defaultStatus[i] = KalypsoStati.BIT_CHECK;
    // CHECK BIT_OK
    final double[] defaultValues = new double[m_valueAxis.length];
    for( int i = 0; i < defaultValues.length; i++ )
      defaultValues[i] = 0d;

    // create empty model
    final IAxis[] axisList = getAxisList();
    int rows = countRows();
    m_intervallModel = new SimpleTuppleModel( axisList, new Object[rows][axisList.length] );

    final Iterator iterator = new CalendarIterator( m_from, m_to, m_calendarField, m_amount );

    // TODO hasnext ?
    Calendar targetCal_last = (Calendar)iterator.next();
    Calendar srcCal_last = targetCal_last;

    Intervall srcIntervall = null;
    Intervall targetIntervall = null;
    int srcRow = 0;
    int targetRow = 0;
    // fill initial row
    final Intervall initialIntervall = new Intervall( m_from, m_from, defaultStatus, defaultValues );
    updateModelfromintervall( m_intervallModel, targetRow, initialIntervall );
    targetRow++;

    int todo = TODO_NOTHING;
    while( todo != TODO_FINISHED )
    {
      // next src intervall ...
      if( srcIntervall == null || todo == TODO_GOTO_NEXT_SRC )
      {
        if( !( srcRow < m_baseModel.getCount() ) )
        {
          // create dummy intervall
          srcIntervall = new Intervall( srcCal_last, m_to, defaultStatus, defaultValues );
          todo = TODO_NOTHING;
          continue;
        }
        final Calendar cal = getDefaultCalendar( (Date)m_baseModel.getElement( srcRow, m_dateAxis ) );
        final Object[] o = ObservationUtilities.getElements( m_baseModel, srcRow, m_statusAxis );
        final Integer[] stati = new Integer[o.length];
        for( int i = 0; i < o.length; i++ )
        {
          if( o[i] instanceof Integer )
            stati[i] = ( (Integer)o[i] );
          if( o[i] instanceof Long ) // TODO when reciving obs from PSI it is a Long
            stati[i] = new Integer( ( (Long)o[i] ).intValue() );
        }

        final Object[] valueOs = ObservationUtilities.getElements( m_baseModel, srcRow, m_valueAxis );
        final Double[] values = new Double[valueOs.length];
        for( int i = 0; i < valueOs.length; i++ )
          values[i] = (Double)valueOs[i];

        // TODO check "&& srcRow > 0"
        if( srcCal_last.before( cal ) && srcRow > 0 )
          srcIntervall = new Intervall( srcCal_last, cal, stati, values );
        else
          srcIntervall = null;
        srcCal_last = cal;
        srcRow++;
        todo = TODO_NOTHING;
      }
      // next target intervall
      if( targetIntervall == null || todo == TODO_GOTO_NEXT_TARGET )
      {
        if( targetIntervall != null )
        {
          updateModelfromintervall( m_intervallModel, targetRow, targetIntervall );
          targetRow++;
        }
        if( !iterator.hasNext() )
        {
          todo = TODO_FINISHED;
          continue;
        }
        final Calendar cal = (Calendar)iterator.next();
        if( targetCal_last.before( cal ) )
          targetIntervall = new Intervall( targetCal_last, cal, defaultStatus, defaultValues );
        else
          targetIntervall = null;
        targetCal_last = cal;
        todo = TODO_NOTHING;
      }
      // check validity of intervalls
      if( srcIntervall == null )
      {
        todo = TODO_GOTO_NEXT_SRC;
        continue;
      }
      if( targetIntervall == null )
      {
        todo = TODO_GOTO_NEXT_TARGET;
        continue;
      }
      // compute intersection intervall
      int matrix = srcIntervall.calcIntersectionMatrix( targetIntervall );
      Intervall intersection = null;
      if( matrix != Intervall.STATUS_INTERSECTION_NONE_BEFORE && matrix != Intervall.STATUS_INTERSECTION_NONE_AFTER )
        intersection = srcIntervall.getIntersection( targetIntervall, m_mode );

      switch( matrix )
      {
      case Intervall.STATUS_INTERSECTION_NONE_BEFORE:
        todo = TODO_GOTO_NEXT_TARGET;
        break;
      case Intervall.STATUS_INTERSECTION_NONE_AFTER:
        todo = TODO_GOTO_NEXT_SRC;
        break;
      case Intervall.STATUS_INTERSECTION_START:
      case Intervall.STATUS_INTERSECTION_INSIDE:
        targetIntervall.merge( intersection, m_mode );
        todo = TODO_GOTO_NEXT_TARGET;
        break;
      case Intervall.STATUS_INTERSECTION_END:
      case Intervall.STATUS_INTERSECTION_ARROUND:
        targetIntervall.merge( intersection, m_mode );
        todo = TODO_GOTO_NEXT_SRC;
        break;
      default:
        break;
      }
    }
  }

  // accept values for result
  private void updateModelfromintervall( ITuppleModel model, int targetRow, Intervall targetIntervall )
      throws SensorException
  {
    final Calendar cal = targetIntervall.getEnd();
    final int[] status = targetIntervall.getStatus();
    final double[] value = targetIntervall.getValue();
    model.setElement( targetRow, cal.getTime(), m_dateAxis );
    for( int i = 0; i < m_statusAxis.length; i++ )
      model.setElement( targetRow, new Integer( status[i] ), m_statusAxis[i] );
    for( int i = 0; i < m_valueAxis.length; i++ )
      model.setElement( targetRow, new Double( value[i] ), m_valueAxis[i] );
  }

  private Calendar getDefaultCalendar( final Date date )
  {
    final Calendar result = Calendar.getInstance();
    result.setTime( date );
    return result;
  }

  public int getCount()
  {
    return m_intervallModel.getCount();
  }

  public int hashCode()
  {
    return m_intervallModel.hashCode();
  }

  public String toString()
  {
    return m_intervallModel.toString();
  }

  public Object getElement( int index, IAxis axis ) throws SensorException
  {
    return m_intervallModel.getElement( index, axis );
  }

  public void setElement( int index, Object element, IAxis axis )
  {
    throw new UnsupportedOperationException( getClass().getName() + " unterstuetzt setElement() nicht." );
    // TODO support it
  }

  public int indexOf( Object element, IAxis axis ) throws SensorException
  {
    if( element instanceof Date )
      return m_baseModel.indexOf( element, axis );
    throw new UnsupportedOperationException( getClass().getName() + " unterstuetzt indexOf fuer die Axe "
        + axis.getName() + " nicht." );
    // TODO support it
  }
}