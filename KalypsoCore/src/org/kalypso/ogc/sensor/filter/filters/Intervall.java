/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.ogc.sensor.filter.filters;

import java.util.Calendar;

import org.kalypso.core.i18n.Messages;

public class Intervall
{
  // DO NOT CHANGE NUMBERING

  // |--other--|
  // |-----this------|
  final public static int STATUS_INTERSECTION_NONE_BEFORE = 0;

  // |--other--|
  // |-----this------|
  final public static int STATUS_INTERSECTION_NONE_AFTER = 15;

  // |--other--|
  // |-----this------|
  final public static int STATUS_INTERSECTION_START = 4;

  // |--other--|
  // |-----this------|
  final public static int STATUS_INTERSECTION_END = 7;

  // |--other--|
  // |-----this------|
  final public static int STATUS_INTERSECTION_INSIDE = 5;

  // |--------other---------|
  // |-----this------|
  final public static int STATUS_INTERSECTION_ARROUND = 6;

  final Calendar m_start;

  final Calendar m_end;

  private int[] m_status;

  private double[] m_value;

  /**
   * @author doemming
   */
  public Intervall( final Calendar start, final Calendar end, final int[] status, final double[] value )
  {
    m_start = (Calendar) start.clone();
    m_end = (Calendar) end.clone();
    m_status = status.clone();
    m_value = value.clone();
  }

  /*
   * @author doemming
   */
  public Intervall( final Calendar start, final Calendar end )
  {
    m_start = (Calendar) start.clone();
    m_end = (Calendar) end.clone();
    m_status = null;
    m_value = null;
  }

  /*
   * @author doemming
   */
  public Intervall( final Calendar start, final Calendar end, final Integer[] status, final Double[] values )
  {
    m_start = start;
    m_end = end;
    m_status = new int[status.length];
    for( int i = 0; i < status.length; i++ )
      m_status[i] = status[i].intValue();
    m_value = new double[values.length];
    for( int i = 0; i < values.length; i++ )
      m_value[i] = values[i].doubleValue();
  }

  public Calendar getEnd( )
  {
    return (Calendar) m_end.clone();
  }

  public Calendar getStart( )
  {
    return (Calendar) m_start.clone();
  }

  public int[] getStatus( )
  {
    return m_status.clone();
  }

  public void setStatus( final int[] status )
  {
    m_status = status.clone();
  }

  public double[] getValue( )
  {
    return m_value.clone();
  }

  public void setValue( final double[] value )
  {
    m_value = value.clone();
  }

  private long getDurationInMillis( )
  {
    return m_end.getTimeInMillis() - m_start.getTimeInMillis();
  }

  public int calcIntersectionMatrix( final Intervall other )
  {
    int result = 0;
    if( getStart().before( other.getStart() ) )
      result |= 1;
    if( getEnd().before( other.getEnd() ) )
      result |= 2;
    if( getStart().before( other.getEnd() ) )
      result |= 4;
    if( getEnd().before( other.getStart() ) )
      result |= 8;
    return result;
  }

  public boolean intersects( final Intervall other )
  {
    final int matrix = calcIntersectionMatrix( other );
    return !(matrix == STATUS_INTERSECTION_NONE_AFTER || matrix == STATUS_INTERSECTION_NONE_BEFORE);
  }

  public Intervall getIntersection( final Intervall other, final int mode )
  {
    final Intervall result;
    final int matrix = calcIntersectionMatrix( other );
    switch( matrix )
    {
      case STATUS_INTERSECTION_START:
        result = new Intervall( getStart(), other.getEnd() );
        break;
      case STATUS_INTERSECTION_END:
        result = new Intervall( other.getStart(), getEnd() );
        break;
      case STATUS_INTERSECTION_INSIDE:
        result = new Intervall( other.getStart(), other.getEnd() );
        break;
      case STATUS_INTERSECTION_ARROUND:
        result = new Intervall( getStart(), getEnd() );
        break;
      case STATUS_INTERSECTION_NONE_BEFORE:
      case STATUS_INTERSECTION_NONE_AFTER:
        return null;
      default:
        return null;
    }
    // calculate intervall values;
    final double[] values = getValue();
    final double[] intervallValues = new double[values.length];
    final double factor = calcFactorIntersect( result, mode );

    for( int i = 0; i < values.length; i++ )
      intervallValues[i] = factor * values[i];
    result.setStatus( getStatus() );
    result.setValue( intervallValues );
    return result;
  }

  public void merge( final Intervall other, final int mode )
  {
    final double factor = calcFactorMerge( other, mode );
    for( int i = 0; i < other.getValue().length; i++ )
      m_value[i] += factor * other.getValue()[i];
    for( int i = 0; i < other.getStatus().length; i++ )
      m_status[i] |= other.getStatus()[i];
  }

  private double calcFactorIntersect( final Intervall other, final int mode )
  {
    switch( mode )
    {
      case IntervallFilter.MODE_SUM:
      {
        /* If target interval length is 0; factor is 0 (the empty sum) */
        final long durationInMillis = getDurationInMillis();
        if( durationInMillis == 0 )
          return 0d;

        return (double) other.getDurationInMillis() / (double) durationInMillis;
      }
      case IntervallFilter.MODE_INTENSITY:
      default:
        return 1d;
    }
  }

  private double calcFactorMerge( final Intervall other, final int mode )
  {
    switch( mode )
    {
      case IntervallFilter.MODE_SUM:
        return 1d;
      case IntervallFilter.MODE_INTENSITY:
      default:
        return (double) other.getDurationInMillis() / (double) getDurationInMillis();
    }
  }

  @Override
  public String toString( )
  {
    final StringBuffer result = new StringBuffer();
    result.append( Messages.getString("org.kalypso.ogc.sensor.filter.filters.Intervall.0") + m_start.getTime().toString() + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    result.append( Messages.getString("org.kalypso.ogc.sensor.filter.filters.Intervall.2") + m_end.getTime().toString() + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    result.append( Messages.getString("org.kalypso.ogc.sensor.filter.filters.Intervall.4") + getDurationInMillis() + Messages.getString("org.kalypso.ogc.sensor.filter.filters.Intervall.5") ); //$NON-NLS-1$ //$NON-NLS-2$
    if( m_value != null )
    {
      result.append( Messages.getString("org.kalypso.ogc.sensor.filter.filters.Intervall.6") ); //$NON-NLS-1$
      for( final double element : m_value )
        result.append( "  " + element ); //$NON-NLS-1$
      result.append( "\n" ); //$NON-NLS-1$
    }
    if( m_status != null )
    {
      result.append( Messages.getString("org.kalypso.ogc.sensor.filter.filters.Intervall.9") ); //$NON-NLS-1$
      for( final int m_statu : m_status )
        result.append( "  " + m_statu ); //$NON-NLS-1$
      result.append( "\n" ); //$NON-NLS-1$
    }
    return result.toString();
  }
}