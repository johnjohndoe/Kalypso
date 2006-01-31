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

package org.kalypso.ogc.sensor.request;

import java.util.Date;

import org.apache.commons.lang.StringUtils;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.zml.request.Request;

/**
 * Contains the request information for an observation.
 * 
 * @author schlienger
 */
public class ObservationRequest implements IRequest
{
  private final static String[] EMPTY_STRING_ARRAY = new String[0];

  private final DateRange m_dateRange;

  private final String m_name;

  private final String[] m_axisTypes;

  private final String[] m_axisTypesWithStatus;

  public ObservationRequest()
  {
    this( null );
  }

  public ObservationRequest( final Date from, final Date to )
  {
    this( new DateRange( from, to ) );
  }

  public ObservationRequest( final DateRange dr )
  {
    this( dr, null, EMPTY_STRING_ARRAY, EMPTY_STRING_ARRAY );
  }

  public ObservationRequest( final String name, final String[] axisTypes, final String[] axisTypesWithStatus )
  {
    this( null, name, axisTypes, axisTypesWithStatus );
  }

  public ObservationRequest( final DateRange dr, final String name, final String[] axisTypes,
      final String[] axisTypesWithStatus )
  {
    m_dateRange = dr;
    m_name = name;
    m_axisTypes = axisTypes;
    m_axisTypesWithStatus = axisTypesWithStatus;
  }

  /**
   * @see org.kalypso.ogc.sensor.request.IRequest#getDateRange()
   */
  public DateRange getDateRange()
  {
    return m_dateRange;
  }

  /**
   * @see org.kalypso.ogc.sensor.request.IRequest#getName()
   */
  public String getName()
  {
    return m_name;
  }

  /**
   * @see org.kalypso.ogc.sensor.request.IRequest#getAxisTypes()
   */
  public String[] getAxisTypes()
  {
    return m_axisTypes;
  }

  /**
   * @see org.kalypso.ogc.sensor.request.IRequest#getAxisTypesWithStatus()
   */
  public String[] getAxisTypesWithStatus()
  {
    return m_axisTypesWithStatus;
  }

  /**
   * @see org.kalypso.ogc.sensor.request.IRequest#toString()
   */
  @Override
  public String toString()
  {
    final StringBuffer bf = new StringBuffer();

    if( m_dateRange != null )
      bf.append( "Date-Range: " ).append( m_dateRange.toString() ).append( "\n" );

    if( m_name != null )
      bf.append( "Name: " ).append( m_name ).append( "\n" );

    if( m_axisTypes.length > 0 )
      bf.append( "Axis-Types: " ).append( StringUtils.join( m_axisTypes, ',') ).append( "\n" );

    if( m_axisTypesWithStatus.length > 0 )
      bf.append( "Status for: " ).append( StringUtils.join( m_axisTypesWithStatus, ',') ).append( "\n" );

    return bf.toString();
  }

  public static ObservationRequest createWith( final Request requestType )
  {
    if( requestType == null )
      return new ObservationRequest();

    final DateRange dr;
    final Date from = requestType.getDateFrom() == null ? null : requestType.getDateFrom().getTime();
    final Date to = requestType.getDateTo() == null ? null : requestType.getDateTo().getTime();
    if( from == null && to == null )
      dr = null;
    else
      dr = new DateRange( from, to );

    final String[] axisTypes;
    if( requestType.getAxes() == null )
      axisTypes = EMPTY_STRING_ARRAY;
    else
      axisTypes = StringUtils.split( requestType.getAxes(), ',' );

    final String[] axisTypesWithStatus;
    if( requestType.getStatusAxes() == null )
      axisTypesWithStatus = new String[0];
    else
      axisTypesWithStatus = StringUtils.split( requestType.getStatusAxes(), ',' );

    final String name = requestType.getName() != null ? requestType.getName() : "unbekannt";
    
    return new ObservationRequest( dr, name, axisTypes, axisTypesWithStatus );
  }
}