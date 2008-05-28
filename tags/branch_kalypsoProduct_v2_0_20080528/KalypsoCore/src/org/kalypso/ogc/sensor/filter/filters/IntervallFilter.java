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

import java.net.URL;
import java.util.Date;

import org.kalypso.contribs.java.util.CalendarUtilities;
import org.kalypso.core.i18n.Messages;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.zml.filters.IntervallFilterType;

/**
 * @author doemming
 */
public class IntervallFilter extends AbstractObservationFilter
{
  public final static int MODE_INTENSITY = 0;

  public final static int MODE_SUM = 1;

  private IObservation m_baseobservation = null;

  private final int m_mode;

  private final int m_calendarField;

  private final int m_amount;

  private final String m_startCalendarField;

  private final int m_startCalendarValue;

  private final double m_defaultValue;

  private final int m_defaultStatus;

  public IntervallFilter( IntervallFilterType filter )
  {
    final String mode = filter.getMode();
    if( "intensity".equalsIgnoreCase( mode ) ) //$NON-NLS-1$
      m_mode = MODE_INTENSITY;
    else if( "sum".equalsIgnoreCase( mode ) ) //$NON-NLS-1$
      m_mode = MODE_SUM;
    else
      m_mode = MODE_INTENSITY; // default is intensity
    m_defaultStatus = filter.getDefaultStatus();
    m_defaultValue = filter.getDefaultValue();
    m_calendarField = CalendarUtilities.getCalendarField( filter.getCalendarField() );
    
    m_amount = filter.getAmount();
    m_startCalendarField = filter.getStartCalendarfield();
    m_startCalendarValue = filter.getStartCalendarvalue();
  }

  @Override
  public void initFilter( Object dummy, IObservation baseObs, URL context ) throws SensorException
  {
    m_baseobservation = baseObs;
    super.initFilter( dummy, baseObs, context );
  }

  @Override
  public ITuppleModel getValues( final IRequest request ) throws SensorException
  {
    final Date from;
    final Date to;
    if( request != null && request.getDateRange() != null )
    {
      from = request.getDateRange().getFrom();
      to = request.getDateRange().getTo();
    }
    else
    {
      from = null;
      to = null;
    }

    return new IntervallTupplemodel( m_mode, m_calendarField, m_amount, m_startCalendarValue, m_startCalendarField,
        m_baseobservation.getValues( request ), from, to, m_defaultValue, m_defaultStatus );

  }

  @Override
  public void setValues( ITuppleModel values )
  {
    throw new UnsupportedOperationException( getClass().getName() + Messages.getString("org.kalypso.ogc.sensor.filter.filters.IntervallFilter.2") ); //$NON-NLS-1$
  }
}