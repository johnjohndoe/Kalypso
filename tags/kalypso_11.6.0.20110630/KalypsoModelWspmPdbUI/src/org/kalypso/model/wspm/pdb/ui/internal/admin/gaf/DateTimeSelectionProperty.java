/*******************************************************************************
 * Copyright (c) 2009 Matthew Hall and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Matthew Hall - initial API and implementation (bug 169876)
 *     Matthew Hall - bug 271720
 ******************************************************************************/

package org.kalypso.model.wspm.pdb.ui.internal.admin.gaf;

import java.util.Calendar;
import java.util.Date;

import org.eclipse.jface.databinding.swt.WidgetValueProperty;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.DateTime;

/**
 * @since 3.2
 */
public class DateTimeSelectionProperty extends WidgetValueProperty
{
  private final Calendar m_calendar;

  public DateTimeSelectionProperty( final Calendar calendar )
  {
    super( SWT.Selection );

    m_calendar = calendar;
  }

  @Override
  public Object getValueType( )
  {
    return Date.class;
  }

  @Override
  protected Object doGetValue( final Object source )
  {
    final DateTime dateTime = (DateTime) source;

    final Calendar cal = m_calendar;
    // cal.clear();
    if( (dateTime.getStyle() & SWT.TIME) != 0 )
    {
      cal.set( Calendar.HOUR_OF_DAY, dateTime.getHours() );
      cal.set( Calendar.MINUTE, dateTime.getMinutes() );
      cal.set( Calendar.SECOND, dateTime.getSeconds() );
    }
    else
    {
      cal.set( Calendar.YEAR, dateTime.getYear() );
      cal.set( Calendar.MONTH, dateTime.getMonth() );
      cal.set( Calendar.DAY_OF_MONTH, dateTime.getDay() );
    }
    return cal.getTime();
  }

  @Override
  protected void doSetValue( final Object source, final Object value )
  {
    final DateTime dateTime = (DateTime) source;

    if( value == null )
      throw new IllegalArgumentException( "Cannot set null selection on DateTime" ); //$NON-NLS-1$

    final Calendar cal = m_calendar;
    cal.setTime( (Date) value );
    if( (dateTime.getStyle() & SWT.TIME) != 0 )
    {
      dateTime.setTime( cal.get( Calendar.HOUR_OF_DAY ), cal.get( Calendar.MINUTE ), cal.get( Calendar.SECOND ) );
    }
    else
    {
      dateTime.setDate( cal.get( Calendar.YEAR ), cal.get( Calendar.MONTH ), cal.get( Calendar.DAY_OF_MONTH ) );
    }
  }
}
