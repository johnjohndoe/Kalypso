/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.timezone.test;

import java.text.DateFormat;
import java.text.ParseException;
import java.util.GregorianCalendar;
import java.util.TimeZone;

import com.sun.org.apache.xerces.internal.jaxp.datatype.XMLGregorianCalendarImpl;

/**
 * @author antanas
 */
public class Test
{

  public static void main( String[] args ) throws ParseException
  {
    final String[] winterTimes = new String[] { Messages.Test_0, Messages.Test_1, Messages.Test_2, Messages.Test_3, Messages.Test_4, Messages.Test_5 };
    final String[] summerTimes = new String[] { Messages.Test_6, Messages.Test_7, Messages.Test_8, Messages.Test_9, Messages.Test_10, Messages.Test_11 };
    final GregorianCalendar calendar = new GregorianCalendar();
    final DateFormat formatter = DateFormat.getDateInstance( DateFormat.MEDIUM );

    // calendar.setTimeZone( TimeZone.getTimeZone( "Europe/Berlin" ) );
    // calendar.setTimeZone( TimeZone.getTimeZone( "CET" ) );
    calendar.setTimeZone( TimeZone.getTimeZone( Messages.Test_12 ) );
    // calendar.setTimeZone( TimeZone.getTimeZone( "GMT" ) );
    // calendar.setTimeZone( TimeZone.getTimeZone( "GMT+1" ) );

    // formatter.setTimeZone( TimeZone.getTimeZone( "Europe/Berlin" ) );
    formatter.setTimeZone( TimeZone.getTimeZone( Messages.Test_13 ) );

    System.out.println( Messages.Test_14 );
    for( final String time : winterTimes )
    {
      calendar.setTime( formatter.parse( time ) );
      final XMLGregorianCalendarImpl xmlCalendar = new XMLGregorianCalendarImpl( calendar );
      System.out.println( time + Messages.Test_15 + formatter.getTimeZone().getDisplayName() + Messages.Test_16 + xmlCalendar.toString() + Messages.Test_17 + xmlCalendar.getTimezone() + Messages.Test_18 );
    }

    System.out.println( Messages.Test_19 );
    for( final String time : summerTimes )
    {
      calendar.setTime( formatter.parse( time ) );
      final XMLGregorianCalendarImpl xmlCalendar = new XMLGregorianCalendarImpl( calendar );
      System.out.println( time + Messages.Test_20 + formatter.getTimeZone().getDisplayName() + Messages.Test_21 + xmlCalendar.toString() + Messages.Test_22 + xmlCalendar.getTimezone() + Messages.Test_23 );
    }
  }

}
