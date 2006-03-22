/*
 * --------------- Kalypso-Header --------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ------------------------------------------------------------------------------------
 */
package org.kalypso.contribs.eclipse.swt.widgets;

import java.text.DateFormat;
import java.text.ParseException;
import java.util.Date;
import java.util.Properties;

import org.eclipse.jface.dialogs.IDialogSettings;

/**
 * A simple structure containing initialisation parameters for the DateRangeInputControl control.
 * 
 * @author schlienger (20.05.2005)
 */
public class DateRangeInputControlStuct
{
  public boolean useRange;

  public Date from;

  public Date to;

  public int days;

  public DateFormat df;

  public DateRangeInputControlStuct( final boolean bUseRange, final Date dFrom, final Date dTo, final int iDays,
      final DateFormat dformat )
  {
    useRange = bUseRange;
    from = dFrom;
    to = dTo;
    days = iDays;
    df = dformat;
  }

  public static DateRangeInputControlStuct create( final IDialogSettings settings, final DateFormat df )
  {
    try
    {
      final boolean useRange = settings.getBoolean( DateRangeInputControl.USE_RANGE );

      final int days = settings.getInt( DateRangeInputControl.NUMBER_OF_DAYS );

      final String sFrom = settings.get( DateRangeInputControl.DATE_FROM );
      Date from = null;
      try
      {
        from = df.parse( sFrom );
      }
      catch( final ParseException e )
      {
        e.printStackTrace();
        from = new Date();
      }

      final String sTo = settings.get( DateRangeInputControl.DATE_TO );
      Date to = null;
      try
      {
        to = df.parse( sTo );
      }
      catch( final ParseException e )
      {
        e.printStackTrace();
        to = new Date();
      }

      return new DateRangeInputControlStuct( useRange, from, to, days, df );
    }
    catch( final Exception e )
    {
      return new DateRangeInputControlStuct( false, new Date(), new Date(), 7, df );
    }
  }

  public static DateRangeInputControlStuct create( final Properties props, final DateFormat df )
  {
    try
    {
      final String sUseRange = props.getProperty( DateRangeInputControl.USE_RANGE );
      final boolean useRange = Boolean.valueOf( sUseRange ).booleanValue();

      final String sDays = props.getProperty( DateRangeInputControl.NUMBER_OF_DAYS );
      final int days = Integer.valueOf( sDays ).intValue();

      final String sFrom = props.getProperty( DateRangeInputControl.DATE_FROM );
      Date from = null;
      try
      {
        from = df.parse( sFrom );
      }
      catch( final ParseException e )
      {
        e.printStackTrace();
        from = new Date();
      }

      final String sTo = props.getProperty( DateRangeInputControl.DATE_TO );
      Date to = null;
      try
      {
        to = df.parse( sTo );
      }
      catch( final ParseException e )
      {
        e.printStackTrace();
        to = new Date();
      }

      return new DateRangeInputControlStuct( useRange, from, to, days, df );
    }
    catch( final Exception e )
    {
      return new DateRangeInputControlStuct( false, new Date(), new Date(), 7, df );
    }
  }

  public void save( final IDialogSettings settings )
  {
    settings.put( DateRangeInputControl.USE_RANGE, useRange );
    settings.put( DateRangeInputControl.NUMBER_OF_DAYS, days );
    settings.put( DateRangeInputControl.DATE_FROM, df.format( from ) );
    settings.put( DateRangeInputControl.DATE_TO, df.format( to ) );
  }

  public void save( final Properties props )
  {
    props.setProperty( DateRangeInputControl.USE_RANGE, String.valueOf( useRange ) );
    props.setProperty( DateRangeInputControl.DATE_FROM, df.format( from ) );
    props.setProperty( DateRangeInputControl.DATE_TO, df.format( to ) );
    props.setProperty( DateRangeInputControl.NUMBER_OF_DAYS, String.valueOf( days ) );
  }
}