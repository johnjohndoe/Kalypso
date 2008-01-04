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
package org.kalypso.model.wspm.sobek.core.ui.boundarycondition;

import java.util.Calendar;
import java.util.GregorianCalendar;

import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.vafada.swtcalendar.SWTCalendar;
import org.vafada.swtcalendar.SWTCalendarEvent;
import org.vafada.swtcalendar.SWTCalendarListener;

/**
 * @author kuch
 */
public class DateTimeDialog extends TitleAreaDialog
{

  private GregorianCalendar m_gregorianCalendar;

  private GregorianCalendar m_preSettedDateTime;

  public DateTimeDialog( final Shell parent )
  {
    super( parent );
    setBlockOnOpen( true );
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createContents(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createContents( final Composite parent )
  {
    final Control contents = super.createContents( parent );

    setTitle( "Choose date / time" );
    setMessage( null );

    return contents;
  }

  /**
   * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( final Composite parent )
  {
    final Composite composite = (Composite) super.createDialogArea( parent );
    composite.setLayout( new GridLayout( 2, false ) );
    final GridData data = new GridData( GridData.FILL, GridData.FILL, true, true );
    data.heightHint = 300;
    data.widthHint = 100;

    composite.setLayoutData( data );

    /* date */
    final SWTCalendar calendar = new SWTCalendar( composite, SWT.FLAT );
    calendar.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false, 2, 0 ) );
    if( m_preSettedDateTime != null )
      calendar.setCalendar( m_preSettedDateTime );

    /* time of day */
    final Label lTime = new Label( composite, SWT.NONE );
    lTime.setText( "Time of day" );

    final DateTime time = new DateTime( composite, SWT.TIME );
    time.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    if( m_preSettedDateTime != null )
      preSetDateTime( m_preSettedDateTime, time );

    /* listeners */
    calendar.addSWTCalendarListener( new SWTCalendarListener()
    {

      public void dateChanged( final SWTCalendarEvent event )
      {
        setDateTime( calendar, time );
      }
    } );

    time.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        setDateTime( calendar, time );
      }
    } );

    setDateTime( calendar, time );

    return composite;
  }

  public GregorianCalendar getDateTime( )
  {
    return m_gregorianCalendar;
  }

  private void preSetDateTime( final GregorianCalendar calendar, final DateTime dateTime )
  {
    dateTime.setDay( calendar.get( Calendar.DAY_OF_MONTH ) );
    dateTime.setMonth( calendar.get( Calendar.MONTH ) );
    dateTime.setYear( calendar.get( Calendar.YEAR ) );
    dateTime.setHours( calendar.get( Calendar.HOUR ) );
    dateTime.setMinutes( calendar.get( Calendar.MINUTE ) );
    dateTime.setSeconds( calendar.get( Calendar.SECOND ) );
  }

  public void setDateTime( final GregorianCalendar dateTime )
  {
    m_preSettedDateTime = dateTime;
  }

  protected void setDateTime( final SWTCalendar calendar, final DateTime time )
  {
    final Calendar date = calendar.getCalendar();
    final int day = date.get( Calendar.DAY_OF_MONTH );
    final int month = date.get( Calendar.MONTH );
    final int year = date.get( Calendar.YEAR );

    final int hours = time.getHours();
    final int minutes = time.getMinutes();
    final int seconds = time.getSeconds();

    m_gregorianCalendar = new GregorianCalendar( year, month, day, hours, minutes, seconds );
  }
}
