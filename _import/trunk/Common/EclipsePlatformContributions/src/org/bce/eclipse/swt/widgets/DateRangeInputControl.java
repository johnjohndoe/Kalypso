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
package org.bce.eclipse.swt.widgets;

import java.text.DateFormat;
import java.text.ParseException;
import java.util.Date;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;

/**
 * A control for entering a date-range either in the form of a number of days or
 * using a range.
 * 
 * @author schlienger
 */
public class DateRangeInputControl extends Composite
{
  /** Flag whether to use the date range or the number of days */
  public final static String USE_RANGE = "kalypso.repository.use_range";

  /** Number of days used in the date-range */
  public final static String NUMBER_OF_DAYS = "kalypso.repository.number_of_days";

  /** Date-from for the preview */
  public final static String DATE_FROM = "kalypso.repository.date_from";

  /** Date-to for the preview */
  public final static String DATE_TO = "kalypso.repository.date_to";

  protected boolean m_useRange;

  protected int m_days;

  protected Date m_from;

  protected Date m_to;

  protected final Button m_btnUseDays;

  protected final Button m_btnUseRange;

  protected final Text m_txtDays;

  protected final Text m_txtFrom;

  protected final Text m_txtTo;

  private final DateFormat m_df;

  private final SelListener m_selListener = new SelListener();

  public DateRangeInputControl( final Composite parent, final int style,
      final DateRangeInputControlStuct struct )
  {
    this( parent, style, struct.useRange, struct.from, struct.to, struct.days,
        struct.df );
  }

  public DateRangeInputControl( final Composite parent, final int style,
      boolean useRange, Date from, Date to, int days, DateFormat df )
  {
    super( parent, style );

    m_df = df;

    final GridLayout gridLayout = new GridLayout( 3, false );
    setLayout( gridLayout );
    setLayoutData( new GridData( GridData.FILL_BOTH ) );

    m_btnUseDays = new Button( this, SWT.RADIO );
    m_btnUseDays.setText( "Tagesanzahl:" );
    m_btnUseDays
        .setToolTipText( "Anzahl der Tagen (0 = ganzer verfügbarer Zeitraum)" );

    m_txtDays = new Text( this, SWT.RIGHT | SWT.BORDER );
    m_txtDays.setText( String.valueOf( days ) );
    m_txtDays.setSize( 100, m_txtDays.getSize().y );
    final GridData gd = new GridData();
    gd.horizontalAlignment = GridData.FILL;
    gd.horizontalSpan = 2;
    m_txtDays.setLayoutData( gd );
    m_txtDays.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        try
        {
          m_days = Integer.parseInt( m_txtDays.getText() );
        }
        catch( final NumberFormatException ex )
        {
          m_days = -1;
        }
      }
    } );

    m_btnUseRange = new Button( this, SWT.RADIO );
    m_btnUseRange.setText( "Zeitraum:" );
    m_btnUseRange
        .setToolTipText( "Eingabe in der From [Von-Bis] (Datum-Beispiel: "
            + m_df.format( new Date() ) + ")" );

    m_txtFrom = new Text( this, SWT.LEFT | SWT.BORDER );
    m_txtFrom.setSize( 50, m_txtFrom.getSize().y );
    m_txtFrom.setText( m_df.format( from ) );
    m_txtFrom.addModifyListener( new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        m_from = parseForDate( m_txtFrom.getText() );
      }
    } );

    m_txtTo = new Text( this, SWT.LEFT | SWT.BORDER );
    m_txtTo.setSize( 50, m_txtTo.getSize().y );
    m_txtTo.setText( m_df.format( to ) );
    m_txtTo.addModifyListener( new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        m_to = parseForDate( m_txtTo.getText() );
      }
    } );

    m_btnUseDays.addSelectionListener( m_selListener );
    m_btnUseRange.addSelectionListener( m_selListener );

    m_btnUseDays.setSelection( !useRange );
    m_btnUseRange.setSelection( useRange );
    // don't know why, but SelectionListener does
    // not trigger, we need to make it explicitely
    m_selListener.explicitSelect();

    // update members
    m_useRange = useRange;
    m_from = from;
    m_to = to;
    m_days = days;
  }

  public void dispose()
  {
    m_btnUseDays.removeSelectionListener( m_selListener );
    m_btnUseRange.removeSelectionListener( m_selListener );

    super.dispose();
  }

  /**
   * Validate the input and return a non-null string if input isn't correct. In
   * the case of an invalid input, the returned string contains a description of
   * the problem
   */
  public String validateInput()
  {
    if( isUseRange() )
    {
      if( getDateFrom() == null )
        return "Die Eingabe Datum-Von <" + m_txtFrom.getText()
            + "> ist nicht gültig.";

      if( getDateTo() == null )
        return "Die Eingabe Datum-Bis <" + m_txtTo.getText()
            + "> ist nicht gültig.";
    }
    else
    {
      if( getNumberOfDays() < 0 )
        return "Die Eingabe Tagesanzahl <" + m_txtDays.getText()
            + "> ist nicht gültig.";
    }

    return null;
  }

  public boolean isUseRange()
  {
    return m_useRange;
  }

  public Date getDateFrom()
  {
    return m_from;
  }

  public Date getDateTo()
  {
    return m_to;
  }

  public int getNumberOfDays()
  {
    return m_days;
  }

  public DateRangeInputControlStuct getStruct()
  {
    return new DateRangeInputControlStuct( isUseRange(), getDateFrom(),
        getDateTo(), getNumberOfDays(), m_df );
  }

  /**
   * Helper: parses the given string into a date. If a ParseException occurs, it
   * returns null.
   * 
   * @return new Date or null if ParseException occured.
   */
  protected Date parseForDate( final String str )
  {
    try
    {
      return m_df.parse( str );
    }
    catch( ParseException e )
    {
      return null;
    }
  }

  private class SelListener implements SelectionListener
  {
    public void explicitSelect()
    {
      final boolean br = m_btnUseRange.getSelection();

      m_txtDays.setEnabled( !br );
      m_txtFrom.setEnabled( br );
      m_txtTo.setEnabled( br );

      m_useRange = br;
    }

    public void widgetSelected( final SelectionEvent e )
    {
      if( e.widget == m_btnUseRange )
        explicitSelect();
    }

    public void widgetDefaultSelected( final SelectionEvent e )
    {
      // empty
    }
  }
}