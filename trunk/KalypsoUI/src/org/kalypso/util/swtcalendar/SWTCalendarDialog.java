package org.kalypso.util.swtcalendar;

import java.util.Calendar;
import java.util.Date;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.vafada.swtcalendar.SWTCalendar;

/**
 * @author belger
 */
public class SWTCalendarDialog extends Dialog
{
  private SWTCalendar m_swtcal;
  private final Calendar m_calendar = Calendar.getInstance();

  public SWTCalendarDialog( final Shell parent, final Date date )
  {
    super( parent );
    
    m_calendar.setTime( date );
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  protected Control createDialogArea( final Composite parent )
  {
    final Composite panel = (Composite)super.createDialogArea( parent );

    panel.getShell().setText( "Datum wählen" );
    
    m_swtcal = new SWTCalendar( panel );
    m_swtcal.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    m_swtcal.setCalendar( m_calendar );
    
    return panel;
  }
  
  public Date getDate()
  {
    return m_swtcal.getCalendar().getTime();
  }
}