package org.kalypso.ui.repository.dialogs;

import java.text.DateFormat;
import java.text.ParseException;
import java.util.Date;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.PreferenceStore;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.ui.preferences.IKalypsoPreferences;

/**
 * Config Dialog for Repository-Preview.
 * 
 * @author schlienger
 */
public class DateRangeInputControl
{
  public final static String DESCRIPTION = "Wählen Sie zwischen Tagesanzahl- oder Zeitraumeingabe.\n"
    + "- Tagesanzahl: Anzahl letzte angezeigte Tagen (0 = ganzer Zeitraum)\n"
    + "- Zeitraum: Eingabe-Von und Eingabe-Bis (Beispielformat: 22.01.2000 13:30:00)";

  private final static String errTitle = "Eingabe bitte prufen...";

  private final static String errMsg = "Fehlerhafte Eingabe, ";

  private PreferenceStore m_store;

  private BooleanFieldEditor m_fUseRange;

  private StringFieldEditor m_fDateFrom;

  private StringFieldEditor m_fDateTo;

  private IntegerFieldEditor m_fNumberOfDays;

  private final DateFormat m_df;

  private AbstractUIPlugin m_plugin;

  private final Shell m_parentShell;

  /**
   * Constructor. If plugin is specified, then its DialogSettings are used as
   * default values.
   * 
   * @param parentShell
   * @param useRange
   * @param from
   * @param to
   * @param days
   * @param df
   * @param plugin
   */
  public DateRangeInputControl( final Shell parentShell, boolean useRange,
      Date from, Date to, int days, final DateFormat df,
      final AbstractUIPlugin plugin )
  {
    m_parentShell = parentShell;
    m_df = df;

    m_plugin = plugin;

    if( plugin != null )
    {
      useRange = plugin.getDialogSettings().getBoolean(
          IKalypsoPreferences.USE_RANGE );

      long d = plugin.getDialogSettings().getLong(
          IKalypsoPreferences.DATE_FROM );
      from = new Date( d );

      d = plugin.getDialogSettings().getLong( IKalypsoPreferences.DATE_TO );
      to = new Date( d );

      days = plugin.getDialogSettings().getInt(
          IKalypsoPreferences.NUMBER_OF_DAYS );
    }

    m_store = new PreferenceStore();
    m_store.setDefault( IKalypsoPreferences.USE_RANGE, useRange );
    m_store.setDefault( IKalypsoPreferences.DATE_FROM, m_df.format( from ) );
    m_store.setDefault( IKalypsoPreferences.DATE_TO, m_df.format( to ) );
    m_store.setDefault( IKalypsoPreferences.NUMBER_OF_DAYS, days );
  }

  public void dispose( )
  {
    m_fDateFrom.dispose();
    m_fDateTo.dispose();
    m_fNumberOfDays.dispose();
    m_fUseRange.dispose();
  }

  public Control createControl( final Composite parent )
  {
    final Composite sub = new Composite( parent, SWT.FILL );

    m_fNumberOfDays = new IntegerFieldEditor(
        IKalypsoPreferences.NUMBER_OF_DAYS, "Anzahl Tagen:", sub );
    m_fUseRange = new BooleanFieldEditor( IKalypsoPreferences.USE_RANGE,
        "Zeitraum statt Tagesanzahl benutzen:", sub );
    m_fDateFrom = new StringFieldEditor( IKalypsoPreferences.DATE_FROM,
        "Zeitraum-Von:", sub );
    m_fDateTo = new StringFieldEditor( IKalypsoPreferences.DATE_TO,
        "Zeitraum-Bis:", sub );

    m_fNumberOfDays.setPreferenceStore( m_store );
    m_fNumberOfDays.loadDefault();
    m_fNumberOfDays.setEmptyStringAllowed( false );
    m_fNumberOfDays
        .setErrorMessage( "Geben Sie einen Zahl im Bereich [0 - N] ein." );

    m_fUseRange.setPreferenceStore( m_store );
    m_fUseRange.loadDefault();

    m_fDateFrom.setPreferenceStore( m_store );
    m_fDateFrom.loadDefault();
    m_fDateTo.setPreferenceStore( m_store );
    m_fDateTo.loadDefault();

    final GridLayout gridLayout = new GridLayout( 2, true );
    sub.setLayout( gridLayout );
    sub.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    m_fNumberOfDays.fillIntoGrid( sub, 2 );
    m_fUseRange.fillIntoGrid( sub, 2 );
    m_fDateFrom.fillIntoGrid( sub, 2 );
    m_fDateTo.fillIntoGrid( sub, 2 );

    return sub;
  }

  public boolean okPressed( )
  {
    m_fUseRange.store();

    m_fDateFrom.setEmptyStringAllowed( !isUseRange() );
    m_fDateTo.setEmptyStringAllowed( !isUseRange() );

    if( isUseRange() )
    {
      if( !m_fDateFrom.isValid()
          || parseForDate( m_fDateFrom.getStringValue() ) == null )
      {
        MessageDialog.openInformation( m_parentShell, errTitle, errMsg
            + m_fDateFrom.getLabelText() + ": " + m_fDateFrom.getStringValue() );
        return false;
      }

      if( !m_fDateTo.isValid()
          || parseForDate( m_fDateTo.getStringValue() ) == null )
      {
        MessageDialog.openInformation( m_parentShell, errTitle, errMsg
            + m_fDateTo.getLabelText() + ": " + m_fDateTo.getStringValue() );
        return false;
      }

      m_fDateFrom.store();
      m_fDateTo.store();
    }
    else
    {
      if( !m_fNumberOfDays.isValid()
          || Integer.valueOf( m_fNumberOfDays.getStringValue() ).intValue() < 0 )
      {
        MessageDialog.openInformation( m_parentShell, errTitle, errMsg
            + m_fNumberOfDays.getLabelText() + ": "
            + m_fNumberOfDays.getStringValue() );
        return false;
      }

      m_fNumberOfDays.store();
    }

    if( m_plugin != null )
    {
      m_plugin.getDialogSettings().put( IKalypsoPreferences.USE_RANGE, isUseRange() );
      m_plugin.getDialogSettings().put( IKalypsoPreferences.DATE_FROM, getDateFrom().getTime() );
      m_plugin.getDialogSettings().put( IKalypsoPreferences.DATE_TO, getDateTo().getTime() );
      m_plugin.getDialogSettings().put( IKalypsoPreferences.NUMBER_OF_DAYS, getNumberOfDays() );
    }
    
    return true;
  }

  /**
   * @return use-range flag
   */
  public boolean isUseRange( )
  {
    return m_store.getBoolean( IKalypsoPreferences.USE_RANGE );
  }

  /**
   * @return from-Date
   */
  public Date getDateFrom( )
  {
    final String from = m_store.getString( IKalypsoPreferences.DATE_FROM );

    return parseForDate( from );
  }

  /**
   * @return to-Date
   */
  public Date getDateTo( )
  {
    final String to = m_store.getString( IKalypsoPreferences.DATE_TO );

    return parseForDate( to );
  }

  /**
   * @return number of days
   */
  public int getNumberOfDays( )
  {
    return m_store.getInt( IKalypsoPreferences.NUMBER_OF_DAYS );
  }

  /**
   * Helper: parses the given string into a date. If a ParseException occurs, it
   * returns null.
   * 
   * @param str
   * @return new Date or null if ParseException occured.
   */
  private Date parseForDate( final String str )
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
}