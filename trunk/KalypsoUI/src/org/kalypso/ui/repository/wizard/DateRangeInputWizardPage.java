package org.kalypso.ui.repository.wizard;

import java.text.DateFormat;

import org.eclipse.core.runtime.Preferences;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.preferences.IKalypsoPreferences;
import org.kalypso.ui.repository.dialogs.DateRangeInputControl;
import org.kalypso.util.runtime.args.DateRangeArgument;

/**
 * DateRangeInputWizardPage
 * 
 * @author schlienger
 */
public class DateRangeInputWizardPage extends WizardPage
{
  private DateRangeInputControl m_ctrl;

  public DateRangeInputWizardPage( )
  {
    super( "DateRangeInputWizardPage" );

    setTitle( "Datum-Eingabe" );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    final Preferences prefs = KalypsoGisPlugin.getDefault()
        .getPluginPreferences();

    final DateFormat df = DateFormat.getDateTimeInstance();

    try
    {
      m_ctrl = new DateRangeInputControl( getShell(), prefs
          .getDefaultBoolean( IKalypsoPreferences.USE_RANGE ), df.parse( prefs
          .getDefaultString( IKalypsoPreferences.DATE_FROM ) ), df.parse( prefs
          .getDefaultString( IKalypsoPreferences.DATE_TO ) ), prefs
          .getDefaultInt( IKalypsoPreferences.NUMBER_OF_DAYS ), df, null );

      setControl( m_ctrl.createControl( parent ) );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw new IllegalStateException( e.getLocalizedMessage() );
    }

    setDescription( DateRangeInputControl.DESCRIPTION );
  }

  public DateRangeArgument getDateRange( )
  {
    if( m_ctrl != null )
    {
      if( m_ctrl.isUseRange() )
        return new DateRangeArgument( m_ctrl.getDateFrom(), m_ctrl.getDateTo() );

      return DateRangeArgument.createFromPastDays( m_ctrl.getNumberOfDays() );
    }

    return null;
  }
}