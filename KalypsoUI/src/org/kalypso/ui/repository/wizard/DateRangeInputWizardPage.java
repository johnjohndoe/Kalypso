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
package org.kalypso.ui.repository.wizard;

import java.text.DateFormat;

import org.eclipse.core.runtime.Preferences;
import org.eclipse.jface.wizard.IWizardPage;
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
  
  /**
   * @see org.eclipse.jface.wizard.WizardPage#getNextPage()
   */
  public IWizardPage getNextPage( )
  {
    // go to next page only when input is valid
    if( m_ctrl.okPressed() )    
      return super.getNextPage();
    
    return null;
  }
}