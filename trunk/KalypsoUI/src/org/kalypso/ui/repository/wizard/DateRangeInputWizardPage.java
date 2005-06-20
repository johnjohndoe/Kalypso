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

import org.bce.eclipse.swt.widgets.DateRangeInputControl;
import org.bce.eclipse.swt.widgets.DateRangeInputControlStuct;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.runtime.args.DateRangeArgument;

/**
 * DateRangeInputWizardPage
 * 
 * @author schlienger
 */
public class DateRangeInputWizardPage extends WizardPage
{
  private final IDialogSettings m_settings;

  private DateRangeInputControl m_control;

  public DateRangeInputWizardPage()
  {
    super( "DateRangeInputWizardPage" );

    m_settings = KalypsoGisPlugin.getDefault().getDialogSettings();
  }

  public void createControl( Composite parent )
  {
    try
    {
      final DateRangeInputControlStuct struct = DateRangeInputControlStuct.create( m_settings, DateFormat
          .getDateTimeInstance() );
      m_control = new DateRangeInputControl( parent, SWT.NONE, struct );

      setControl( m_control );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw new IllegalStateException( e.getLocalizedMessage() );
    }

    setTitle( "Zeitraum-Eingabe" );
    setDescription( "Geben Sie bitte die Zeitraum-Information ein f�r welche die Zeitreihe exportiert werden soll." );
  }

  public DateRangeArgument getDateRange()
  {
    if( m_control != null )
    {
      if( m_control.isUseRange() )
        return new DateRangeArgument( m_control.getDateFrom(), m_control.getDateTo() );

      return DateRangeArgument.createFromPastDays( m_control.getNumberOfDays() );
    }

    return null;
  }

  /**
   * @see org.eclipse.jface.wizard.WizardPage#getNextPage()
   */
  public IWizardPage getNextPage()
  {
    // go to next page only when input is valid
    final String err = m_control.validateInput();
    if( err == null )
    {
      // save dialog settings for next dialog
      m_control.getStruct().save( m_settings );

      return super.getNextPage();
    }

    // else inform user about invalid input
    MessageDialog.openInformation( getShell(), "Fehlerhafte Eingabe", "Pr�fen Sie bitte Ihre Eingabe. " + err );

    return null;
  }
}