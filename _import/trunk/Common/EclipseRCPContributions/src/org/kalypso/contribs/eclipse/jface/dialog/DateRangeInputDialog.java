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
package org.kalypso.contribs.eclipse.jface.dialog;

import java.util.Date;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.eclipse.swt.widgets.DateRangeInputControl;
import org.kalypso.contribs.eclipse.swt.widgets.DateRangeInputControlStuct;

/**
 * TitleAreaDialog for editing a date-range. It uses the DateRangeInputControl.
 * 
 * @author schlienger
 */
public class DateRangeInputDialog extends TitleAreaDialog
{
  private final String m_title;

  private final String m_message;

  private DateRangeInputControl m_control;

  private final DateRangeInputControlStuct m_sruct;

  public DateRangeInputDialog( final Shell parentShell, final String title, final String message,
      final DateRangeInputControlStuct sruct )
  {
    super( parentShell );

    m_title = title;
    m_message = message;
    m_sruct = sruct;
  }

  protected Control createDialogArea( final Composite parent )
  {
    setTitle( m_title );
    setMessage( m_message );

    m_control = new DateRangeInputControl( parent, SWT.NONE, m_sruct );

    return m_control;
  }

  protected void okPressed()
  {
    final String err = m_control.validateInput();

    if( err != null )
    {
      MessageDialog.openInformation( getParentShell(), "Fehlerhafte Eingabe", "Bitte prüfen Sie Ihre Eingabe. " + err );
      return;
    }

    super.okPressed();
  }

  public DateRangeInputControlStuct getStruct()
  {
    return m_control.getStruct();
  }

  public boolean isUseRange()
  {
    return m_control.isUseRange();
  }

  public Date getDateFrom()
  {
    return m_control.getDateFrom();
  }

  public Date getDateTo()
  {
    return m_control.getDateTo();
  }

  public int getNumberOfDays()
  {
    return m_control.getNumberOfDays();
  }
}