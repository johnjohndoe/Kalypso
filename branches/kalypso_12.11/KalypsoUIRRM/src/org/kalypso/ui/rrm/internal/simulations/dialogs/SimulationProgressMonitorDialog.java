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
package org.kalypso.ui.rrm.internal.simulations.dialogs;

import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

/**
 * A progress monitor dialog, which shows also the console window.
 * 
 * @author Holger Albert
 */
public class SimulationProgressMonitorDialog extends ProgressMonitorDialog
{
  /**
   * The main composite.
   */
  // private final Composite m_main;

  /**
   * The constructor.
   * 
   * @param parent
   *          The parent shell, or null to create a top-level shell.
   */
  public SimulationProgressMonitorDialog( final Shell parent )
  {
    super( parent );

    // m_main = null;
  }

  /**
   * @see org.eclipse.jface.dialogs.ProgressMonitorDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( final Composite parent )
  {
    /* Create the main composite. */
    // m_main = new Composite( parent, SWT.NONE );
    // final GridLayout mainLayout = new GridLayout( 1, false );
    // mainLayout.marginHeight = 0;
    // mainLayout.marginWidth = 0;
    // m_main.setLayout( mainLayout );
    // final GridData mainData = new GridData( SWT.FILL, SWT.FILL, true, true, 2, 1 );
    // mainData.heightHint = 150;
    // m_main.setLayoutData( mainData );

    /* Create a label. */
    // final Label consoleLabel = new Label( m_main, SWT.WRAP );
    // consoleLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    // consoleLabel.setText( "Console Output" );

    /* Create the console view. */
    // final IConsoleManager consoleManager = ConsolePlugin.getDefault().getConsoleManager();
    // TODO

    /* Create a empty label. */
    // final Label emptyLabel = new Label( m_main, SWT.NONE );
    // emptyLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    /* Create the default progress dialog controls. */
    super.createDialogArea( parent );

    return parent;
  }
}