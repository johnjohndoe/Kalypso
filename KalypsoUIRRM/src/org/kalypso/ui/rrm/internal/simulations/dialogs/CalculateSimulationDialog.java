/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;

/**
 * This dialog asks the user, if he wants to calculate the simulation and offers some options.
 * 
 * @author Holger Albert
 */
public class CalculateSimulationDialog extends Dialog
{
  /**
   * The dialog settings.
   */
  private final IDialogSettings m_settings;

  /**
   * The constructor.
   * 
   * @param shell
   *          The parent shell, or null to create a top-level shell.
   */
  public CalculateSimulationDialog( final Shell shell )
  {
    super( shell );

    m_settings = DialogSettingsUtils.getDialogSettings( KalypsoUIRRMPlugin.getDefault(), getClass().getName() );
  }

  /**
   * The constructor.
   * 
   * @param parentShell
   *          The object that returns the current parent shell.
   */
  public CalculateSimulationDialog( final IShellProvider parentShell )
  {
    super( parentShell );

    m_settings = DialogSettingsUtils.getDialogSettings( KalypsoUIRRMPlugin.getDefault(), getClass().getName() );
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( final Composite parent )
  {
    /* Set the title. */
    getShell().setText( "Calculate Simulation" );

    /* Create the main composite. */
    final Composite main = (Composite) super.createDialogArea( parent );
    main.setLayout( new GridLayout( 1, false ) );
    main.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* Create a label. */
    final Label questionLabel = new Label( main, SWT.WRAP );
    questionLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    questionLabel.setText( "Do you want to start the calculation? 'Ok' will start the calculation. 'Cancel' will abort." );

    // TODO Was tun, wenn es mehrere Simulationen sind (Checkboxen unten weglassen)?

    /* Create a empty label. */
    final Label emptyLabel = new Label( main, SWT.NONE );
    emptyLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    /* Create a button. */
    final Button catchmentModelButton = new Button( main, SWT.CHECK );
    catchmentModelButton.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    catchmentModelButton.setText( "Calculate the catchment models" );

    // TODO Nur bei Langzeit diese Checkbox zeigen?

    /* Create a button. */
    final Button startConditionsButton = new Button( main, SWT.CHECK );
    startConditionsButton.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    startConditionsButton.setText( "Calculate the start conditions" );

    // TODO Simulation ist noch nie gerechnet worden -> Gebietsmodelle müssen gerechnet werden...
    // TODO Simulation wurde bereits gerechnet -> Gebietsmodelle sind nicht aktuell und müssen neu gerechnet werden...
    // TODO Simulation wurde bereits gerechnet -> Gebietsmodelle sind aktuell und müssen nicht neu gerechnet werden...

    // TODO Vergleiche lastModified von den GNMs und den Simulationsergebnis...

    // TODO Wenn es eine Kurzzeit-Simulation ist und eine Langzeit-Simulation für die Anfangswerte gesetzt ist...
    // TODO Wenn es eine Kurzzeit-Simulation ist und keine Langzeit-Simulation für die Anfangswerte gesetzt ist...

    // TODO Wenn es eine Langzeit-Simulation ist und Anfangswerte berechnet werden sollen...
    // TODO Wenn es eine Langzeit-Simulation ist und keine Anfangswerte berechnet werden sollen...

    // TODO

    return main;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected void createButtonsForButtonBar( final Composite parent )
  {
    super.createButtonsForButtonBar( parent );

    /* Check, if the dialog is allowed to be completed. */
    checkDialogComplete();
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#getDialogBoundsSettings()
   */
  @Override
  protected IDialogSettings getDialogBoundsSettings( )
  {
    return DialogSettingsUtils.getSection( m_settings, "bounds" ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#isResizable()
   */
  @Override
  protected boolean isResizable( )
  {
    return true;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#okPressed()
   */
  @Override
  protected void okPressed( )
  {
    // TODO

    super.okPressed();
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#cancelPressed()
   */
  @Override
  protected void cancelPressed( )
  {
    // TODO

    super.cancelPressed();
  }

  /**
   * This function checks, if the dialog is allowed to be completed.
   */
  private void checkDialogComplete( )
  {
    /* Get the OK button. */
    final Button okButton = getButton( IDialogConstants.OK_ID );

    /* First of all, it should be allowed to complete. */
    okButton.setEnabled( true );

    // TODO
  }
}