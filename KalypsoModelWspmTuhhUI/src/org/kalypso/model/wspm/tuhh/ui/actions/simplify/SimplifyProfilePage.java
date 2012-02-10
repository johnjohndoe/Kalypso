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
package org.kalypso.model.wspm.tuhh.ui.actions.simplify;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.filter.ProfilePointFilterComposite;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.observation.result.IRecord;

/**
 * @author Gernot Belger
 */
public class SimplifyProfilePage extends WizardPage
{
  private static final String SETTINGS_DISTANCE = "distance"; //$NON-NLS-1$

  private static final String SETTINGS_KEEP_BUILDING_PONITS = "keepBuildingPoints"; //$NON-NLS-1$

  private static final double DEFAULT_DISTANCE = 0.5;

  private double m_distance = DEFAULT_DISTANCE;

  private final ProfilePointFilterComposite m_filterChooser = new ProfilePointFilterComposite();

  private boolean m_isKeepBuildingPoints = false;

  protected SimplifyProfilePage( final String pageName )
  {
    super( pageName );

    setTitle( Messages.getString("SimplifyProfilePage_2") ); //$NON-NLS-1$
    setDescription( Messages.getString("SimplifyProfilePage_3") ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    final Group group = new Group( parent, SWT.NONE );
    group.setLayout( new GridLayout( 2, false ) );
    setControl( group );

    initFromSettings();

    createDistanceControl( group );
    createKeepBuildingPointsControl( group );
    createFilterControl( group );
  }

  private void createKeepBuildingPointsControl( final Composite parent )
  {
    final Button checkbox = new Button( parent, SWT.CHECK );
    checkbox.setLayoutData( new GridData( SWT.LEFT, SWT.CENTER, false, false, 2, 1 ) );
    checkbox.setText( Messages.getString("SimplifyProfilePage_4") ); //$NON-NLS-1$
    checkbox.setToolTipText( Messages.getString("SimplifyProfilePage_5") ); //$NON-NLS-1$
    checkbox.setSelection( m_isKeepBuildingPoints );

    checkbox.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final boolean selection = checkbox.getSelection();
        handleKeepBuildingPointsChanged( selection );
      }
    } );
  }

  private void createFilterControl( final Composite parent )
  {
    final Control filterControl = m_filterChooser.createControl( parent, SWT.BORDER );
    filterControl.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true, 2, 1 ) );

    m_filterChooser.setDialogSettings( getDialogSettings() );
  }

  private void createDistanceControl( final Group group )
  {
    final Label label = new Label( group, SWT.NONE );
    label.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );
    label.setText( Messages.getString("SimplifyProfilePage_6") ); //$NON-NLS-1$

    final Spinner spinner = new Spinner( group, SWT.BORDER | SWT.TRAIL );
    final GridData gridData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    spinner.setLayoutData( gridData );

    spinner.setDigits( 2 );
    spinner.setMaximum( Integer.MAX_VALUE );
    spinner.setIncrement( 10 );
    spinner.setPageIncrement( 100 );

    spinner.setSelection( (int) (m_distance * 100) );

    spinner.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        handleDistanceChanged( spinner.getSelection() / 100.0 );
      }
    } );
  }

  private void initFromSettings( )
  {
    final IDialogSettings dialogSettings = getDialogSettings();
    if( dialogSettings == null )
      return;

    try
    {
      m_distance = dialogSettings.getDouble( SETTINGS_DISTANCE );
      m_isKeepBuildingPoints = dialogSettings.getBoolean( SETTINGS_KEEP_BUILDING_PONITS );
    }
    catch( final NumberFormatException e )
    {
      // ignore
    }
  }

  protected void handleKeepBuildingPointsChanged( final boolean keepBuildingPoints )
  {
    m_isKeepBuildingPoints = keepBuildingPoints;

    final IDialogSettings dialogSettings = getDialogSettings();
    if( dialogSettings != null )
      dialogSettings.put( SETTINGS_KEEP_BUILDING_PONITS, keepBuildingPoints );
  }

  protected void handleDistanceChanged( final double distance )
  {
    m_distance = distance;

    final IDialogSettings dialogSettings = getDialogSettings();
    if( dialogSettings != null )
      dialogSettings.put( SETTINGS_DISTANCE, distance );
  }

  public double getDistance( )
  {
    return m_distance;
  }

  public IRecord[] getSelectedPoints( final IProfil profile )
  {
    return m_filterChooser.getSelectedPoints( profile );
  }

  public boolean isKeepBuildingPoints( )
  {
    return m_isKeepBuildingPoints;
  }

}
