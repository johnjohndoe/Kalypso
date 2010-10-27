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
package org.kalypso.model.wspm.tuhh.ui.export.shape;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;

/**
 * @author belger
 */
public class ExportProfileLineInterpolationPage extends WizardPage
{
  private static final String SETTINGS_DO_INTERPOLATION = "doInterpolation";

  private static final String SETTINGS_DO_FORELAND = "doForeland";

  private boolean m_doInterpolation = true;

  private boolean m_doForelandInterpolation = false;

  public ExportProfileLineInterpolationPage( final String pageName )
  {
    super( pageName );

    setTitle( "Interpolation options" );
    setDescription( "Choose options for interpolated profiles on this page" );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    readDialogSettings();

    final Group group = new Group( parent, SWT.NONE );
    setControl( group );

    group.setText( "Interpolation Parameters" );
    group.setLayout( new GridLayout() );

    final Button doInterpolationButton = new Button( group, SWT.CHECK );
    doInterpolationButton.setText( "Add Interpolated Profiles" );
    doInterpolationButton.setToolTipText( "Interpolates profiles for interpolated stations during the calculation to the output." );
    doInterpolationButton.setSelection( m_doInterpolation );

    final Button interpolationForelandButton = new Button( group, SWT.CHECK );
    interpolationForelandButton.setText( "Interpolate Forland" );
    interpolationForelandButton.setToolTipText( "If checked, the profile will also be interpolated on the foreland. Else, only the main channel will be interpolated and exported." );
    interpolationForelandButton.setSelection( m_doForelandInterpolation );
    interpolationForelandButton.setEnabled( m_doInterpolation );

    doInterpolationButton.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final boolean selection = doInterpolationButton.getSelection();
        setDoInterpolation( selection );
        interpolationForelandButton.setEnabled( selection );
      }
    } );

    interpolationForelandButton.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        setDoForeland( interpolationForelandButton.getSelection() );
      }
    } );

  }

  private void readDialogSettings( )
  {
    final IDialogSettings dialogSettings = getDialogSettings();
    if( dialogSettings == null )
      return;

    m_doInterpolation = dialogSettings.getBoolean( SETTINGS_DO_INTERPOLATION );
    m_doForelandInterpolation = dialogSettings.getBoolean( SETTINGS_DO_FORELAND );
  }

  protected void setDoInterpolation( final boolean doInterpolation )
  {
    m_doInterpolation = doInterpolation;

    final IDialogSettings dialogSettings = getDialogSettings();
    if( dialogSettings != null )
      dialogSettings.put( SETTINGS_DO_INTERPOLATION, m_doInterpolation );
  }

  protected void setDoForeland( final boolean doForelandInterpolation )
  {
    m_doForelandInterpolation = doForelandInterpolation;

    final IDialogSettings dialogSettings = getDialogSettings();
    if( dialogSettings != null )
      dialogSettings.put( SETTINGS_DO_FORELAND, m_doForelandInterpolation );
  }

  public boolean shouldAddInterpolatedProfiles( )
  {
    return m_doInterpolation;
  }

  public boolean shouldInterpolateForland( )
  {
    return m_doForelandInterpolation;
  }

}
