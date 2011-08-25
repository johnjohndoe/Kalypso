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
package org.kalypso.model.wspm.pdb.ui.internal.preferences;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.model.wspm.pdb.connect.IPdbSettings;
import org.kalypso.model.wspm.pdb.connect.PdbSettings;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
class SettingsTypePage extends WizardPage
{
  private IPdbSettings m_settings;

  protected SettingsTypePage( final String pageName )
  {
    super( pageName );

    setTitle( Messages.getString( "SettingsTypePage.0" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "SettingsTypePage.1" ) ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    final String[] connectionTypes = PdbSettings.getSettingsTypes();
    final IPdbSettings[] settings = new IPdbSettings[connectionTypes.length];
    for( int i = 0; i < settings.length; i++ )
      settings[i] = PdbSettings.createSettings( connectionTypes[i] );

    final TableViewer tableViewer = new TableViewer( parent, SWT.BORDER );
    setControl( tableViewer.getControl() );

    tableViewer.setLabelProvider( new SettingsLabelProvider( "%s" ) ); //$NON-NLS-1$
    tableViewer.setContentProvider( new ArrayContentProvider() );
    tableViewer.setSorter( new ViewerSorter() );
    tableViewer.setInput( settings );

    tableViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        final IPdbSettings element = (IPdbSettings) selection.getFirstElement();
        handleSettingsSelected( element );
      }
    } );

    setPageComplete( false );

    if( settings.length > 0 )
      tableViewer.setSelection( new StructuredSelection( settings[0] ) );
  }

  protected void handleSettingsSelected( final IPdbSettings settings )
  {
    m_settings = settings;

    setPageComplete( settings != null );
  }

  public IPdbSettings getSettings( )
  {
    return m_settings;
  }
}
