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

import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.kalypso.contribs.eclipse.jface.action.ActionButton;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.pdb.connect.IPdbSettings;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.PdbSettings;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

public class WspmPdbPreferencePage extends PreferencePage implements IWorkbenchPreferencePage
{
  public static final String ID = "WspmPdbPreferencePage"; //$NON-NLS-1$

  private SettingsAction[] m_actions;

  private List<IPdbSettings> m_tableInput;

  private IPdbSettings m_selectedItem;

  private TableViewer m_viewer;

  public WspmPdbPreferencePage( )
  {
    super( Messages.getString( "WspmPdbPreferencePage.0" ) ); //$NON-NLS-1$

    noDefaultAndApplyButton();
  }

  @Override
  public void init( final IWorkbench workbench )
  {
  }

  @Override
  public boolean performOk( )
  {
    try
    {
      final IPdbSettings[] settings = m_tableInput.toArray( new IPdbSettings[m_tableInput.size()] );
      PdbSettings.setSettings( settings );
      return true;
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();

      final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, Messages.getString( "WspmPdbPreferencePage.1" ), e ); //$NON-NLS-1$
      return new StatusDialog( getShell(), status, Messages.getString( "WspmPdbPreferencePage.2" ) ).open() == Window.OK; //$NON-NLS-1$
    }
  }

  @Override
  protected Control createContents( final Composite parent )
  {
    final Group panel = new Group( parent, SWT.NONE );
    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( panel );
    panel.setText( Messages.getString( "WspmPdbPreferencePage.3" ) ); //$NON-NLS-1$

    m_actions = createActions();

    createDescription( panel ).setLayoutData( new GridData( SWT.FILL, SWT.TOP, true, false, 2, 1 ) );
    createConnectionList( panel ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    createButtonBar( panel ).setLayoutData( new GridData( SWT.LEFT, SWT.FILL, false, true ) );

    return panel;
  }

  private SettingsAction[] createActions( )
  {
    final SettingsAction[] actions = new SettingsAction[3];
    actions[0] = new CreateSettingsAction( this );
    actions[1] = new EditSettingsAction( this );
    actions[2] = new RemoveSettingsAction( this );
    return actions;
  }

  private Control createDescription( final Group panel )
  {
    final Label label = new Label( panel, SWT.WRAP );

    label.setText( Messages.getString( "WspmPdbPreferencePage.4" ) ); //$NON-NLS-1$

    return label;
  }

  private Control createConnectionList( final Composite parent )
  {
    final PdbSettingsViewer settingsViewer = new PdbSettingsViewer();
    m_viewer = settingsViewer.createViewer( parent );
    m_tableInput = settingsViewer.getInput();

    if( m_tableInput.size() > 0 )
      m_selectedItem = m_tableInput.get( 0 );

    m_viewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        final IPdbSettings settings = (IPdbSettings) selection.getFirstElement();
        handleItemSelected( settings );
      }
    } );

    if( m_selectedItem != null )
      m_viewer.setSelection( new StructuredSelection( m_selectedItem ) );

    return m_viewer.getControl();
  }

  protected void handleItemSelected( final IPdbSettings settings )
  {
    m_selectedItem = settings;

    updateActions();
  }

  private void updateActions( )
  {
    for( final SettingsAction action : m_actions )
      action.update();
  }

  private Control createButtonBar( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    GridLayoutFactory.fillDefaults().applyTo( panel );

    for( final IAction action : m_actions )
    {
      final Button button = ActionButton.createButton( null, panel, action );
      button.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    }

    updateActions();

    return panel;
  }

  IPdbSettings getSelectedItem( )
  {
    return m_selectedItem;
  }

  public void removeItem( final IPdbSettings settings )
  {
    final List<IPdbSettings> connections = m_tableInput;
    final int index = connections.indexOf( settings );
    connections.remove( settings );
    m_viewer.refresh();

    final IPdbSettings newSelection = findSelectionAfterRemove( index );
    if( newSelection != null )
      m_viewer.setSelection( new StructuredSelection( newSelection ) );
  }

  private IPdbSettings findSelectionAfterRemove( final int removedIndex )
  {
    final int size = m_tableInput.size();
    if( size == 0 )
      return null;

    if( size > removedIndex )
      return m_tableInput.get( removedIndex );

    return m_tableInput.get( size - 1 );
  }

  public void addNewItem( final IPdbSettings newSettings )
  {
    m_tableInput.add( newSettings );
    m_viewer.refresh();
    m_viewer.setSelection( new StructuredSelection( newSettings ) );
  }

  public void replaceItem( final IPdbSettings oldSettings, final IPdbSettings newSettings )
  {
    final int index = m_tableInput.indexOf( oldSettings );
    m_tableInput.set( index, newSettings );
    m_viewer.refresh();
    m_viewer.setSelection( new StructuredSelection( newSettings ) );
  }
}