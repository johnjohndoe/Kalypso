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
package org.kalypso.model.wspm.pdb.ui.preferences.internal;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.viewers.ArrayContentProvider;
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
import org.eclipse.swt.widgets.Table;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.kalypso.contribs.eclipse.jface.action.ActionButton;
import org.kalypso.core.status.StatusDialog2;
import org.kalypso.model.wspm.pdb.connect.IPdbConnectInfo;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.PdbConnections;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;

public class WspmPdbPreferencePage extends PreferencePage implements IWorkbenchPreferencePage
{
  public static final String ID = "WspmPdbPreferencePage"; //$NON-NLS-1$

  private PdbPageAction[] m_actions;

  private List<IPdbConnectInfo> m_tableInput;

  private IPdbConnectInfo m_selectedItem;

  private TableViewer m_viewer;

  public WspmPdbPreferencePage( )
  {
    super( "Wspm Cross Section" );
  }

  @Override
  public void init( final IWorkbench workbench )
  {
    m_tableInput = getTableInput();
    if( m_tableInput.size() > 0 )
      m_selectedItem = m_tableInput.get( 0 );
  }

  @Override
  public boolean performOk( )
  {
    try
    {
      final IPdbConnectInfo[] infos = m_tableInput.toArray( new IPdbConnectInfo[m_tableInput.size()] );
      PdbConnections.setConnections( infos );
      return true;
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();

      final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, "Failed to store connections", e );
      return new StatusDialog2( getShell(), status, "Save Connections" ).open() == Window.OK;
    }
  }

  private List<IPdbConnectInfo> getTableInput( )
  {
    // Get connections and clone into list; we are going to change the list
    final IPdbConnectInfo[] connections = PdbConnections.getConnectionsOrError();
    return new ArrayList<IPdbConnectInfo>( Arrays.asList( connections ) );
  }

  @Override
  protected Control createContents( final Composite parent )
  {
    final Group panel = new Group( parent, SWT.NONE );
    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( panel );
    panel.setText( "Connections" );

    m_actions = createActions();

    createDescription( panel ).setLayoutData( new GridData( SWT.FILL, SWT.TOP, true, false, 2, 1 ) );
    createConnectionList( panel ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    createButtonBar( panel ).setLayoutData( new GridData( SWT.LEFT, SWT.FILL, false, true ) );

    return panel;
  }

  private PdbPageAction[] createActions( )
  {
    final PdbPageAction[] actions = new PdbPageAction[3];
    actions[0] = new CreateConnectionAction( this );
    actions[1] = new EditConnectionAction( this );
    actions[2] = new RemoveConnectionAction( this );
    return actions;
  }

  private Control createDescription( final Group panel )
  {
    final Label label = new Label( panel, SWT.WRAP );

    label.setText( "Add, remove or edit database connections." );

    return label;
  }

  private Control createConnectionList( final Composite parent )
  {
    m_viewer = new TableViewer( parent, SWT.SINGLE | SWT.BORDER );
    final Table table = m_viewer.getTable();
    table.setHeaderVisible( false );

    m_viewer.setContentProvider( new ArrayContentProvider() );
    m_viewer.setLabelProvider( new PdbConnectInfoLabelProvider( "%s - %s" ) );
    m_viewer.setInput( m_tableInput );

    m_viewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        final IPdbConnectInfo info = (IPdbConnectInfo) selection.getFirstElement();
        handleItemSelected( info );
      }
    } );

    if( m_selectedItem != null )
      m_viewer.setSelection( new StructuredSelection( m_selectedItem ) );

    return m_viewer.getControl();
  }

  protected void handleItemSelected( final IPdbConnectInfo info )
  {
    m_selectedItem = info;

    updateActions();
  }

  private void updateActions( )
  {
    for( final PdbPageAction action : m_actions )
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

  IPdbConnectInfo getSelectedItem( )
  {
    return m_selectedItem;
  }

  public void removeItem( final IPdbConnectInfo info )
  {
    final List<IPdbConnectInfo> connections = m_tableInput;
    final int index = connections.indexOf( info );
    connections.remove( info );
    m_viewer.refresh();

    final IPdbConnectInfo newSelection = findSelectionAfterRemove( index );
    if( newSelection != null )
      m_viewer.setSelection( new StructuredSelection( newSelection ) );
  }

  private IPdbConnectInfo findSelectionAfterRemove( final int removedIndex )
  {
    final int size = m_tableInput.size();
    if( size == 0 )
      return null;

    if( size > removedIndex )
      return m_tableInput.get( removedIndex );

    return m_tableInput.get( size - 1 );
  }

  public void addNewItem( final IPdbConnectInfo newInfo )
  {
    m_tableInput.add( newInfo );
    m_viewer.refresh();
    m_viewer.setSelection( new StructuredSelection( newInfo ) );
  }

  public void replaceItem( final IPdbConnectInfo oldInfo, final IPdbConnectInfo newInfo )
  {
    final int index = m_tableInput.indexOf( oldInfo );
    m_tableInput.set( index, newInfo );
    m_viewer.refresh();
    m_viewer.setSelection( new StructuredSelection( newInfo ) );
  }
}