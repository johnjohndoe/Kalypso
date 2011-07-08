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
package org.kalypso.model.wspm.pdb.ui.internal.admin.event;

import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.hibernate.Session;
import org.kalypso.contribs.eclipse.jface.action.ActionButton;
import org.kalypso.contribs.eclipse.jface.action.UpdateableAction;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.ui.internal.content.filter.WaterBodyFilterControl;

/**
 * @author Gernot Belger
 */
public class ManageEventsPage extends WizardPage
{
  private final EventViewer m_viewer;

  private UpdateableAction[] m_actions;

  private Event m_selectedItem;

  private final Session m_session;

  private final IPdbConnection m_connection;

  protected ManageEventsPage( final String pageName, final IPdbConnection connection, final Session session )
  {
    super( pageName );

    m_connection = connection;

    setTitle( "Manage Water Bodies" );
    setDescription( "Manage the Water Bodies of the Cross Section Database" );

    m_session = session;
    m_viewer = new EventViewer( m_session );
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    setControl( panel );
    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( panel );

    final Composite tablePanel = new Composite( panel, SWT.NONE );
    GridLayoutFactory.fillDefaults().applyTo( tablePanel );
    tablePanel.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    // Table
    final TableViewer tableViewer = m_viewer.createTableViewer( tablePanel );
    final GridData tableData = new GridData( SWT.FILL, SWT.FILL, true, true );
    tableData.heightHint = 250;
    m_viewer.getControl().setLayoutData( tableData );

    tableViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        final Event waterlevel = (Event) selection.getFirstElement();
        handleItemSelected( waterlevel );
      }
    } );

    createSearchFields( tablePanel, tableViewer ).setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );

    final UpdateableAction[] actions = createActions();
    final Composite actionPanel = new Composite( panel, SWT.NONE );
    actionPanel.setLayoutData( new GridData( SWT.FILL, SWT.FILL, false, true ) );
    GridLayoutFactory.fillDefaults().applyTo( actionPanel );

    for( final UpdateableAction action : actions )
    {
      final Button button = ActionButton.createButton( null, actionPanel, action );
      button.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    }

    updateActions();
  }

  private Control createSearchFields( final Composite parent, final TableViewer viewer )
  {
    final Group panel = new Group( parent, SWT.NONE );
    panel.setLayout( new FillLayout() );
    panel.setText( "Search" );

    // FIXME
    new WaterBodyFilterControl( null, panel ).setViewer( viewer );

    return panel;
  }

  private UpdateableAction[] createActions( )
  {
    m_actions = new UpdateableAction[0];

// m_actions[0] = new EditEventAction( this, m_viewer );
// m_actions[1] = new RemoveEventAction( this, m_viewer );
// m_actions[2] = new ImportEventAction( this, m_viewer );

    return m_actions;
  }

  protected void handleItemSelected( final Event event )
  {
    m_selectedItem = event;

    updateActions();
  }

  private void updateActions( )
  {
    for( final UpdateableAction action : m_actions )
      action.update();
  }

  public Event getSelectedItem( )
  {
    return m_selectedItem;
  }

  Session getSession( )
  {
    return m_session;
  }

  public IPdbConnection getConnection( )
  {
    return m_connection;
  }
}