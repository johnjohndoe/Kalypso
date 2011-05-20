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
package org.kalypso.model.wspm.pdb.ui.admin.waterbody.internal;

import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.contribs.eclipse.jface.action.ActionButton;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBodies;

/**
 * @author Gernot Belger
 */
public class ManageWaterBodiesPage extends WizardPage
{
  private final WaterBodyViewer m_viewer;

  private final IPdbConnection m_connection;

  private WaterBodyAction[] m_actions;

  private WaterBodies m_selectedItem;

  protected ManageWaterBodiesPage( final String pageName, final IPdbConnection connection )
  {
    super( pageName );

    setTitle( "Manage Water Bodies" );
    setDescription( "Manage the Water Bodies of the Cross Section Database" );

    m_connection = connection;

    m_viewer = new WaterBodyViewer( connection );
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    setControl( panel );
    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( panel );

    // Table
    m_viewer.createTableViewer( panel );
    m_viewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    m_viewer.getViewer().addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        final WaterBodies waterBody = (WaterBodies) selection.getFirstElement();
        handleItemSelected( waterBody );
      }
    } );

    final WaterBodyAction[] actions = createActions();
    final Composite actionPanel = new Composite( panel, SWT.NONE );
    actionPanel.setLayoutData( new GridData( SWT.FILL, SWT.FILL, false, true ) );
    GridLayoutFactory.fillDefaults().applyTo( actionPanel );

    for( final WaterBodyAction action : actions )
    {
      final Button button = ActionButton.createButton( null, actionPanel, action );
      button.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    }
  }

  private WaterBodyAction[] createActions( )
  {
    m_actions = new WaterBodyAction[3];
    m_actions[0] = new AddWaterBodyAction( m_connection, m_viewer, "&New..." );
    m_actions[1] = new EditWaterBodyAction( this );
    m_actions[2] = new RemoveWaterBodyAction( this );

    return m_actions;
  }

  protected void handleItemSelected( final WaterBodies waterBody )
  {
    m_selectedItem = waterBody;

    updateActions();
  }

  private void updateActions( )
  {
    for( final WaterBodyAction action : m_actions )
      action.update();
  }

  public WaterBodies getSelectedItem( )
  {
    return m_selectedItem;
  }
}