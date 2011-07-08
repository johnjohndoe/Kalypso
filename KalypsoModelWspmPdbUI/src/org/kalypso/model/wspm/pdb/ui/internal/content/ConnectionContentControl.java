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
package org.kalypso.model.wspm.pdb.ui.internal.content;

import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.TreeEvent;
import org.eclipse.swt.events.TreeListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.contribs.eclipse.jface.action.ActionHyperlink;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.contribs.eclipse.swt.widgets.ControlUtils;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.ui.internal.admin.state.StatesViewer;
import org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.WaterBodyViewer;
import org.kalypso.model.wspm.pdb.ui.internal.wspm.PdbWspmProject;

/**
 * @author Gernot Belger
 */
public class ConnectionContentControl extends Composite
{
  private final IJobChangeListener m_refreshJobListener = new JobChangeAdapter()
  {
    @Override
    public void done( final IJobChangeEvent event )
    {
      handleRefreshDone();
    }
  };

  private final RefreshContentJob m_refreshJob;

  private TreeViewer m_viewer;

  private final ToolBarManager m_manager;

  private final PdbWspmProject m_project;

  private ColumnsResizeControlListener m_treeListener;

  public ConnectionContentControl( final FormToolkit toolkit, final Section parent, final IPdbConnection connection, final PdbWspmProject project )
  {
    super( parent, SWT.NONE );

    m_project = project;

    m_refreshJob = new RefreshContentJob( connection );
    m_refreshJob.setUser( true );
    m_refreshJob.addJobChangeListener( m_refreshJobListener );

    GridLayoutFactory.fillDefaults().spacing( 0, 0 ).applyTo( this );
    toolkit.adapt( this );

    ControlUtils.addDisposeListener( this );

    m_manager = new ToolBarManager( SWT.FLAT | SWT.SHADOW_OUT );

    createToolbar( toolkit, this ).setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    createTree( toolkit, this ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    createActions();

    final CheckoutAction checkoutAction = new CheckoutAction( this );
    ActionHyperlink.createHyperlink( toolkit, this, SWT.PUSH, checkoutAction ).setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    refresh( null );
  }

  @Override
  public void dispose( )
  {
    resetInput();

    super.dispose();
  }

  private Control createToolbar( final FormToolkit toolkit, final Composite parent )
  {
    final ToolBar toolBar = m_manager.createControl( parent );
    toolkit.adapt( toolBar );
    return toolBar;
  }

  private Control createTree( final FormToolkit toolkit, final Composite parent )
  {
    final Tree tree = toolkit.createTree( parent, SWT.FULL_SELECTION | SWT.MULTI );
    tree.setHeaderVisible( true );
    m_viewer = new TreeViewer( tree );
    m_viewer.setUseHashlookup( true );

    final ViewerColumn nameColumn = StatesViewer.createNameColumn( m_viewer );
    WaterBodyViewer.createNameColumn( m_viewer );
    StatesViewer.createMeasurementDateColumn( m_viewer );

    ColumnViewerSorter.setSortState( nameColumn, false );

    m_treeListener = new ColumnsResizeControlListener();
    tree.addControlListener( m_treeListener );

    m_viewer.setContentProvider( new ByWaterBodyContentProvider() );
    tree.addTreeListener( new TreeListener()
    {
      @Override
      public void treeExpanded( final TreeEvent e )
      {
        refreshColumnSizes();
      }

      @Override
      public void treeCollapsed( final TreeEvent e )
      {
        refreshColumnSizes();
      }
    } );

    return tree;
  }

  private void createActions( )
  {
    m_manager.add( new RefreshAction( this ) );
    m_manager.add( new ExpandAllAction( this ) );
    m_manager.add( new CollapseAllAction( this ) );
    m_manager.add( new Separator() );

    m_manager.update( true );
  }

  public void refresh( final ElementSelector elementToSelect )
  {
    m_refreshJob.cancel();

    resetInput();

    m_refreshJob.setElementToSelect( elementToSelect );
    m_refreshJob.schedule( 100 );
  }

  private void resetInput( )
  {
    // TODO:maybe we should keep input until new one is loaded?

    final Object oldInput = m_viewer.getInput();
    if( oldInput instanceof ConnectionInput )
      ((ConnectionInput) oldInput).dispose();

    m_viewer.setInput( PdbLabelProvider.PENDING );
    refreshColumnSizes();
  }

  protected void handleRefreshDone( )
  {
    final Object oldInput = m_viewer.getInput();

    try
    {
      final ConnectionInput input = m_refreshJob.getInput();
      ViewerUtilities.setInput( m_viewer, input, true );

      final ElementSelector selector = m_refreshJob.getElementToSelect();
      final Object element = selector == null ? null : selector.findElement( input );
      if( element != null )
        ViewerUtilities.setSelection( m_viewer, new StructuredSelection( element ), true, true );

      refreshColumnSizes();
    }
    finally
    {
      if( oldInput instanceof ConnectionInput )
        ((ConnectionInput) oldInput).dispose();
    }
  }

  public IStructuredSelection getSelection( )
  {
    return (IStructuredSelection) m_viewer.getSelection();
  }

  public PdbWspmProject getProject( )
  {
    return m_project;
  }

  protected void refreshColumnSizes( )
  {
    m_treeListener.updateColumnSizes();
  }

  public TreeViewer getTreeViewer( )
  {
    return m_viewer;
  }
}