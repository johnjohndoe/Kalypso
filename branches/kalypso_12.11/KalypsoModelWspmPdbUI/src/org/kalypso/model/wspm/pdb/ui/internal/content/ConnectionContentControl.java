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

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.viewers.ILabelDecorator;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.TreeEvent;
import org.eclipse.swt.events.TreeListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.menus.IMenuService;
import org.eclipse.ui.services.IServiceLocator;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.jface.viewers.tree.CollapseAllTreeItemsAction;
import org.kalypso.contribs.eclipse.jface.viewers.tree.ExpandAllTreeItemsAction;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.contribs.eclipse.swt.widgets.ControlUtils;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.ui.internal.IWaterBodyStructure;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.admin.state.StatesViewer;
import org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.WaterBodyViewer;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class ConnectionContentControl extends Composite
{
  private static final String TOOLBAR_URI = "toolbar:org.kalypso.model.wspm.pdb.ui.content"; //$NON-NLS-1$

  private static final IStatus STATUS_UPDATE = new Status( IStatus.INFO, WspmPdbUiPlugin.PLUGIN_ID, Messages.getString( "ConnectionContentControl_0" ) ); //$NON-NLS-1$

  private final IJobChangeListener m_refreshJobListener = new JobChangeAdapter()
  {
    @Override
    public void done( final IJobChangeEvent event )
    {
      handleRefreshDone( event.getResult() );
    }
  };

  private final RefreshContentJob m_refreshJob;

  private TreeViewer m_waterViewer;

  private final ToolBarManager m_manager;

  private final IServiceLocator m_serviceLocator;

  private StatusComposite m_statusBar;

//  private StateTreeComposite m_stateTree;

  // private final StateTreeUpdater m_stateTreeUpdater;

  public ConnectionContentControl( final IServiceLocator serviceLocator, final FormToolkit toolkit, final Composite parent, final IPdbConnection connection )
  {
    super( parent, SWT.NONE );

    m_serviceLocator = serviceLocator;

    m_refreshJob = new RefreshContentJob( connection );
    m_refreshJob.setSystem( true );
    m_refreshJob.addJobChangeListener( m_refreshJobListener );

    GridLayoutFactory.fillDefaults().spacing( 0, 0 ).applyTo( this );
    if( toolkit != null )
      toolkit.adapt( this );

    ControlUtils.addDisposeListener( this );

    m_manager = new ToolBarManager( SWT.FLAT | SWT.SHADOW_OUT );

    // final SashForm sash = new SashForm( this, SWT.VERTICAL );
    // sash.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    // if( toolkit != null )
    // toolkit.adapt( sash );

    createWaterTree( toolkit, this );
    // createWaterTree( toolkit, sash );
    // sash.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    // createStateTree( toolkit, sash );

    // FIXME: put into dialog settings
    // sash.setWeights( new int[] { 50, 50 } );

    createActions();

    // m_stateTreeUpdater = new StateTreeUpdater( connection, m_stateTree );

    refresh( null );
  }

  private void createWaterTree( final FormToolkit toolkit, final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    if( toolkit != null )
      toolkit.adapt( panel );

    GridLayoutFactory.fillDefaults().spacing( 0, 0 ).applyTo( panel );

    createToolbar( toolkit, panel ).setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    createStatusBar( toolkit, panel ).setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    createTreeViewer( toolkit, panel ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
  }

  // private void createStateTree( final FormToolkit toolkit, final Composite parent )
  // {
  // m_stateTree = new StateTreeComposite( toolkit, parent );
  // }

  private Control createStatusBar( final FormToolkit toolkit, final Composite parent )
  {
    m_statusBar = new StatusComposite( toolkit, parent, StatusComposite.DETAILS | StatusComposite.HIDE_DETAILS_IF_DISABLED );
    return m_statusBar;
  }

  @Override
  public void dispose( )
  {
    setInput( null );

    if( m_serviceLocator != null )
    {
      final IMenuService service = (IMenuService)m_serviceLocator.getService( IMenuService.class );
      service.releaseContributions( m_manager );
    }

    m_manager.dispose();

    super.dispose();
  }

  private Control createToolbar( final FormToolkit toolkit, final Composite parent )
  {
    final ToolBar toolBar = m_manager.createControl( parent );
    if( toolkit != null )
      toolkit.adapt( toolBar );
    return toolBar;
  }

  private Control createTreeViewer( final FormToolkit toolkit, final Composite parent )
  {
    m_waterViewer = createContentTree( toolkit, parent, null );
    m_waterViewer.setInput( PdbLabelProvider.PENDING );

    m_waterViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleSelectionChanged( (IStructuredSelection)event.getSelection() );
      }
    } );

    return m_waterViewer.getControl();
  }

  public static TreeViewer createContentTree( final FormToolkit toolkit, final Composite parent, final ILabelDecorator nameDecorator )
  {
    final Tree tree = new Tree( parent, SWT.FULL_SELECTION | SWT.MULTI | SWT.BORDER );
    if( toolkit != null )
      toolkit.adapt( tree, false, false );

    tree.setHeaderVisible( true );

    final TreeViewer viewer = new TreeViewer( tree );
    viewer.setUseHashlookup( true );
    viewer.setContentProvider( new ByWaterBodyContentProvider() );

    final ViewerColumn nameColumn = StatesViewer.createNameColumn( viewer, nameDecorator );
    WaterBodyViewer.createNameColumn( viewer );
//    StatesViewer.createMeasurementDateColumn( viewer );

    ColumnViewerSorter.setSortState( nameColumn, false );

    final ColumnsResizeControlListener treeListener = new ColumnsResizeControlListener();
    tree.addControlListener( treeListener );

    tree.addTreeListener( new TreeListener()
    {
      @Override
      public void treeExpanded( final TreeEvent e )
      {
        treeListener.updateColumnSizes();
      }

      @Override
      public void treeCollapsed( final TreeEvent e )
      {
        treeListener.updateColumnSizes();
      }
    } );

    return viewer;
  }

  protected void handleSelectionChanged( final IStructuredSelection selection )
  {
    // m_stateTreeUpdater.setSelection( selection );

    /* Preserve selection from user, i.e. if the selection is changed during refresh, use that new selection */
    final ElementSelector selector = new ElementSelector();
    selector.setElemensToSelect( selection.toArray() );
    m_refreshJob.setElementToSelect( selector );
  }

  private void createActions( )
  {
    m_manager.add( new RefreshAction( this ) );
    m_manager.add( new ExpandAllTreeItemsAction( m_waterViewer ) );
    m_manager.add( new CollapseAllTreeItemsAction( m_waterViewer ) );
    m_manager.add( new Separator() );

    if( m_serviceLocator != null )
    {
      final IMenuService service = (IMenuService)m_serviceLocator.getService( IMenuService.class );
      service.populateContributionManager( m_manager, TOOLBAR_URI );
    }

    m_manager.update( true );
  }

  public void refresh( final ElementSelector elementToSelect )
  {
    updateStatusBar( STATUS_UPDATE );

    // TODO: it would also be nice to directly update any changed elements
    // Hm, maybe we should directly always work on the current state?

    applySelection( elementToSelect );

    m_refreshJob.cancel();

    final Font italicFont = JFaceResources.getFontRegistry().getItalic( JFaceResources.DIALOG_FONT );
    m_waterViewer.getControl().setFont( italicFont );

    // m_refreshJob.setElementToSelect( elementToSelect );
    m_refreshJob.schedule( 500 );
  }

  protected synchronized void setInput( final Object input )
  {
    final Object oldInput = m_waterViewer.getInput();
    if( oldInput instanceof ConnectionInput )
      ((ConnectionInput)oldInput).dispose();

    m_waterViewer.setInput( input );
    refreshColumnSizes();
  }

  protected void handleRefreshDone( final IStatus status )
  {
    final ConnectionInput input = m_refreshJob.getInput();
    final ElementSelector selector = m_refreshJob.getElementToSelect();

    final TreeViewer viewer = m_waterViewer;
    final Runnable operation = new Runnable()
    {
      @Override
      public void run( )
      {
        setInput( input );

        final Font normalFont = JFaceResources.getFontRegistry().get( JFaceResources.DIALOG_FONT );
        viewer.getControl().setFont( normalFont );

        applySelection( selector );

        if( status.isOK() )
          refreshStatusBar();
        else
          updateStatusBar( status );
      }
    };
    ViewerUtilities.execute( m_waterViewer, operation, true );
  }

  protected void refreshStatusBar( )
  {
    final IConnectionContentProvider contentProvider = (IConnectionContentProvider)m_waterViewer.getContentProvider();
    final IStatus status = contentProvider.getInputStatus();
    updateStatusBar( status );
  }

  protected void updateStatusBar( final IStatus status )
  {
    m_statusBar.setStatus( status );

    final boolean wasVisible = m_statusBar.isVisible();
    final boolean isVisible = !status.isOK();

    m_statusBar.setVisible( isVisible );
    ((GridData)m_statusBar.getLayoutData()).exclude = !isVisible;

    if( isVisible != wasVisible )
      layout( true, true );
  }

  protected void applySelection( final ElementSelector selector )
  {
    final Object input = m_waterViewer.getInput();
    if( !(input instanceof ConnectionInput) )
      return;

    final Object[] element = selector == null ? ArrayUtils.EMPTY_OBJECT_ARRAY : selector.getElements( (ConnectionInput)input );
    if( element != null )
      ViewerUtilities.setSelection( m_waterViewer, new StructuredSelection( element ), true, true );

    refreshColumnSizes();
  }

  public IStructuredSelection getSelection( )
  {
    return (IStructuredSelection)m_waterViewer.getSelection();
  }

  protected void refreshColumnSizes( )
  {
    ColumnsResizeControlListener.refreshColumnsWidth( m_waterViewer.getTree() );
  }

  public TreeViewer getWatersViewer( )
  {
    return m_waterViewer;
  }

//  public TreeViewer getStatesViewer( )
//  {
//    return m_stateTree.getTreeViewer();
//  }

  public IWaterBodyStructure getStructure( )
  {
    if( m_waterViewer == null )
      return null;

    final ConnectionInput input = (ConnectionInput)m_waterViewer.getInput();
    if( input == null )
      return null;

    return input.getStructure();
  }
}