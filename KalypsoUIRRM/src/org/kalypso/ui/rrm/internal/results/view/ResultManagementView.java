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
package org.kalypso.ui.rrm.internal.results.view;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.handlers.CollapseAllHandler;
import org.eclipse.ui.handlers.ExpandAllHandler;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.contribs.eclipse.swt.widgets.SectionUtils;
import org.kalypso.contribs.eclipse.ui.forms.ToolkitUtils;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.results.view.tree.filter.CleanSearchPanelAction;
import org.kalypso.ui.rrm.internal.results.view.tree.filter.HydrolgyManagementSearchControl;
import org.kalypso.ui.rrm.internal.results.view.tree.filter.IRrmDiagramFilterControl;
import org.kalypso.ui.rrm.internal.results.view.tree.strategies.NaModelStrategy;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeStrategy;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNode;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNodeContentProvider;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNodeLabelComparator;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNodeLabelProvider;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNodeModel;

/**
 * @author Dirk Kuch
 */
public class ResultManagementView extends ViewPart
{
  public static String ID = "org.kalypso.ui.rrm.internal.results.view.ResultManagementView"; //$NON-NLS-1$

  protected TreeViewer m_treeViewer;

  protected final TreeViewerSelectionStack m_stack = new TreeViewerSelectionStack( 3 );

  private HydrolgyManagementSearchControl m_searchPanel;

  private RrmScenario m_scenario;

  private TreeNodeModel m_model;

  private CollapseAllHandler m_collapseHandler;

  private ExpandAllHandler m_expandHandler;

  @Override
  public void createPartControl( final Composite parent )
  {
    final FormToolkit toolkit = ToolkitUtils.createToolkit( parent );

    final Composite body = toolkit.createComposite( parent );
    GridLayoutFactory.fillDefaults().applyTo( body );

    createResultTreeView( body ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    createSearchControls( body, toolkit ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    /* register tree handlers */
    final IHandlerService handlerService = (IHandlerService)getSite().getService( IHandlerService.class );

    m_collapseHandler = new CollapseAllHandler( m_treeViewer );
    m_expandHandler = new ExpandAllHandler( m_treeViewer );

    handlerService.activateHandler( CollapseAllHandler.COMMAND_ID, m_collapseHandler );
    handlerService.activateHandler( ExpandAllHandler.COMMAND_ID, m_expandHandler );
  }

  @Override
  public void dispose( )
  {
    m_collapseHandler.dispose();
    m_expandHandler.dispose();

    super.dispose();
  }

  public TreeViewerSelectionStack getSelectionStack( )
  {
    return m_stack;
  }

  private Composite createResultTreeView( final Composite parent )
  {
    final Composite tree = createTree( parent );
    getSite().setSelectionProvider( m_treeViewer );

    return tree;
  }

  private Control createSearchControls( final Composite parent, final FormToolkit toolkit )
  {
    final Section section = toolkit.createSection( parent, ExpandableComposite.TITLE_BAR | ExpandableComposite.TWISTIE | ExpandableComposite.EXPANDED );
    section.setText( Messages.getString( "TimeseriesManagementView_0" ) ); //$NON-NLS-1$
    section.setLayout( new FillLayout() );

    final ToolBarManager toolbar = SectionUtils.createSectionToolbar( section );

    m_searchPanel = new HydrolgyManagementSearchControl( section, toolkit, m_treeViewer );
    toolkit.adapt( m_searchPanel );

    toolbar.add( new CleanSearchPanelAction( m_searchPanel ) );
    toolbar.update( true );

    section.setClient( m_searchPanel );

    return section;
  }

  private Composite createTree( final Composite panel )
  {
    m_treeViewer = new TreeViewer( panel, SWT.FLAT | SWT.MULTI )
    {
      @Override
      public ISelection getSelection( )
      {
        return m_stack.getSelection( (IStructuredSelection)super.getSelection() );
      }
    };

    final TreeNodeLabelProvider labelProvider = new TreeNodeLabelProvider();
    labelProvider.setSelectionStack( m_stack );

    m_treeViewer.setContentProvider( new TreeNodeContentProvider() );
    m_treeViewer.setLabelProvider( labelProvider );
    m_treeViewer.setComparator( new TreeNodeLabelComparator() );
    m_treeViewer.addDoubleClickListener( new IDoubleClickListener()
    {
      @Override
      public void doubleClick( final DoubleClickEvent event )
      {
        final TreeNode[] changes = m_stack.add( (IStructuredSelection)event.getSelection() );
        for( final TreeNode changed : changes )
          m_treeViewer.refresh( changed );
      }
    } );

    return m_treeViewer.getTree();
  }

  @Override
  public void setFocus( )
  {
    m_treeViewer.getControl().setFocus();
  }

  public void setInput( final RrmScenario scenario )
  {
    m_scenario = scenario;
    final ITreeNodeStrategy strategy = new NaModelStrategy( scenario, this );

    m_model = new TreeNodeModel( strategy, m_treeViewer );
    m_treeViewer.setInput( m_model );

    /** set tree viewer selection to it's first item! */
    final TreeNodeModel model = m_model;
    new UIJob( "" ) //$NON-NLS-1$
    {
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {
        final TreeNode[] elements = model.getRootElements();
        if( ArrayUtils.isNotEmpty( elements ) )
          m_treeViewer.setSelection( new StructuredSelection( elements[0] ) );

        return Status.OK_STATUS;
      }
    }.schedule();
  }

  public TreeViewer getTreeViewer( )
  {
    return m_treeViewer;
  }

  public TreeNodeModel getModel( )
  {
    return m_model;
  }

  public IRrmDiagramFilterControl getFilterControl( )
  {
    return m_searchPanel;
  }

  @Override
  public Object getAdapter( final Class adapter )
  {
    if( adapter == TreeViewer.class )
      return m_treeViewer;
    else if( adapter == TreeViewerSelectionStack.class )
      return m_stack;

    return super.getAdapter( adapter );
  }

  public void refresh( )
  {
    setInput( m_scenario );
  }

  public IStructuredSelection getSelection( )
  {
    return (IStructuredSelection)m_treeViewer.getSelection();
  }
}