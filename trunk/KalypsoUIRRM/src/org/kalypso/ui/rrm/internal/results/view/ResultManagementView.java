/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.contribs.eclipse.swt.layout.Layouts;
import org.kalypso.contribs.eclipse.ui.forms.ToolkitUtils;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.rrm.internal.results.view.tree.HydrologyResultFilter;
import org.kalypso.ui.rrm.internal.results.view.tree.NaModelStrategy;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeStrategy;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNode;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNodeContentProvider;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNodeLabelComparator;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNodeLabelProvider;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNodeModel;

/**
 * @author Gernot Belger
 */
public class ResultManagementView extends ViewPart
{
  public static String ID = "org.kalypso.ui.rrm.internal.results.view.ResultManagementView"; //$NON-NLS-1$

  private TreeViewer m_treeViewer;

  @Override
  public void createPartControl( final Composite parent )
  {
    final FormToolkit toolkit = ToolkitUtils.createToolkit( parent );

    final Composite body = toolkit.createComposite( parent );
    body.setLayout( Layouts.createGridLayout() );

    createResultTreeView( body ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
// createSearchControls( body, toolkit ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
  }

  private Composite createResultTreeView( final Composite parent )
  {
    final Composite tree = createTree( parent );
    getSite().setSelectionProvider( m_treeViewer );

    return tree;
  }

// private Control createSearchControls( final Composite parent, final FormToolkit toolkit )
// {
// final Section section = toolkit.createSection( parent, ExpandableComposite.TITLE_BAR | ExpandableComposite.TWISTIE |
// ExpandableComposite.EXPANDED );
//    section.setText( Messages.getString( "TimeseriesManagementView_0" ) ); //$NON-NLS-1$
// section.setLayout( new FillLayout() );
//
// final ToolBarManager toolbar = SectionUtils.createSectionToolbar( section );
//
// final TimeseriesBrowserSearchViewer searchPanel = new TimeseriesBrowserSearchViewer( section, toolkit, m_treeViewer
// );
// toolkit.adapt( searchPanel );
//
// toolbar.add( new CleanSearchPanelAction( searchPanel ) );
// toolbar.update( true );
//
// section.setClient( searchPanel );
//
// return section;
// }

  private Composite createTree( final Composite panel )
  {
    m_treeViewer = new TreeViewer( panel, SWT.FLAT | SWT.MULTI );
    m_treeViewer.setContentProvider( new TreeNodeContentProvider() );
    m_treeViewer.setLabelProvider( new TreeNodeLabelProvider() );
    m_treeViewer.setComparator( new TreeNodeLabelComparator() );
    m_treeViewer.addFilter( new HydrologyResultFilter() );

    return m_treeViewer.getTree();
  }

  @Override
  public void setFocus( )
  {
    m_treeViewer.getControl().setFocus();
  }

  public void setInput( final CommandableWorkspace workspace, final NaModell model )
  {
    final ITreeNodeStrategy strategy = new NaModelStrategy( model );

    final TreeNodeModel input = new TreeNodeModel( strategy, workspace, m_treeViewer );
    m_treeViewer.setInput( input );

    /** set tree viewer selection to it's first item! */
    new UIJob( "" ) //$NON-NLS-1$
    {
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {
        final TreeNode[] elements = input.getRootElements();
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
}