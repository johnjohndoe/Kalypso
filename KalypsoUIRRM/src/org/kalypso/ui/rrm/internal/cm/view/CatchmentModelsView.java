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
package org.kalypso.ui.rrm.internal.cm.view;

import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.handlers.CollapseAllHandler;
import org.eclipse.ui.handlers.ExpandAllHandler;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.model.hydrology.binding.cm.ICatchmentModel;
import org.kalypso.model.hydrology.binding.timeseriesMappings.ITimeseriesMappingCollection;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeStrategy;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNodeContentProvider;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNodeLabelComparator;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNodeLabelProvider;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNodeModel;

/**
 * @author Gernot Belger
 */
public class CatchmentModelsView extends ViewPart
{
  public static String ID = "org.kalypso.ui.rrm.internal.cm.view.CatchmentModelsView"; //$NON-NLS-1$

  private TreeViewer m_treeViewer;

  private CollapseAllHandler m_collapseHandler;

  private ExpandAllHandler m_expandHandler;

  @Override
  public void createPartControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );

    GridLayoutFactory.fillDefaults().applyTo( panel );

    createTree( panel ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    final IWorkbenchPartSite site = getSite();

    site.setSelectionProvider( m_treeViewer );

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

  private Control createTree( final Composite panel )
  {
    m_treeViewer = new TreeViewer( panel );
    m_treeViewer.setContentProvider( new TreeNodeContentProvider() );
    m_treeViewer.setLabelProvider( new TreeNodeLabelProvider() );
    m_treeViewer.setComparator( new TreeNodeLabelComparator() );

    return m_treeViewer.getTree();
  }

  @Override
  public void setFocus( )
  {
    m_treeViewer.getControl().setFocus();
  }

  public void setInput( final ICatchmentModel model, final ITimeseriesMappingCollection timeseriesMappings, final CommandableWorkspace... workspaces )
  {
    final ITreeNodeStrategy strategy = new TimeseriesMappingsTreeStrategy( model, timeseriesMappings );
    final TreeNodeModel input = new TreeNodeModel( strategy, m_treeViewer, workspaces );

    m_treeViewer.setInput( input );
  }

  @Override
  public Object getAdapter( final Class adapter )
  {
    if( adapter == TreeViewer.class )
      return m_treeViewer;

    return super.getAdapter( adapter );
  }
}