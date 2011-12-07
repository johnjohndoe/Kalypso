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
package org.kalypso.ui.rrm.internal.timeseries.view;

import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.rrm.internal.timeseries.binding.StationCollection;

/**
 * @author Gernot Belger
 */
public class TimeseriesManagementView extends ViewPart
{
  public static String ID = "org.kalypso.ui.rrm.internal.timeseries.view.TimeseriesManagementView"; //$NON-NLS-1$

  private TreeViewer m_treeViewer;

  @Override
  public void createPartControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );

    GridLayoutFactory.swtDefaults().applyTo( panel );

    createTree( panel ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    getSite().setSelectionProvider( m_treeViewer );
  }

  private Control createTree( final Composite panel )
  {
    m_treeViewer = new TreeViewer( panel );

    m_treeViewer.setContentProvider( new StationsContentProvider() );

    m_treeViewer.setLabelProvider( new TimeseriesNodeLabelProvider() );

// ColumnViewerUtil.createEmptyColumn( m_treeViewer );
//
// /* Label Column */
// final ViewerColumn labelColumn = ColumnViewerUtil.createViewerColumn( m_treeViewer, SWT.LEFT );
// labelColumn.setLabelProvider( new TimeseriesNodeLabelProvider() );
// final ViewerColumnItem labelColumnItem = new ViewerColumnItem( labelColumn );
// labelColumnItem.setText( "Name" );
// labelColumnItem.setResizable( false );
//
// ColumnsResizeControlListener.setMinimumPackWidth( labelColumnItem.getColumn() );
// ColumnViewerSorter.registerSorter( labelColumn, new TimeseriesNodeLabelComparator() );
//
// /* Identifier Column */
// final ViewerColumn idColumn = ColumnViewerUtil.createViewerColumn( m_treeViewer, SWT.LEFT );
// idColumn.setLabelProvider( new TimeseriesNodeIdProvider() );
//
// final ViewerColumnItem idColumnItem = new ViewerColumnItem( idColumn );
// idColumnItem.setText( "Identifier" );
// idColumnItem.setResizable( false );
//
// ColumnsResizeControlListener.setMinimumPackWidth( idColumnItem.getColumn() );
// ColumnViewerSorter.registerSorter( idColumn, new TimeseriesNodeLabelComparator() );

    final Tree tree = m_treeViewer.getTree();
    // tree.setHeaderVisible( true );
    // tree.setLinesVisible( true );
    // tree.addControlListener( new ColumnsResizeControlListener() );

    return tree;
  }

  @Override
  public void setFocus( )
  {
    m_treeViewer.getControl().setFocus();
  }

  public void setInput( final CommandableWorkspace workspace, final StationCollection stations )
  {
    final StationsByStationModel input = new StationsByStationModel( workspace, stations );

    m_treeViewer.setInput( input );
  }
}