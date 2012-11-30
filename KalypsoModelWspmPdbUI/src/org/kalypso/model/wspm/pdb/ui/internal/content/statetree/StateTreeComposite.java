/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.pdb.ui.internal.content.statetree;

import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ILabelDecorator;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.TreeEvent;
import org.eclipse.swt.events.TreeListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.model.wspm.pdb.ui.internal.admin.state.StatesViewer;
import org.kalypso.model.wspm.pdb.ui.internal.content.ByWaterBodyContentProvider;
import org.kalypso.model.wspm.pdb.ui.internal.content.PdbLabelProvider;

/**
 * @author Gernot Belger
 */
public class StateTreeComposite extends Composite
{
  private TreeViewer m_stateViewer;

  public StateTreeComposite( final FormToolkit toolkit, final Composite parent )
  {
    super( parent, SWT.NONE );

    if( toolkit != null )
      toolkit.adapt( this );

    GridLayoutFactory.fillDefaults().spacing( 0, 0 ).applyTo( this );

    createContents( toolkit, this );
  }

  private void createContents( final FormToolkit toolkit, final Composite parent )
  {
    // TODO Auto-generated method stub
    createTreeViewer( toolkit, parent ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
  }

  private Control createTreeViewer( final FormToolkit toolkit, final Composite parent )
  {
    m_stateViewer = createContentTree( toolkit, parent, null );
    m_stateViewer.setInput( PdbLabelProvider.PENDING );

    m_stateViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleSelectionChanged( (IStructuredSelection)event.getSelection() );
      }
    } );

    return m_stateViewer.getControl();
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
    StatesViewer.createMeasurementDateColumn( viewer );

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

  /** directly set input without using external connection */
  public void setInput( final Object[] input )
  {
    if( isDisposed() )
      return;

    final Display display = getDisplay();

    final TreeViewer stateViewer = m_stateViewer;

    final Runnable runnable = new Runnable()
    {
      @Override
      public void run( )
      {
        if( stateViewer.getControl().isDisposed() )
          return;

        stateViewer.setInput( input );
        stateViewer.expandToLevel( 1 );
      }
    };

    display.asyncExec( runnable );
  }

  // TODO: instead bind to observable value
  protected void handleSelectionChanged( final IStructuredSelection selection )
  {
    // TODO Auto-generated method stub

  }

  public TreeViewer getTreeViewer( )
  {
    return m_stateViewer;
  }
}