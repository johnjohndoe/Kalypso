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
package org.kalypso.ui.rrm.internal.diagram;

import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.contribs.eclipse.swt.layout.Layouts;
import org.kalypso.contribs.eclipse.ui.forms.ToolkitUtils;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNode;
import org.kalypso.zml.core.base.IMultipleZmlSourceElement;
import org.kalypso.zml.core.base.selection.ZmlSelectionBuilder;
import org.kalypso.zml.ui.chart.view.ZmlDiagramChartPartComposite;

/**
 * @author Gernot Belger
 */
public class RrmDiagramView extends ViewPart
{
  public static String ID = "org.kalypso.ui.rrm.internal.diagram.RrmDiagramView"; //$NON-NLS-1$

  private final ISelectionChangedListener m_selectionListener = new ISelectionChangedListener()
  {
    @Override
    public void selectionChanged( final SelectionChangedEvent event )
    {
      handleSelectionChanged( (IStructuredSelection) event.getSelection() );
    }
  };

  private TreeNode m_node;

  private ZmlDiagramChartPartComposite m_chartPart;

  @Override
  public void createPartControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    GridLayoutFactory.fillDefaults().applyTo( panel );

    createDiagram( panel );

// getSite().setSelectionProvider( m_treeViewer );
  }

  protected void handleSelectionChanged( final IStructuredSelection selection )
  {
    final IMultipleZmlSourceElement[] sources = ZmlSelectionBuilder.getSelection( selection );
    m_chartPart.setSelection( sources );
  }

  public void hookSelection( final ISelectionProvider provider )
  {
    provider.addSelectionChangedListener( m_selectionListener );

    setNode( null );
  }

  private void setNode( final TreeNode node )
  {
    m_node = node;

    updateControl();
  }

  private void updateControl( )
  {
    // TODO Auto-generated method stub

  }

  private void createDiagram( final Composite panel )
  {
    final FormToolkit toolkit = ToolkitUtils.createToolkit( panel );

    final Composite base = toolkit.createComposite( panel, SWT.RIGHT | SWT.EMBEDDED | SWT.BORDER );
    final GridLayout layout = Layouts.createGridLayout();
    layout.verticalSpacing = 0;
    base.setLayout( layout );
    base.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

    m_chartPart = new ZmlDiagramChartPartComposite( this, getClass().getResource( "templates/diagram.kod" ) ); //$NON-NLS-1$
    m_chartPart.createControl( base, toolkit );
  }

  @Override
  public void setFocus( )
  {
    final Composite composite = (Composite) m_chartPart.getChartComposite();
    composite.setFocus();
  }
}