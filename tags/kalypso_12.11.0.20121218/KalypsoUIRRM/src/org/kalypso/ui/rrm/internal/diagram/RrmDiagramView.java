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
package org.kalypso.ui.rrm.internal.diagram;

import org.apache.commons.lang3.ObjectUtils;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.contribs.eclipse.ui.forms.ToolkitUtils;
import org.kalypso.ui.rrm.internal.results.view.tree.filter.IRrmDiagramFilterControl;
import org.kalypso.zml.core.base.IMultipleZmlSourceElement;
import org.kalypso.zml.core.base.selection.ZmlSelectionBuilder;
import org.kalypso.zml.ui.chart.view.ZmlDiagramChartPartComposite;

import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.layer.manager.AbstractChartLayerVisitor;

/**
 * @author Gernot Belger
 */
public class RrmDiagramView extends ViewPart
{
  public static String ID = "org.kalypso.ui.rrm.internal.diagram.RrmDiagramView"; //$NON-NLS-1$

  private int m_minTraverseLevel = 2;

  private final ISelectionChangedListener m_selectionListener = new ISelectionChangedListener()
  {
    @Override
    public void selectionChanged( final SelectionChangedEvent event )
    {
      handleSelectionChanged( (IStructuredSelection) event.getSelection() );
    }
  };

  private ZmlDiagramChartPartComposite m_chartPart;

  private IRrmDiagramFilterControl m_selectionFilterControl;

  private IStructuredSelection m_lastSelection;

  @Override
  public void createPartControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    GridLayoutFactory.fillDefaults().applyTo( panel );

    createDiagram( panel );
  }

  public void setSelectionTraverseLevel( final int level )
  {
    m_minTraverseLevel = level;
  }

  @Override
  public void init( final IViewSite site ) throws PartInitException
  {
    super.init( site );

    m_chartPart = new ZmlDiagramChartPartComposite( this, getClass().getResource( "/etc/timeseries/diagram.kod" ) ); //$NON-NLS-1$
    m_chartPart.init( site );
  }

  protected void handleSelectionChanged( final IStructuredSelection selection )
  {
    final RrmDiagramSelectionConverter converter = new RrmDiagramSelectionConverter( m_selectionFilterControl, m_minTraverseLevel );
    final IStructuredSelection converted = converter.doConvert( selection );

    if( ObjectUtils.equals( m_lastSelection, converted ) )
      return;

    m_lastSelection = converted;

    final IMultipleZmlSourceElement[] sources = ZmlSelectionBuilder.getSelection( converted );
    m_chartPart.setSelection( sources );
  }

  public void hookSelection( final ISelectionProvider provider )
  {
    provider.addSelectionChangedListener( m_selectionListener );
  }

  private void createDiagram( final Composite panel )
  {
    final FormToolkit toolkit = ToolkitUtils.createToolkit( panel );

    final Composite base = toolkit.createComposite( panel, SWT.RIGHT | SWT.EMBEDDED | SWT.BORDER );
    GridLayoutFactory.fillDefaults().applyTo( base );
    base.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

    m_chartPart.createControl( base );

    m_chartPart.getChartModel().getLayerManager().accept( new AbstractChartLayerVisitor()
    {
      @Override
      public void visit( final IChartLayer layer )
      {
        layer.setVisible( false );
      }
    } );
  }

  @Override
  public void setFocus( )
  {
    final Composite composite = (Composite) m_chartPart.getChartComposite();
    if( composite != null && !composite.isDisposed() )
      composite.setFocus();
  }

  public void setSelectionFilter( final IRrmDiagramFilterControl selectionFilterControl )
  {
    m_selectionFilterControl = selectionFilterControl;
  }
}