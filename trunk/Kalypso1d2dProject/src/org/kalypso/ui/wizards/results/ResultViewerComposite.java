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
package org.kalypso.ui.wizards.results;

import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.model.WorkbenchContentProvider;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;

/**
 * @author jung
 * 
 */
public class ResultViewerComposite
{

  private final IThemeConstructionFactory m_factory;

  private final ViewerFilter m_filter;

  private IResultMeta m_resultRoot;

  private CheckboxTreeViewer m_treeViewer;

  private final ViewerComparator m_comparator;

  private Object[] m_checkedElements;

  public ResultViewerComposite( final IResultMeta resultRoot, final ViewerFilter filter, final ViewerComparator comparator, final IThemeConstructionFactory factory )
  {
    m_resultRoot = resultRoot;
    m_filter = filter;
    m_comparator = comparator;
    m_factory = factory;
  }

  public Composite createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new FillLayout( SWT.HORIZONTAL ) );

    m_treeViewer = new CheckboxTreeViewer( panel, SWT.BORDER );
    m_treeViewer.setContentProvider( new WorkbenchContentProvider() );
    m_treeViewer.setLabelProvider( new WorkbenchLabelProvider() );
    m_treeViewer.addFilter( m_filter );
    m_treeViewer.setComparator( m_comparator );
    m_treeViewer.setInput( m_resultRoot );

    /* The next two lines are needed so that checking children of checked elements always works. */
    m_treeViewer.expandAll();
    m_treeViewer.collapseAll();

    /* Info View for one result */
    final ResultMetaInfoViewer resultViewer = new ResultMetaInfoViewer( panel, SWT.NONE, m_factory );

    m_treeViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleSelectionChanged( (IStructuredSelection) event.getSelection(), resultViewer );
      }
    } );

    m_treeViewer.addCheckStateListener( new ICheckStateListener()
    {
      @Override
      public void checkStateChanged( final CheckStateChangedEvent event )
      {
        final IResultMeta resultMeta = (IResultMeta) event.getElement();
        final boolean isChecked = event.getChecked();
        handleCheckStateChanged( resultMeta, isChecked );
      }
    } );

    /* Check elements if any defined */
    if( m_checkedElements != null )
    {
      final ITreeContentProvider contentProvider = (ITreeContentProvider) m_treeViewer.getContentProvider();
      for( final Object elementToCheck : m_checkedElements )
      {
        final Object parentToExpand = contentProvider.getParent( elementToCheck );
        if( parentToExpand != null )
          m_treeViewer.expandToLevel( parentToExpand, 1 );
      }
      m_treeViewer.setCheckedElements( m_checkedElements );
    }
    return panel;
  }

  protected void handleSelectionChanged( final IStructuredSelection selection, final ResultMetaInfoViewer resultViewer )
  {
    resultViewer.setInput( selection.getFirstElement() );
  }

  protected void handleCheckStateChanged( final IResultMeta resultMeta, final boolean isChecked )
  {
    m_treeViewer.setSubtreeChecked( resultMeta, isChecked );
  }

  public void setResultRoot( final IResultMeta resultRoot )
  {
    m_resultRoot = resultRoot;

    if( m_treeViewer != null )
      m_treeViewer.setInput( resultRoot );
  }

  public IResultMeta getResultRoot( )
  {
    return m_resultRoot;
  }

  public IResultMeta[] getSelectedResults( )
  {
    final Object[] checkedElements = m_treeViewer.getCheckedElements();
    final IResultMeta[] resultArray = new IResultMeta[checkedElements.length];
    for( int i = 0; i < checkedElements.length; i++ )
      resultArray[i] = (IResultMeta) checkedElements[i];
    return resultArray;
  }

  public CheckboxTreeViewer getTreeViewer( )
  {
    return m_treeViewer;
  }

  public void setInitialCheckedElements( final Object[] checkedElements )
  {
    m_checkedElements = checkedElements;
  }

}