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

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.model.WorkbenchContentProvider;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.kalypso.contribs.eclipse.jface.action.ActionButton;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;

/**
 * Wizard page for displaying the result database in a checkbox-treeview Components are a {@link CheckboxTreeViewer} and
 * an {@link ResultMetaInfoViewer}. <br>
 * optional: The result info viewer can be given a {@link IThemeConstructionFactory} for displaying special button /
 * combo components for sld handling displayed inside the info viewer.
 * 
 * @author Thomas Jung
 */
public class SelectResultWizardPage extends WizardPage
{
  private final Collection<IAction> m_actions = new ArrayList<>();

  // TODO: most use cases of result viewer only need the information about a result node, not creation of a theme. We should separate these concerns.
  private final IThemeConstructionFactory m_factory;

  private IResultMeta m_resultRoot;

  private CheckboxTreeViewer m_treeViewer;

  private Object[] m_checkedElements = null;

  private ViewerFilter m_filter;

  private ViewerComparator m_comparator;

  public SelectResultWizardPage( final String pageName, final String title, final IThemeConstructionFactory factory )
  {
    super( pageName );

    setTitle( title );

    m_factory = factory;

    setDescription( Messages.getString( "org.kalypso.ui.wizards.results.SelectResultWizardPage.0" ) ); //$NON-NLS-1$
  }

  public void setFilter( final ViewerFilter filter )
  {
    m_filter = filter;

    if( m_treeViewer != null )
      m_treeViewer.setFilters( new ViewerFilter[] { m_filter } );
  }

  public void setComparator( final ViewerComparator comparator )
  {
    m_comparator = comparator;

    if( m_treeViewer != null )
      m_treeViewer.setComparator( m_comparator );
  }

  /**
   * Adds an action to the toolbar. Must be called before {@link #createControl(Composite)} is called.
   */
  protected final void addAction( final IAction action )
  {
    Assert.isTrue( m_treeViewer == null );

    m_actions.add( action );
  }

  public void setResultMeta( final IResultMeta resultRoot )
  {
    m_resultRoot = resultRoot;

    if( m_treeViewer != null )
      m_treeViewer.setInput( resultRoot );
  }

  public IResultMeta getResultRoot( )
  {
    return m_resultRoot;
  }

  @Override
  public void createControl( final Composite parent )
  {
    /* set a fixed size to the Wizard */
    // HACK!!
    // FIXME: bad -> do this in a different way!
    final Object layoutData = parent.getLayoutData();
    if( layoutData instanceof GridData )
    {
      final GridData pLayout = (GridData)layoutData;
      pLayout.widthHint = 700;
      pLayout.heightHint = 400;
      parent.layout();
    }

    final Composite panel = new Composite( parent, SWT.NONE );
    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( panel );

    m_treeViewer = new CheckboxTreeViewer( panel, SWT.BORDER );
    m_treeViewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    m_treeViewer.setContentProvider( new WorkbenchContentProvider() );
    m_treeViewer.setLabelProvider( new WorkbenchLabelProvider() );

    if( m_filter != null )
      m_treeViewer.setFilters( new ViewerFilter[] { m_filter } );

    m_treeViewer.setComparator( m_comparator );

    m_treeViewer.setInput( m_resultRoot );

    /* The next two lines are needed so that checking children of checked elements always works. */
    // FIXME: only, because getParent on the content provider does not work correctly, we should fix that
    m_treeViewer.expandAll();
    m_treeViewer.collapseAll();

    /* Info View for one result */
    final ResultMetaInfoViewer resultViewer = new ResultMetaInfoViewer( panel, SWT.NONE, m_factory );
    resultViewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    createToolbar( panel ).setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );

    // TODO: allow user to set an individually name of the difference result

    m_treeViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleSelectionChanged( (IStructuredSelection)event.getSelection(), resultViewer );
      }
    } );

    m_treeViewer.addCheckStateListener( new ICheckStateListener()
    {
      @Override
      public void checkStateChanged( final CheckStateChangedEvent event )
      {
        final IResultMeta resultMeta = (IResultMeta)event.getElement();
        final boolean isChecked = event.getChecked();
        handleCheckStateChanged( resultMeta, isChecked );
      }
    } );

    /* Check elements if any defined */
    if( m_checkedElements != null )
    {
      final ITreeContentProvider contentProvider = (ITreeContentProvider)m_treeViewer.getContentProvider();
      for( final Object elementToCheck : m_checkedElements )
      {
        final Object parentToExpand = contentProvider.getParent( elementToCheck );
        if( parentToExpand != null )
          m_treeViewer.expandToLevel( parentToExpand, 1 );
      }
      m_treeViewer.setCheckedElements( m_checkedElements );
    }

    setControl( panel );
  }

  private Control createToolbar( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new RowLayout() );

    for( final IAction action : m_actions )
      ActionButton.createButton( null, panel, action );

    return panel;
  }

  protected void handleSelectionChanged( final IStructuredSelection selection, final ResultMetaInfoViewer resultViewer )
  {
    resultViewer.setInput( selection.getFirstElement() );
  }

  public IResultMeta[] getSelectedResults( )
  {
    final Object[] checkedElements = m_treeViewer.getCheckedElements();
    final IResultMeta[] resultArray = new IResultMeta[checkedElements.length];
    for( int i = 0; i < checkedElements.length; i++ )
      resultArray[i] = (IResultMeta)checkedElements[i];
    return resultArray;
  }

  protected void handleCheckStateChanged( final IResultMeta resultMeta, final boolean isChecked )
  {
    m_treeViewer.setSubtreeChecked( resultMeta, isChecked );
    getContainer().updateButtons();
  }

  public IThemeConstructionFactory getThemeFactory( )
  {
    return m_factory;
  }

  /**
   * The elements which should initially be checked.
   * <p>
   * This method must be called before createControl is invoked.
   * </p>
   */
  public void setInitialCheckedElements( final Object[] checkedElements )
  {
    m_checkedElements = checkedElements;
  }

  // TODO: not nice... currently use to refresh tree
  public CheckboxTreeViewer getTreeViewer( )
  {
    return m_treeViewer;
  }
}