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

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.databinding.viewers.IViewerObservableValue;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.contribs.eclipse.jface.viewers.tree.CollapseAllTreeItemsAction;
import org.kalypso.contribs.eclipse.jface.viewers.tree.ExpandAllTreeItemsAction;
import org.kalypso.contribs.eclipse.jface.viewers.tree.ITreeViewerProvider;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;

/**
 * Wizard page for displaying the result database in a checkbox-treeview Components are a {@link CheckboxTreeViewer} and
 * an {@link ResultMetaInfoViewer}. <br>
 * optional: The result info viewer can be given a {@link IThemeConstructionFactory} for displaying special button /
 * combo components for sld handling displayed inside the info viewer.
 * 
 * @author Thomas Jung
 */
public class SelectResultWizardPage extends WizardPage implements ITreeViewerProvider
{
  private final ToolBarManager m_toolbarManager = new ToolBarManager( SWT.FLAT );

  private final Collection<IAction> m_actions = new ArrayList<>();

  private final SelectResultData m_data;

  // TODO: most use cases of result viewer only need the information about a result node, not creation of a theme. We should separate these concerns.
  private IThemeConstructionFactory m_factory;

  private CheckboxTreeViewer m_treeViewer;

  private Object[] m_checkedElements = null;

  private ViewerFilter m_filter;

  private DatabindingWizardPage m_binding;

  public SelectResultWizardPage( final String pageName, final String title, final SelectResultData data )
  {
    super( pageName );

    m_data = data;

    setTitle( title );
    setDescription( Messages.getString( "org.kalypso.ui.wizards.results.SelectResultWizardPage.0" ) ); //$NON-NLS-1$

    addAction( new CollapseAllTreeItemsAction( this ) );
    addAction( new ExpandAllTreeItemsAction( this ) );
    // separator
    addAction( null );
  }

  @Override
  public void dispose( )
  {
    if( m_binding != null )
      m_binding.dispose();

    m_toolbarManager.dispose();

    super.dispose();
  }

  public void setFactory( final IThemeConstructionFactory factory )
  {
    Assert.isTrue( m_treeViewer == null );

    m_factory = factory;
  }

  public void setFilter( final ViewerFilter filter )
  {
    Assert.isTrue( m_treeViewer == null );

    m_filter = filter;
  }

  /**
   * Adds an action to the toolbar. Must be called before {@link #createControl(Composite)} is called.
   */
  public final void addAction( final IAction action )
  {
    Assert.isTrue( m_treeViewer == null );

    m_actions.add( action );
  }

  @Override
  public void createControl( final Composite parent )
  {
    m_binding = new DatabindingWizardPage( this, null );

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
    setControl( panel );

    GridLayoutFactory.swtDefaults().numColumns( 2 ).equalWidth( true ).applyTo( panel );

    /* tree panel */
    createTreeControls( panel ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* Info View for one result */
    final ResultMetaInfoViewer resultViewer = new ResultMetaInfoViewer( panel, SWT.NONE, m_factory );
    resultViewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    // TODO: allow user to set an individually name of the difference result

    m_treeViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection)event.getSelection();
        resultViewer.setInput( selection.getFirstElement() );
      }
    } );

    // FIXME: use checkstate provider and delegate behavior to various strategies that also validate the current check state
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
  }

  private Control createTreeControls( final Composite parent )
  {
    final Composite treePanel = new Composite( parent, SWT.NONE );
    GridLayoutFactory.fillDefaults().spacing( 0, 0 ).applyTo( treePanel );

    createToolbar( treePanel ).setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    createTree( treePanel ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    if( m_data.getShowOptions() )
      createFilterControls( treePanel ).setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    m_treeViewer.getControl().setFocus();

    return treePanel;
  }

  private Control createFilterControls( final Composite parent )
  {
    final Group panel = new Group( parent, SWT.NONE );
    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( panel );

    panel.setText( "Filter" );

    final Label label = new Label( panel, SWT.NONE );
    label.setText( "Show" );

    final ComboViewer combo = new ComboViewer( panel, SWT.DROP_DOWN | SWT.READ_ONLY );
    combo.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    combo.setContentProvider( new ArrayContentProvider() );
    combo.setLabelProvider( new LabelProvider() );
    combo.setInput( SelectResultData.ShowType.values() );

    /* binding */
    final IViewerObservableValue targetSelection = ViewersObservables.observeSinglePostSelection( combo );
    final IObservableValue modelSelection = BeansObservables.observeValue( m_data, SelectResultData.PROPERTY_SHOW_ALL );
    m_binding.bindValue( targetSelection, modelSelection );

    return panel;
  }

  private Control createTree( final Composite parent )
  {
    final IScenarioResultMeta currentScenarioResult = m_data.getCurrentScenarioResult();

    m_treeViewer = new CheckboxTreeViewer( parent, SWT.BORDER );

    m_treeViewer.setContentProvider( new ResultMetaContentProvider( currentScenarioResult ) );
    m_treeViewer.setLabelProvider( WorkbenchLabelProvider.getDecoratingWorkbenchLabelProvider() );
    m_treeViewer.setComparator( new Result1d2dMetaComparator() );

    m_treeViewer.addFilter( new Result1d2dMetaFilter() );
    if( m_filter != null )
      m_treeViewer.addFilter( m_filter );

    /* binding */
    final IObservableValue targetInput = ViewersObservables.observeInput( m_treeViewer );
    final IObservableValue modelInput = BeansObservables.observeValue( m_data, SelectResultData.PROPERTY_RESULT_ROOT );
    m_binding.bindValue( targetInput, modelInput );

    /* The next two lines are needed so that checking children of checked elements always works. */
    // FIXME: only, because getParent on the content provider does not work correctly, we should fix that
    m_treeViewer.expandAll();
    m_treeViewer.collapseAll();

    return m_treeViewer.getControl();
  }

  private Control createToolbar( final Composite parent )
  {
    for( final IAction action : m_actions )
    {
      if( action == null )
        m_toolbarManager.add( new Separator( "xxx" ) );
      else
        m_toolbarManager.add( action );
    }

    return m_toolbarManager.createControl( parent );
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

  // TODO: not nice... currently used to refresh tree
  @Override
  public TreeViewer getTreeViewer( )
  {
    return m_treeViewer;
  }
}