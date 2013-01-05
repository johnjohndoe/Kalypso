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
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;

/**
 * Encapsulates the tree, toolbar and filter controls of the result selection pages.<br/>
 * 
 * @author Gernot Belger
 */
public class SelectResultTreeComposite
{
  private final ToolBarManager m_toolbarManager = new ToolBarManager( SWT.FLAT );

  private final SelectResultData m_data;

  private final Collection<IAction> m_actions = new ArrayList<>();

  private ViewerFilter m_filter;

  private TreeViewer m_treeViewer;

  public SelectResultTreeComposite( final SelectResultData data )
  {
    m_data = data;
  }

  public void dispose( )
  {
    m_toolbarManager.dispose();
  }

  /**
   * <dl>
   * <dt><b>Styles:</b></dt>
   * <dd>CHECK</dd>
   * </dl>
   */
  public Control createControls( final IDataBinding binding, final Composite parent, final int style )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    GridLayoutFactory.fillDefaults().spacing( 0, 0 ).applyTo( panel );

    createToolbar( panel ).setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Control treeControl = createTree( binding, panel, style );
    final GridData treeData = new GridData( SWT.FILL, SWT.FILL, true, true );
    // REMARK: ensure a minimum width of all controls when the wizard is opened the first time
    treeData.heightHint = 300;
    treeData.widthHint = 400;
    treeControl.setLayoutData( treeData );

    if( m_data.getShowOptions() )
      createFilterControls( binding, panel ).setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    return panel;
  }

  private Control createToolbar( final Composite parent )
  {
    for( final IAction action : m_actions )
    {
      if( action == null )
        m_toolbarManager.add( new Separator() );
      else
        m_toolbarManager.add( action );
    }

    return m_toolbarManager.createControl( parent );
  }

  private Control createTree( final IDataBinding binding, final Composite parent, final int style )
  {
    final IScenarioResultMeta currentScenarioResult = m_data.getCurrentScenarioResult();

    if( (style & SWT.CHECK) != 0 )
      m_treeViewer = new CheckboxTreeViewer( parent, style );
    else
      m_treeViewer = new TreeViewer( parent, style );

    m_treeViewer.setContentProvider( new ResultMetaContentProvider( currentScenarioResult ) );
    m_treeViewer.setLabelProvider( WorkbenchLabelProvider.getDecoratingWorkbenchLabelProvider() );
    m_treeViewer.setComparator( new Result1d2dMetaComparator() );

    m_treeViewer.addFilter( new Result1d2dMetaFilter() );
    if( m_filter != null )
      m_treeViewer.addFilter( m_filter );

    /* binding */
    final IObservableValue targetInput = ViewersObservables.observeInput( m_treeViewer );
    final IObservableValue modelInput = BeansObservables.observeValue( m_data, SelectResultData.PROPERTY_RESULT_ROOT );
    binding.bindValue( targetInput, modelInput );

    final IViewerObservableValue targetSelection = ViewersObservables.observeSinglePostSelection( m_treeViewer );
    final IObservableValue modelSelection = BeansObservables.observeValue( m_data, SelectResultData.PROPERTY_TREE_SELECTION );
    binding.bindValue( targetSelection, modelSelection );

    /* The next two lines are needed so that checking children of checked elements always works. */
    // FIXME: only, because getParent on the content provider does not work correctly, we should fix that
    m_treeViewer.expandAll();
    m_treeViewer.collapseAll();

    return m_treeViewer.getControl();
  }

  private Control createFilterControls( final IDataBinding binding, final Composite parent )
  {
    final Group panel = new Group( parent, SWT.NONE );
    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( panel );

    panel.setText( Messages.getString( "SelectResultWizardPage_0" ) ); //$NON-NLS-1$

    final Label label = new Label( panel, SWT.NONE );
    label.setText( Messages.getString( "SelectResultWizardPage_1" ) ); //$NON-NLS-1$

    final ComboViewer combo = new ComboViewer( panel, SWT.DROP_DOWN | SWT.READ_ONLY );
    combo.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    combo.setContentProvider( new ArrayContentProvider() );
    combo.setLabelProvider( new LabelProvider() );
    combo.setInput( SelectResultData.ShowType.values() );

    /* binding */
    final IViewerObservableValue targetSelection = ViewersObservables.observeSinglePostSelection( combo );
    final IObservableValue modelSelection = BeansObservables.observeValue( m_data, SelectResultData.PROPERTY_SHOW_ALL );
    binding.bindValue( targetSelection, modelSelection );

    return panel;
  }

  public void setFilter( final ViewerFilter filter )
  {
    Assert.isTrue( m_treeViewer == null );

    m_filter = filter;
  }

  /**
   * Adds an action to the toolbar. Must be called before {@link #createControl(Composite)} is called.
   */
  public void addAction( final IAction action )
  {
    Assert.isTrue( m_treeViewer == null );

    m_actions.add( action );
  }

  public TreeViewer getTreeViewer( )
  {
    return m_treeViewer;
  }
}