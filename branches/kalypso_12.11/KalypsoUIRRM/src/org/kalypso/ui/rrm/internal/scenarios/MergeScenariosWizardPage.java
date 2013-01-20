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
package org.kalypso.ui.rrm.internal.scenarios;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.set.WritableSet;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.databinding.viewers.IViewerObservableSet;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.IOpenListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.OpenEvent;
import org.eclipse.jface.viewers.TableLayout;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.TreeViewerColumn;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.kalypso.afgui.views.ScenarioViewerFilter;
import org.kalypso.afgui.workflow.WorkflowBreadcrumbContentProvider;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.DataSetBinder;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;

import de.renew.workflow.connector.cases.IScenario;

/**
 * The merge scenarios wizard page.
 * 
 * @author Holger Albert
 */
public class MergeScenariosWizardPage extends WizardPage
{
  /**
   * The scenarios data object.
   */
  protected final MergeScenariosData m_scenariosData;

  /**
   * The scenario compare status contains stati for several cases.
   */
  protected final ScenarioCompareStatus m_compareStatus;

  /**
   * The data binding context.
   */
  private DatabindingWizardPage m_dataBinding;

  /**
   * The tree viewer for selecting the scenarios to merge.
   */
  private CheckboxTreeViewer m_treeViewer;

  /**
   * The constructor.
   * 
   * @param pageName
   *          The name of the page.
   * @param scenariosData
   *          The scenarios data object.
   */
  public MergeScenariosWizardPage( final String pageName, final MergeScenariosData scenariosData )
  {
    super( pageName );

    m_scenariosData = scenariosData;
    m_compareStatus = new ScenarioCompareStatus();
    m_dataBinding = null;
    m_treeViewer = null;

    setTitle( Messages.getString( "MergeScenariosWizardPage_0" ) ); //$NON-NLS-1$
    setDescription( String.format( Messages.getString( "MergeScenariosWizardPage_1" ), scenariosData.getTargetScenario().getName() ) ); //$NON-NLS-1$
  }

  @Override
  public void createControl( final Composite parent )
  {
    /* The data binding wizard page. */
    m_dataBinding = new DatabindingWizardPage( this, null );

    /* Create the main composite. */
    final Composite main = new Composite( parent, SWT.NONE );
    main.setLayout( new GridLayout( 2, false ) );
    main.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* Create the checkbox tree viewer. */
    m_treeViewer = new CheckboxTreeViewer( main, SWT.BORDER | SWT.SINGLE | SWT.FULL_SELECTION | SWT.H_SCROLL | SWT.V_SCROLL );
    configureTreeViewer( m_treeViewer );
    m_treeViewer.setAutoExpandLevel( 2 );
    m_treeViewer.setContentProvider( new WorkflowBreadcrumbContentProvider() );
    m_treeViewer.setFilters( new ViewerFilter[] { new ScenarioViewerFilter() } );
    m_treeViewer.setInput( m_scenariosData.getTargetScenario().getProject() );
    m_treeViewer.addOpenListener( new IOpenListener()
    {
      @Override
      public void open( final OpenEvent event )
      {
        handleOpen( event );
      }
    } );

    /* Create a button. */
    final Button deleteButton = new Button( main, SWT.CHECK );
    deleteButton.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, true, false ) );
    deleteButton.setText( Messages.getString( "MergeScenariosWizardPage_2" ) ); //$NON-NLS-1$

    /* Create a button. */
    final Button compareButton = new Button( main, SWT.PUSH );
    compareButton.setLayoutData( new GridData( SWT.END, SWT.CENTER, true, false ) );
    compareButton.setText( String.format( Messages.getString( "MergeScenariosWizardPage_3" ), m_scenariosData.getTargetScenario().getName() ) ); //$NON-NLS-1$
    compareButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handleCompareSelected( m_scenariosData, m_compareStatus );
      }
    } );

    /* Data binding. */
    bindTreeViewer( m_treeViewer );
    bindDeleteButton( deleteButton );

    /* Set the control. */
    setControl( main );
  }

  /**
   * This function creates the columns in the tree viewer.
   * 
   * @param treeViewer
   *          The tree viewer.
   */
  private void configureTreeViewer( final TreeViewer treeViewer )
  {
    /* Configure the tree. */
    final Tree tree = treeViewer.getTree();
    tree.setLayout( new TableLayout() );
    tree.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true, 2, 1 ) );
    tree.setLinesVisible( true );
    tree.setHeaderVisible( true );

    /* Create the scenarios column. */
    final TreeViewerColumn scenariosViewerColumn = new TreeViewerColumn( treeViewer, SWT.NONE );
    scenariosViewerColumn.setLabelProvider( new ScenariosColumnLabelProvider( m_scenariosData.getTargetScenario() ) );
    final TreeColumn scenariosColumn = scenariosViewerColumn.getColumn();
    scenariosColumn.setText( Messages.getString( "MergeScenariosWizardPage_4" ) ); //$NON-NLS-1$
    scenariosColumn.setWidth( 200 );
    scenariosColumn.setAlignment( SWT.LEAD );

    /* Create the model column. */
    final TreeViewerColumn modelViewerColumn = new TreeViewerColumn( treeViewer, SWT.NONE );
    modelViewerColumn.setLabelProvider( new ScenarioCompareStatusLabelProvider( m_compareStatus, ScenarioCompareStatus.KEY_MODEL, m_scenariosData.getTargetScenario() ) );
    final TreeColumn modelColumn = modelViewerColumn.getColumn();
    modelColumn.setText( Messages.getString( "MergeScenariosWizardPage_5" ) ); //$NON-NLS-1$
    modelColumn.setWidth( 125 );
    modelColumn.setAlignment( SWT.LEAD );

    /* Create the parameter column. */
    final TreeViewerColumn parameterViewerColumn = new TreeViewerColumn( treeViewer, SWT.NONE );
    parameterViewerColumn.setLabelProvider( new ScenarioCompareStatusLabelProvider( m_compareStatus, ScenarioCompareStatus.KEY_PARAMETER, m_scenariosData.getTargetScenario() ) );
    final TreeColumn parameterColumn = parameterViewerColumn.getColumn();
    parameterColumn.setText( Messages.getString( "MergeScenariosWizardPage_6" ) ); //$NON-NLS-1$
    parameterColumn.setWidth( 125 );
    parameterColumn.setAlignment( SWT.LEAD );

    /* Create the hydrotopes column. */
    final TreeViewerColumn hydrotopesViewerColumn = new TreeViewerColumn( treeViewer, SWT.NONE );
    hydrotopesViewerColumn.setLabelProvider( new ScenarioCompareStatusLabelProvider( m_compareStatus, ScenarioCompareStatus.KEY_HYDROTOPES, m_scenariosData.getTargetScenario() ) );
    final TreeColumn hydrotopesColumn = hydrotopesViewerColumn.getColumn();
    hydrotopesColumn.setText( Messages.getString( "MergeScenariosWizardPage_7" ) ); //$NON-NLS-1$
    hydrotopesColumn.setWidth( 125 );
    hydrotopesColumn.setAlignment( SWT.LEAD );
  }

  private void bindTreeViewer( final CheckboxTreeViewer treeViewer )
  {
    final IViewerObservableSet target = ViewersObservables.observeCheckedElements( treeViewer, IScenario.class );
    final WritableSet model = m_scenariosData.getSelectedScenariosSet();
    final DataSetBinder binder = new DataSetBinder( target, model );
    m_dataBinding.bindValue( binder );
  }

  private void bindDeleteButton( final Button deleteButton )
  {
    final ISWTObservableValue target = SWTObservables.observeSelection( deleteButton );
    final IObservableValue model = BeansObservables.observeValue( m_scenariosData, "deleteScenarios" ); //$NON-NLS-1$
    final DataBinder dataBinder = new DataBinder( target, model );
    m_dataBinding.bindValue( dataBinder );
  }

  protected void handleOpen( final OpenEvent event )
  {
    final ISelection selection = event.getSelection();
    if( selection.isEmpty() || !(selection instanceof IStructuredSelection) )
      return;

    final IStructuredSelection structuredSelection = (IStructuredSelection)selection;
    final Object firstElement = structuredSelection.getFirstElement();
    if( !(firstElement instanceof IScenario) )
      return;

    final IScenario scenario = (IScenario)firstElement;
    final String uri = scenario.getURI();
    final String name = scenario.getName();
    final IStatus status = m_compareStatus.getMergedStatus( uri, name );
    if( status == null )
      return;

    final StatusDialog dialog = new StatusDialog( getShell(), status, getTitle() );
    dialog.open();
  }

  /**
   * This function compares a scenario against the selected scenarios and updates the UI.
   * 
   * @param scenariosData
   *          The scenarios data object.
   * @param compareStatus
   *          The scenario compare status contains stati for several cases.
   */
  protected void handleCompareSelected( final MergeScenariosData scenariosData, final ScenarioCompareStatus compareStatus )
  {
    /* Create the operation. */
    final CompareScenariosOperation operation = new CompareScenariosOperation( scenariosData, compareStatus );

    /* Execute the operation. */
    final IStatus status = RunnableContextHelper.execute( getContainer(), false, true, operation );
    if( !status.isOK() )
    {
      /* Log the error message. */
      KalypsoUIRRMPlugin.getDefault().getLog().log( status );

      /* Show a status dialog. */
      final StatusDialog statusDialog = new StatusDialog( getShell(), status, getTitle() );
      statusDialog.open();
    }

    /* Refresh. */
    m_treeViewer.refresh();
  }

  /**
   * This function returns the scenarios data object.
   * 
   * @return The scenarios data object.
   */
  public MergeScenariosData getScenariosData( )
  {
    return m_scenariosData;
  }

  /**
   * This function returns the scenario compare status.
   * 
   * @return The scenario compare status.
   */
  public ScenarioCompareStatus getCompareStatus( )
  {
    return m_compareStatus;
  }
}