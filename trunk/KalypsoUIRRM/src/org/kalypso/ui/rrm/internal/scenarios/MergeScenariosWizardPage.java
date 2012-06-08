/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
import org.kalypso.afgui.internal.ui.workflow.WorkflowBreadcrumbContentProvider;
import org.kalypso.afgui.views.ScenarioViewerFilter;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.DataSetBinder;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;

import de.renew.workflow.connector.cases.IScenario;

/**
 * The merge scenarios wizard page.
 * 
 * @author Holger Albert
 */
public class MergeScenariosWizardPage extends WizardPage
{
  /**
   * The scenario, where the others scenarios should be merged into.
   */
  protected final IScenario m_scenario;

  /**
   * The scenarios data object.
   */
  protected final MergeScenariosData m_scenariosData;

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
   * @param scenario
   *          The scenario, where the others scenarios should be merged into.
   * @param scenariosData
   *          The scenarios data object.
   */
  public MergeScenariosWizardPage( final String pageName, final IScenario scenario, final MergeScenariosData scenariosData )
  {
    super( pageName );

    m_scenario = scenario;
    m_scenariosData = scenariosData;
    m_dataBinding = null;
    m_treeViewer = null;

    setTitle( "Szenarien zusammenführen" );
    setDescription( String.format( "Auswahl der Szenarien, die in das Szenario '%s' zusammengeführt werden sollen.", scenario.getName() ) );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
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
    m_treeViewer = new CheckboxTreeViewer( main, SWT.SINGLE | SWT.FULL_SELECTION | SWT.H_SCROLL | SWT.V_SCROLL );
    configureTreeViewer( m_treeViewer );
    m_treeViewer.setAutoExpandLevel( 2 );
    m_treeViewer.setContentProvider( new WorkflowBreadcrumbContentProvider() );
    m_treeViewer.setFilters( new ViewerFilter[] { new ScenarioViewerFilter() } );
    m_treeViewer.setInput( m_scenario.getProject() );

    /* Create a button. */
    final Button deleteButton = new Button( main, SWT.CHECK );
    deleteButton.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, true, false ) );
    deleteButton.setText( "Importierte Szenarien löschen" );

    /* Create a button. */
    final Button compareButton = new Button( main, SWT.PUSH );
    compareButton.setLayoutData( new GridData( SWT.END, SWT.CENTER, true, false ) );
    compareButton.setText( String.format( "Mit '%s' vergleichen", m_scenario.getName() ) );
    compareButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handleCompareSelected( m_scenario, m_scenariosData );
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
    scenariosViewerColumn.setLabelProvider( new ScenariosColumnLabelProvider() );
    final TreeColumn scenariosColumn = scenariosViewerColumn.getColumn();
    scenariosColumn.setText( "Szenario" );
    scenariosColumn.setWidth( 200 );
    scenariosColumn.setAlignment( SWT.LEAD );

    /* Create the model column. */
    final TreeViewerColumn modelViewerColumn = new TreeViewerColumn( treeViewer, SWT.NONE );
    modelViewerColumn.setLabelProvider( new ModelColumnLabelProvider() );
    final TreeColumn modelColumn = modelViewerColumn.getColumn();
    modelColumn.setText( "Modell" );
    modelColumn.setWidth( 75 );
    modelColumn.setAlignment( SWT.LEAD );

    /* Create the parameter column. */
    final TreeViewerColumn parameterViewerColumn = new TreeViewerColumn( treeViewer, SWT.NONE );
    parameterViewerColumn.setLabelProvider( new ParameterColumnLabelProvider() );
    final TreeColumn parameterColumn = parameterViewerColumn.getColumn();
    parameterColumn.setText( "Parameter" );
    parameterColumn.setWidth( 75 );
    parameterColumn.setAlignment( SWT.LEAD );

    /* Create the hydrotopes column. */
    final TreeViewerColumn hydrotopesViewerColumn = new TreeViewerColumn( treeViewer, SWT.NONE );
    hydrotopesViewerColumn.setLabelProvider( new HydrotopesColumnLabelProvider() );
    final TreeColumn hydrotopesColumn = hydrotopesViewerColumn.getColumn();
    hydrotopesColumn.setText( "Hydrotope" );
    hydrotopesColumn.setWidth( 75 );
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
    final IObservableValue model = BeansObservables.observeValue( m_scenariosData, "deleteScenarios" );
    final DataBinder dataBinder = new DataBinder( target, model );
    m_dataBinding.bindValue( dataBinder );
  }

  /**
   * This function compares a scenario against the selected scenarios and updates the UI.
   * 
   * @param scenario
   *          The scenario, where the others scenarios should be merged into.
   * @param scenariosData
   *          The scenarios data object.
   */
  protected void handleCompareSelected( final IScenario scenario, final MergeScenariosData scenariosData )
  {
    /* Create the operation. */
    final CompareScenariosOperation operation = new CompareScenariosOperation( scenario, scenariosData );

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
}