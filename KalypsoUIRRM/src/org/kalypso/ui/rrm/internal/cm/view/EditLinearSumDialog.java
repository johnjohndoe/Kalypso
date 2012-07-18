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
package org.kalypso.ui.rrm.internal.cm.view;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ColumnViewerEditorActivationEvent;
import org.eclipse.jface.viewers.ColumnViewerEditorActivationListener;
import org.eclipse.jface.viewers.ColumnViewerEditorDeactivationEvent;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.commons.databinding.dialog.DatabindingTitleAreaDialog;
import org.kalypso.commons.databinding.validation.ValidationStatusUtilities;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.hydrology.binding.cm.ILinearSumGenerator;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.model.hydrology.timeseries.TimeseriesValidatingOperation;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ui.rrm.internal.IUiRrmWorkflowConstants;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.cm.LinearSumHelper;
import org.kalypso.ui.rrm.internal.cm.view.comparator.DescriptionComparator;
import org.kalypso.ui.rrm.internal.cm.view.comparator.FactorComparator;
import org.kalypso.ui.rrm.internal.cm.view.comparator.GroupComparator;
import org.kalypso.ui.rrm.internal.cm.view.comparator.NameComparator;
import org.kalypso.ui.rrm.internal.cm.view.comparator.QualityComparator;
import org.kalypso.ui.rrm.internal.cm.view.comparator.StationComparator;
import org.kalypso.ui.rrm.internal.cm.view.comparator.TimestepComparator;
import org.kalypso.ui.rrm.internal.cm.view.filter.GroupViewerFilter;
import org.kalypso.ui.rrm.internal.cm.view.filter.ParameterTypeViewerFilter;
import org.kalypso.ui.rrm.internal.cm.view.filter.TimestepViewerFilter;
import org.kalypso.ui.rrm.internal.cm.view.provider.DescriptionColumnLabelProvider;
import org.kalypso.ui.rrm.internal.cm.view.provider.FactorColumnLabelProvider;
import org.kalypso.ui.rrm.internal.cm.view.provider.GroupColumnLabelProvider;
import org.kalypso.ui.rrm.internal.cm.view.provider.NameColumnLabelProvider;
import org.kalypso.ui.rrm.internal.cm.view.provider.QualityColumnLabelProvider;
import org.kalypso.ui.rrm.internal.cm.view.provider.StationColumnLabelProvider;
import org.kalypso.ui.rrm.internal.cm.view.provider.TimestepColumnLabelProvider;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeModel;
import org.kalypsodeegree.model.feature.Feature;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * This dialog allows the editing of the properties of a linear sum catchment model.
 * 
 * @author Holger Albert
 */
public class EditLinearSumDialog extends TitleAreaDialog
{
  private final PropertyChangeListener m_propertyListener = new PropertyChangeListener()
  {
    @Override
    public void propertyChange( final PropertyChangeEvent evt )
    {
      if( evt.getPropertyName().equals( ILinearSumGenerator.PROPERTY_PARAMETER_TYPE.toString() ) )
        handleParameterTypeChanged( evt );
      else
        handlePropertyChanged();
    }
  };

  /**
   * The model.
   */
  private final ITreeNodeModel m_model;

  /**
   * The bean to edit.
   */
  protected final LinearSumBean m_bean;

  /**
   * The main group.
   */
  private Group m_mainGroup;

  /**
   * The secondary group.
   */
  private Group m_secondaryGroup;

  /**
   * The details group.
   */
  private Group m_detailsGroup;

  private StatusComposite m_mainStatusComposite;

  /**
   * The catchment viewer.
   */
  protected TableViewer m_catchmentViewer;

  protected GroupViewerFilter m_groupViewerFilter;

  protected TimestepViewerFilter m_timestepViewerFilter;

  /**
   * The timeseries viewer.
   */
  protected TableViewer m_timeseriesViewer;

  /**
   * The details status composite.
   */
  protected StatusComposite m_detailsStatusComposite;

  /**
   * The selected catchment.
   */
  protected CatchmentBean m_catchmentBean;

  /**
   * The data binding.
   */
  private IDataBinding m_dataBinding;

  /**
   * The dialog settings.
   */
  private final IDialogSettings m_settings;

  /**
   * The ignore next change flag.
   */
  private boolean m_ignoreNextChange;

  /**
   * The constructor.
   * 
   * @param parentShell
   *          The parent shell, or null to create a top-level shell.
   * @param model
   *          The model.
   * @param bean
   *          The bean to edit.
   */
  public EditLinearSumDialog( final Shell shell, final ITreeNodeModel model, final LinearSumBean bean )
  {
    super( shell );

    m_model = model;
    m_bean = bean;

    m_mainGroup = null;
    m_secondaryGroup = null;
    m_detailsGroup = null;
    m_mainStatusComposite = null;
    m_catchmentViewer = null;
    m_groupViewerFilter = null;
    m_timestepViewerFilter = null;
    m_timeseriesViewer = null;
    m_detailsStatusComposite = null;
    m_catchmentBean = null;
    m_dataBinding = null;
    m_settings = DialogSettingsUtils.getDialogSettings( KalypsoUIRRMPlugin.getDefault(), getClass().getName() );
    m_ignoreNextChange = false;

    m_bean.addPropertyChangeListener( m_propertyListener );
  }

  @Override
  protected Control createDialogArea( final Composite parent )
  {
    /* Set the title. */
    getShell().setText( Messages.getString( "EditLinearSumDialog_0" ) ); //$NON-NLS-1$
    setTitle( Messages.getString( "EditLinearSumDialog_0" ) ); //$NON-NLS-1$

    /* Create the control. */
    final Composite control = (Composite) super.createDialogArea( parent );

    /* Create the main composite. */
    final Composite main = new Composite( control, SWT.NONE );
    main.setLayout( new GridLayout( 3, false ) );
    final GridData mainData = new GridData( SWT.FILL, SWT.FILL, true, true );
    mainData.heightHint = 550;
    mainData.widthHint = 1000;
    main.setLayoutData( mainData );

    /* Create the main sash form. */
    final SashForm mainSashForm = new SashForm( main, SWT.NONE );
    mainSashForm.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* Create the main group. */
    m_mainGroup = new Group( mainSashForm, SWT.NONE );
    m_mainGroup.setLayout( new GridLayout( 1, false ) );
    final GridData mainGroupData = new GridData( SWT.FILL, SWT.FILL, true, true );
    mainGroupData.widthHint = 200;
    m_mainGroup.setLayoutData( mainGroupData );
    m_mainGroup.setText( Messages.getString( "EditLinearSumDialog_1" ) ); //$NON-NLS-1$

    /* Create the content of the main group. */
    createMainContent( m_mainGroup );

    /* Create the secondary group. */
    m_secondaryGroup = new Group( mainSashForm, SWT.NONE );
    m_secondaryGroup.setLayout( new GridLayout( 1, false ) );
    final GridData secondaryGroupData = new GridData( SWT.FILL, SWT.FILL, true, true );
    secondaryGroupData.widthHint = 200;
    m_secondaryGroup.setLayoutData( secondaryGroupData );
    m_secondaryGroup.setText( Messages.getString( "EditLinearSumDialog_3" ) ); //$NON-NLS-1$

    /* Create the content of the secondary group. */
    createSecondaryContent( m_secondaryGroup );

    /* Create the details group. */
    m_detailsGroup = new Group( mainSashForm, SWT.NONE );
    m_detailsGroup.setLayout( new GridLayout( 1, false ) );
    m_detailsGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    m_detailsGroup.setText( Messages.getString( "EditLinearSumDialog_2" ) ); //$NON-NLS-1$

    /* Create the content of the details group. */
    createDetailsContent( m_detailsGroup, null );

    /* Set the weights. */
    mainSashForm.setWeights( new int[] { 25, 25, 50 } );

    return control;
  }

  @Override
  protected void createButtonsForButtonBar( final Composite parent )
  {
    super.createButtonsForButtonBar( parent );

    /* Update the status. */
    updateStatus();
    validateTimeseriesRanges( m_bean );
  }

  @Override
  protected IDialogSettings getDialogBoundsSettings( )
  {
    return DialogSettingsUtils.getSection( m_settings, "bounds" ); //$NON-NLS-1$
  }

  @Override
  protected boolean isResizable( )
  {
    return true;
  }

  @Override
  protected void okPressed( )
  {
    /* Check the status of the timeseries. */
    final IStatus timeseriesStatus = validateTimeseriesRanges( m_bean );
    if( !timeseriesStatus.isOK() )
    {
      final MultiStatus status = new MultiStatus( KalypsoUIRRMPlugin.getID(), IStatus.ERROR, new IStatus[] { timeseriesStatus }, Messages.getString( "EditLinearSumDialog.0" ), null ); //$NON-NLS-1$
      final StatusDialog statusDialog = new StatusDialog( getShell(), status, getShell().getText() );
      statusDialog.open();
      return;
    }

    /* Check the status of the data binding. */
    final IStatus bindingStatus = ValidationStatusUtilities.getFirstNonOkStatus( m_dataBinding );
    if( !bindingStatus.isOK() )
    {
      final MultiStatus status = new MultiStatus( KalypsoUIRRMPlugin.getID(), IStatus.ERROR, new IStatus[] { bindingStatus }, Messages.getString( "EditLinearSumDialog.0" ), null ); //$NON-NLS-1$
      final StatusDialog statusDialog = new StatusDialog( getShell(), status, getShell().getText() );
      statusDialog.open();
      return;
    }

    /* Perform ok. */
    final IStatus performStatus = performOk();
    if( !performStatus.isOK() )
    {
      StatusDialog.open( getShell(), performStatus, getShell().getText() );
      return;
    }

    /* Dispose the dialog. */
    dispose();

    super.okPressed();
  }

  @Override
  protected void cancelPressed( )
  {
    /* Dispose the dialog. */
    dispose();

    super.cancelPressed();
  }

  /**
   * This function creates the content of the main group.
   * 
   * @param parent
   *          The parent composite.
   */
  private void createMainContent( final Composite parent )
  {
    /* Create the form. */
    final ScrolledForm form = new ScrolledForm( parent, SWT.H_SCROLL | SWT.V_SCROLL );
    form.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    form.setExpandHorizontal( true );
    form.setExpandVertical( true );

    /* Get the body. */
    final Composite body = form.getBody();
    final GridLayout bodyLayout = new GridLayout( 1, false );
    bodyLayout.marginHeight = 0;
    bodyLayout.marginWidth = 0;
    body.setLayout( bodyLayout );

    /* Create the data binding. */
    m_dataBinding = new DatabindingTitleAreaDialog( this, null );

    /* Create the linear sum new composite. */
    final LinearSumNewComposite composite = new LinearSumNewComposite( body, m_bean, m_dataBinding, true );
    composite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* Create the status composite. */
    m_mainStatusComposite = new StatusComposite( parent, StatusComposite.DETAILS );
    m_mainStatusComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    /* Do a reflow and a layout. */
    form.reflow( true );
    form.layout( true, true );
  }

  /**
   * This function creates the content of the secondary group.
   * 
   * @param parent
   *          The parent composite.
   */
  private void createSecondaryContent( final Composite parent )
  {
    /* Create the catchment viewer. */
    m_catchmentViewer = new TableViewer( parent, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION | SWT.SINGLE );
    m_catchmentViewer.getTable().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    m_catchmentViewer.getTable().setLinesVisible( true );
    m_catchmentViewer.getTable().setHeaderVisible( true );
    m_catchmentViewer.getTable().addControlListener( new ColumnsResizeControlListener() );
    m_catchmentViewer.setContentProvider( new ArrayContentProvider() );

    /* Create the columns. */
    createCatchmentViewerColumns( m_catchmentViewer );

    /* Set the input. */
    m_catchmentViewer.setInput( m_bean.getCatchments() );

    /* Add a listener. */
    m_catchmentViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      /**
       * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
       */
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final ISelection selection = event.getSelection();
        if( selection.isEmpty() || !(selection instanceof IStructuredSelection) )
        {
          m_catchmentBean = null;
          updateDetailsGroup();
          return;
        }

        final Object firstElement = ((IStructuredSelection) selection).getFirstElement();
        if( !(firstElement instanceof CatchmentBean) )
        {
          m_catchmentBean = null;
          updateDetailsGroup();
          return;
        }

        m_catchmentBean = (CatchmentBean) firstElement;
        updateDetailsGroup();
      }
    } );
  }

  private void createCatchmentViewerColumns( final TableViewer viewer )
  {
    /* Create the name column. */
    final TableViewerColumn nameColumn = new TableViewerColumn( viewer, SWT.LEFT );
    nameColumn.getColumn().setText( Messages.getString( "EditLinearSumDialog_4" ) ); //$NON-NLS-1$
    nameColumn.getColumn().setWidth( 150 );
    nameColumn.setLabelProvider( new NameColumnLabelProvider() );
    ColumnsResizeControlListener.setMinimumPackWidth( nameColumn.getColumn() );
    ColumnViewerSorter.registerSorter( nameColumn, new NameComparator() );

    /* Create the description column. */
    if( m_bean.hasDescription() )
    {
      final TableViewerColumn descriptionColumn = new TableViewerColumn( viewer, SWT.LEFT );
      descriptionColumn.getColumn().setText( Messages.getString( "EditLinearSumDialog_5" ) ); //$NON-NLS-1$
      descriptionColumn.getColumn().setWidth( 150 );
      descriptionColumn.setLabelProvider( new DescriptionColumnLabelProvider() );
      ColumnsResizeControlListener.setMinimumPackWidth( descriptionColumn.getColumn() );
      ColumnViewerSorter.registerSorter( descriptionColumn, new DescriptionComparator() );
    }

    /* Define a initial order. */
    ColumnViewerSorter.setSortState( nameColumn, Boolean.FALSE );
  }

  /**
   * This function creates the content of the details group.
   * 
   * @param parent
   *          The parent composite.
   * @param catchmentBean
   *          The selected catchment.
   */
  private void createDetailsContent( final Composite parent, final CatchmentBean catchmentBean )
  {
    /* Create a text field. */
    final Text groupText = new Text( parent, SWT.BORDER );
    groupText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    groupText.setMessage( Messages.getString("EditLinearSumDialog.2") ); //$NON-NLS-1$
    groupText.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        if( m_groupViewerFilter != null )
        {
          final Text source = (Text) e.getSource();
          m_groupViewerFilter.updateSearchText( source.getText() );
          m_timeseriesViewer.refresh();
        }
      }
    } );

    /* Create a text field. */
    final Text timestepText = new Text( parent, SWT.BORDER );
    timestepText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    timestepText.setMessage( Messages.getString("EditLinearSumDialog.3") ); //$NON-NLS-1$
    timestepText.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        if( m_timestepViewerFilter != null )
        {
          final Text source = (Text) e.getSource();
          m_timestepViewerFilter.updateSearchText( source.getText() );
          m_timeseriesViewer.refresh();
        }
      }
    } );

    /* Create the timeseries viewer. */
    m_timeseriesViewer = new TableViewer( parent, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION | SWT.SINGLE );
    m_timeseriesViewer.getTable().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    m_timeseriesViewer.getTable().setLinesVisible( true );
    m_timeseriesViewer.getTable().setHeaderVisible( true );
    m_timeseriesViewer.getTable().addControlListener( new ColumnsResizeControlListener() );
    m_timeseriesViewer.setContentProvider( new ArrayContentProvider() );

    /* Create the viewer filters. */
    final ParameterTypeViewerFilter parameterTypeViewerFilter = new ParameterTypeViewerFilter( (String) m_bean.getProperty( ILinearSumGenerator.PROPERTY_PARAMETER_TYPE ) );
    m_groupViewerFilter = new GroupViewerFilter();
    m_timestepViewerFilter = new TimestepViewerFilter();

    /* Set the viewer filters. */
    m_timeseriesViewer.setFilters( new ViewerFilter[] { parameterTypeViewerFilter, m_groupViewerFilter, m_timestepViewerFilter } );

    /* Create the columns. */
    createTimeseriesViewerColumns( m_timeseriesViewer );

    /* Set the input. */
    if( catchmentBean != null )
      m_timeseriesViewer.setInput( catchmentBean.getTimeseries() );

    /* Create the status composite. */
    m_detailsStatusComposite = new StatusComposite( parent, StatusComposite.DETAILS );
    m_detailsStatusComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    /* Add a listener. */
    m_timeseriesViewer.getColumnViewerEditor().addEditorActivationListener( new ColumnViewerEditorActivationListener()
    {
      @Override
      public void beforeEditorDeactivated( final ColumnViewerEditorDeactivationEvent event )
      {
      }

      @Override
      public void beforeEditorActivated( final ColumnViewerEditorActivationEvent event )
      {
      }

      @Override
      public void afterEditorDeactivated( final ColumnViewerEditorDeactivationEvent event )
      {
        m_catchmentViewer.refresh();
        updateStatus();
        validateTimeseriesRanges( m_bean );
      }

      @Override
      public void afterEditorActivated( final ColumnViewerEditorActivationEvent event )
      {
      }
    } );
  }

  private void createTimeseriesViewerColumns( final TableViewer viewer )
  {
    /* Create the group column. */
    final TableViewerColumn groupColumn = new TableViewerColumn( viewer, SWT.LEFT );
    groupColumn.getColumn().setText( Messages.getString( "EditLinearSumDialog_7" ) ); //$NON-NLS-1$
    groupColumn.getColumn().setWidth( 100 );
    groupColumn.setLabelProvider( new GroupColumnLabelProvider() );
    ColumnsResizeControlListener.setMinimumPackWidth( groupColumn.getColumn() );
    ColumnViewerSorter.registerSorter( groupColumn, new GroupComparator() );

    /* Create the station column. */
    final TableViewerColumn stationColumn = new TableViewerColumn( viewer, SWT.LEFT );
    stationColumn.getColumn().setText( Messages.getString( "EditLinearSumDialog_8" ) ); //$NON-NLS-1$
    stationColumn.getColumn().setWidth( 100 );
    stationColumn.setLabelProvider( new StationColumnLabelProvider() );
    ColumnsResizeControlListener.setMinimumPackWidth( stationColumn.getColumn() );
    ColumnViewerSorter.registerSorter( stationColumn, new StationComparator() );

    /* Create the timestep column. */
    final TableViewerColumn timestepColumn = new TableViewerColumn( viewer, SWT.LEFT );
    timestepColumn.getColumn().setText( Messages.getString( "EditLinearSumDialog_9" ) ); //$NON-NLS-1$
    timestepColumn.getColumn().setWidth( 75 );
    timestepColumn.setLabelProvider( new TimestepColumnLabelProvider() );
    ColumnsResizeControlListener.setMinimumPackWidth( timestepColumn.getColumn() );
    ColumnViewerSorter.registerSorter( timestepColumn, new TimestepComparator() );

    /* Create the quality column. */
    final TableViewerColumn qualityColumn = new TableViewerColumn( viewer, SWT.LEFT );
    qualityColumn.getColumn().setText( Messages.getString( "EditLinearSumDialog_10" ) ); //$NON-NLS-1$
    qualityColumn.getColumn().setWidth( 100 );
    qualityColumn.setLabelProvider( new QualityColumnLabelProvider() );
    ColumnsResizeControlListener.setMinimumPackWidth( qualityColumn.getColumn() );
    ColumnViewerSorter.registerSorter( qualityColumn, new QualityComparator() );

    /* Create the factor column. */
    final TableViewerColumn factorColumn = new TableViewerColumn( viewer, SWT.LEFT );
    factorColumn.getColumn().setText( Messages.getString( "EditLinearSumDialog_11" ) ); //$NON-NLS-1$
    factorColumn.getColumn().setWidth( 75 );
    factorColumn.setLabelProvider( new FactorColumnLabelProvider() );
    factorColumn.setEditingSupport( new FactorEditingSupport( viewer ) );
    ColumnsResizeControlListener.setMinimumPackWidth( factorColumn.getColumn() );
    ColumnViewerSorter.registerSorter( factorColumn, new FactorComparator() );

    /* Define a initial order. */
    ColumnViewerSorter.setSortState( factorColumn, Boolean.TRUE );
  }

  /**
   * This function saves the changes.
   * 
   * @return A ERROR status on error or an OK status.
   */
  private IStatus performOk( )
  {
    try
    {
      final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
      final CommandableWorkspace generatorsWorkspace = dataProvider.getCommandableWorkSpace( IUiRrmWorkflowConstants.SCENARIO_DATA_CATCHMENT_MODELS );

      /* Apply the changes. */
      final Feature generator = m_bean.apply( generatorsWorkspace, (String) m_bean.getProperty( ILinearSumGenerator.PROPERTY_PARAMETER_TYPE ) );

      /* Refresh the tree. */
      m_model.refreshTree( generator );

      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed to save the model", e ); //$NON-NLS-1$
    }
  }

  /**
   * This function disposes the dialog.
   */
  private void dispose( )
  {
    m_mainGroup = null;
    m_secondaryGroup = null;
    m_detailsGroup = null;
    m_mainStatusComposite = null;
    m_catchmentViewer = null;
    m_groupViewerFilter = null;
    m_timestepViewerFilter = null;
    m_timeseriesViewer = null;
    m_detailsStatusComposite = null;
    m_catchmentBean = null;
    m_dataBinding = null;
    /* HINT: Do not discard the dialog settings, will be used to save the dialog bounds . */
    m_ignoreNextChange = false;
  }

  /**
   * This function updates the details group.
   */
  protected void updateDetailsGroup( )
  {
    /* Cannot do anything. */
    if( m_detailsGroup == null || m_detailsGroup.isDisposed() )
      return;

    /* Set the input. */
    if( m_catchmentBean != null )
      m_timeseriesViewer.setInput( m_catchmentBean.getTimeseries() );
    else
      m_timeseriesViewer.setInput( new FactorizedTimeseriesBean[] {} );

    /* Update the status. */
    updateStatus();
    validateTimeseriesRanges( m_bean );
  }

  /**
   * This function updates the status.
   */
  protected void updateStatus( )
  {
    if( m_detailsStatusComposite == null || m_detailsStatusComposite.isDisposed() )
      return;

    if( m_catchmentBean != null )
    {
      m_catchmentBean.updateStatus();
      m_detailsStatusComposite.setStatus( m_catchmentBean.getStatus() );
    }
    else
      m_detailsStatusComposite.setStatus( new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), Messages.getString( "EditLinearSumDialog_6" ) ) ); //$NON-NLS-1$

    if( m_catchmentViewer != null && !m_catchmentViewer.getTable().isDisposed() )
      m_catchmentViewer.refresh();
  }

  /**
   * This function handles the property changed event for the parameter type.
   * 
   * @param evt
   *          The property change event.
   */
  protected void handleParameterTypeChanged( final PropertyChangeEvent evt )
  {
    /* Avoid loop, if we cancel the change. */
    if( m_ignoreNextChange == true )
    {
      m_ignoreNextChange = false;
      return;
    }

    /* Get the shell. */
    final Shell shell = getShell();

    /* Show the confirm dialog. */
    if( !MessageDialog.openConfirm( shell, shell.getText(), Messages.getString( "EditLinearSumDialog.1" ) ) ) //$NON-NLS-1$
    {
      m_ignoreNextChange = true;
      final LinearSumBean generator = m_bean;
      final Runnable revertOperation = new Runnable()
      {
        @Override
        public void run( )
        {
          generator.setProperty( ILinearSumGenerator.PROPERTY_PARAMETER_TYPE, evt.getOldValue() );
        }
      };
      shell.getDisplay().asyncExec( revertOperation );

      return;
    }

    /* Reset the timeseries. */
    m_bean.resetTimeseries();
    updateStatus();
    validateTimeseriesRanges( m_bean );

    /* Update the viewer filter in the timeseries viewer. */
    final String parameterType = (String) evt.getNewValue();
    if( m_timeseriesViewer != null && !m_timeseriesViewer.getTable().isDisposed() )
      m_timeseriesViewer.setFilters( new ViewerFilter[] { new ParameterTypeViewerFilter( parameterType ) } );
  }

  protected void handlePropertyChanged( )
  {
    updateStatus();
    validateTimeseriesRanges( m_bean );
  }

  protected IStatus validateTimeseriesRanges( final LinearSumBean bean )
  {
    if( m_mainStatusComposite == null || m_mainStatusComposite.isDisposed() )
      return null;

    final ITimeseries[] timeseries = LinearSumHelper.collectTimeseries( bean );
    final DateRange dateRange = LinearSumHelper.createDateRange( bean );

    final TimeseriesValidatingOperation operation = new TimeseriesValidatingOperation( timeseries, dateRange );
    final IStatus status = operation.execute( new NullProgressMonitor() );

    m_mainStatusComposite.setStatus( status );

    return status;
  }
}