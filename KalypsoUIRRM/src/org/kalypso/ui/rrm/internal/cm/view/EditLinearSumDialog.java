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
package org.kalypso.ui.rrm.internal.cm.view;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import org.eclipse.core.databinding.Binding;
import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.core.databinding.observable.ChangeEvent;
import org.eclipse.core.databinding.observable.IChangeListener;
import org.eclipse.core.databinding.observable.IObservable;
import org.eclipse.core.databinding.observable.list.IObservableList;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.TrayDialog;
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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.commons.databinding.forms.DatabindingForm;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.rcm.binding.ILinearSumGenerator;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.cm.view.comparator.DescriptionComparator;
import org.kalypso.ui.rrm.internal.cm.view.comparator.FactorComparator;
import org.kalypso.ui.rrm.internal.cm.view.comparator.GroupComparator;
import org.kalypso.ui.rrm.internal.cm.view.comparator.NameComparator;
import org.kalypso.ui.rrm.internal.cm.view.comparator.QualityComparator;
import org.kalypso.ui.rrm.internal.cm.view.comparator.StationComparator;
import org.kalypso.ui.rrm.internal.cm.view.comparator.TimestepComparator;
import org.kalypso.ui.rrm.internal.cm.view.filter.ParameterTypeViewerFilter;
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

/**
 * This dialog allows the editing of the properties of a linear sum catchment model.
 * 
 * @author Holger Albert
 */
public class EditLinearSumDialog extends TrayDialog
{
  /**
   * A property change listener for the parameter type.
   */
  private final PropertyChangeListener m_parameterTypeListener = new PropertyChangeListener()
  {
    @Override
    public void propertyChange( final PropertyChangeEvent evt )
    {
      handleParameterTypeChanged( evt );
    }
  };

  /**
   * The model.
   */
  private final ITreeNodeModel m_model;

  /**
   * The bean to edit.
   */
  private final LinearSumBean m_bean;

  /**
   * The form.
   */
  private Form m_form;

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

  /**
   * The catchment viewer.
   */
  protected TableViewer m_catchmentViewer;

  /**
   * The timeseries viewer.
   */
  private TableViewer m_timeseriesViewer;

  /**
   * The status composite.
   */
  protected StatusComposite m_statusComposite;

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
  private IDialogSettings m_settings;

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

    m_form = null;
    m_mainGroup = null;
    m_secondaryGroup = null;
    m_detailsGroup = null;
    m_catchmentViewer = null;
    m_timeseriesViewer = null;
    m_statusComposite = null;
    m_catchmentBean = null;
    m_dataBinding = null;
    m_settings = DialogSettingsUtils.getDialogSettings( KalypsoUIRRMPlugin.getDefault(), getClass().getName() );
    m_ignoreNextChange = false;

    m_bean.addPropertyChangeListener( ILinearSumGenerator.PROPERTY_PARAMETER_TYPE.toString(), m_parameterTypeListener );
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( final Composite parent )
  {
    /* Set the title. */
    getShell().setText( Messages.getString( "EditLinearSumDialog_0" ) ); //$NON-NLS-1$

    /* Create the main composite. */
    final Composite main = (Composite) super.createDialogArea( parent );
    main.setLayout( new GridLayout( 1, false ) );
    final GridData mainData = new GridData( SWT.FILL, SWT.FILL, true, true );
    mainData.heightHint = 550;
    mainData.widthHint = 1000;
    main.setLayoutData( mainData );

    /* Create the form. */
    m_form = new Form( main, SWT.NONE );
    m_form.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* Create the data binding. */
    m_dataBinding = new DatabindingForm( m_form, null );

    /* Get the body. */
    final Composite body = m_form.getBody();
    body.setLayout( new GridLayout( 3, false ) );

    /* Create the main group. */
    m_mainGroup = new Group( body, SWT.NONE );
    m_mainGroup.setLayout( new GridLayout( 1, false ) );
    final GridData mainGroupData = new GridData( SWT.FILL, SWT.FILL, true, true );
    mainGroupData.widthHint = 200;
    m_mainGroup.setLayoutData( mainGroupData );
    m_mainGroup.setText( Messages.getString( "EditLinearSumDialog_1" ) ); //$NON-NLS-1$

    /* Create the content of the main group. */
    createMainContent( m_mainGroup );

    /* Create the secondary group. */
    m_secondaryGroup = new Group( body, SWT.NONE );
    m_secondaryGroup.setLayout( new GridLayout( 1, false ) );
    final GridData secondaryGroupData = new GridData( SWT.FILL, SWT.FILL, true, true );
    secondaryGroupData.widthHint = 200;
    m_secondaryGroup.setLayoutData( secondaryGroupData );
    m_secondaryGroup.setText( Messages.getString( "EditLinearSumDialog_3" ) ); //$NON-NLS-1$

    /* Create the content of the secondary group. */
    createSecondaryContent( m_secondaryGroup );

    /* Create the details group. */
    m_detailsGroup = new Group( body, SWT.NONE );
    m_detailsGroup.setLayout( new GridLayout( 1, false ) );
    m_detailsGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    m_detailsGroup.setText( Messages.getString( "EditLinearSumDialog_2" ) ); //$NON-NLS-1$

    /* Create the content of the details group. */
    createDetailsContent( m_detailsGroup, null );

    /* HINT: The change listeners must be added, after the bindings were done. */
    addChangeListeners( m_dataBinding );

    return main;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected void createButtonsForButtonBar( final Composite parent )
  {
    super.createButtonsForButtonBar( parent );

    /* Do a layout. */
    m_form.layout( true, true );

    /* Validate the dialog. */
    validateDialog();
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#getDialogBoundsSettings()
   */
  @Override
  protected IDialogSettings getDialogBoundsSettings( )
  {
    return DialogSettingsUtils.getSection( m_settings, "bounds" ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#isResizable()
   */
  @Override
  protected boolean isResizable( )
  {
    return true;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#okPressed()
   */
  @Override
  protected void okPressed( )
  {
    /* Perform ok. */
    performOk();

    /* Dispose the dialog. */
    dispose();

    super.okPressed();
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#cancelPressed()
   */
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
    body.setLayout( new GridLayout( 1, false ) );

    /* Create the linear sum new composite. */
    final LinearSumNewComposite composite = new LinearSumNewComposite( body, m_bean, m_dataBinding, true );
    composite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

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
        validateDialog();
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
    /* Create the timeseries viewer. */
    m_timeseriesViewer = new TableViewer( parent, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION | SWT.SINGLE );
    m_timeseriesViewer.getTable().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    m_timeseriesViewer.getTable().setLinesVisible( true );
    m_timeseriesViewer.getTable().setHeaderVisible( true );
    m_timeseriesViewer.getTable().addControlListener( new ColumnsResizeControlListener() );
    m_timeseriesViewer.setContentProvider( new ArrayContentProvider() );
    m_timeseriesViewer.setFilters( new ViewerFilter[] { new ParameterTypeViewerFilter( (String) m_bean.getProperty( ILinearSumGenerator.PROPERTY_PARAMETER_TYPE ) ) } );

    /* Create the columns. */
    createTimeseriesViewerColumns( m_timeseriesViewer );

    /* Set the input. */
    if( catchmentBean != null )
      m_timeseriesViewer.setInput( catchmentBean.getTimeseries() );

    /* Create the status composite. */
    m_statusComposite = new StatusComposite( parent, SWT.NONE );
    m_statusComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

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
        validateDialog();
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
   * This function adds change listeners to the databinding.
   * 
   * @param dataBinding
   *          The data binding.
   */
  private void addChangeListeners( final IDataBinding dataBinding )
  {
    final DataBindingContext bindingContext = dataBinding.getBindingContext();
    final IObservableList bindings = bindingContext.getBindings();
    for( final Object object : bindings )
    {
      final Binding binding = (Binding) object;
      final IObservable target = binding.getTarget();
      target.addChangeListener( new IChangeListener()
      {
        @Override
        public void handleChange( final ChangeEvent event )
        {
          // TODO Is notified before the change, not after...
          System.out.println( "AHHHHHHHHHHHHHHHHHHHHHH" );
          validateDialog();
        }
      } );
    }
  }

  /**
   * This function updates the status.
   */
  private void updateStatus( )
  {
    if( m_statusComposite == null || m_statusComposite.isDisposed() )
      return;

    if( m_catchmentBean != null )
    {
      m_catchmentBean.updateStatus();
      m_statusComposite.setStatus( m_catchmentBean.getStatus() );
    }
    else
      m_statusComposite.setStatus( new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), Messages.getString( "EditLinearSumDialog_6" ) ) ); //$NON-NLS-1$
  }

  /**
   * This function saves the changes.
   */
  private void performOk( )
  {
    try
    {
      /* Apply the changes. */
      final Feature generator = m_bean.apply( m_model.getWorkspace(), (String) m_bean.getProperty( ILinearSumGenerator.PROPERTY_PARAMETER_TYPE ) );

      /* Refresh the tree. */
      m_model.refreshTree( generator );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed to save the model", e ); //$NON-NLS-1$
      StatusDialog.open( getShell(), status, getShell().getText() );
    }
  }

  /**
   * This function disposes the dialog.
   */
  private void dispose( )
  {
    m_form = null;
    m_mainGroup = null;
    m_secondaryGroup = null;
    m_detailsGroup = null;
    m_catchmentViewer = null;
    m_timeseriesViewer = null;
    m_statusComposite = null;
    m_catchmentBean = null;
    m_dataBinding = null;
    m_settings = null;
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

    /* Validate the dialog. */
    validateDialog();
  }

  /**
   * This function validates the dialog.
   */
  protected void validateDialog( )
  {
    /* This is independent of the other validation. */
    updateStatus();

    /* Cannot do anything. */
    if( m_form == null || m_form.isDisposed() )
      return;

    /* Get the ok button. */
    final Button okButton = getButton( IDialogConstants.OK_ID );
    if( okButton == null )
      return;

    /* Set the ok button to enabled. */
    okButton.setEnabled( true );

    /* On error in the form, the dialog may not be completed. */
    final int messageType = m_form.getMessageType();
    if( messageType == IMessageProvider.ERROR )
    {
      /* HINT: The message was already set by the databinding. */
      okButton.setEnabled( false );
      return;
    }

    /* Get the timestep. */
    final Integer timestep = (Integer) m_bean.getProperty( ILinearSumGenerator.PROPERTY_TIMESTEP );
    if( timestep != null && timestep.intValue() == 1440 )
    {
      /* We need a valid timestamp, if the timestep represents one day (1440 minutes). */
      /* The case that the timestamp format is wrong is covered in the above if. */
      final String timestamp = (String) m_bean.getProperty( ILinearSumGenerator.PROPERTY_TIMESTAMP );
      if( timestamp == null || timestamp.length() == 0 )
      {
        m_form.setMessage( "For day values, a timestamp is needed...", IMessageProvider.ERROR );
        okButton.setEnabled( false );
        return;
      }
    }

    /* HINT: The validation was ok. */
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
    if( !MessageDialog.openConfirm( shell, shell.getText(), "Changing the parameter type needs to update the list of timeseries. Unsaved changes will be lost. Continue?" ) )
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

    /* Update the viewer filter in the timeseries viewer. */
    final String parameterType = (String) evt.getNewValue();
    if( m_timeseriesViewer != null && !m_timeseriesViewer.getTable().isDisposed() )
      m_timeseriesViewer.setFilters( new ViewerFilter[] { new ParameterTypeViewerFilter( parameterType ) } );
  }
}