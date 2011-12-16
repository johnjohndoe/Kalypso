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
package org.kalypso.ui.rrm.internal.cm.view;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
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
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.forms.widgets.Form;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.commons.databinding.forms.DatabindingForm;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.contribs.eclipse.swt.widgets.ControlUtils;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.rcm.binding.ILinearSumGenerator;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeModel;
import org.kalypsodeegree.model.feature.Feature;

/**
 * This dialog allowes the editing of catchments of a catchment model.
 * 
 * @author Holger Albert
 */
public class EditCatchmentsDialog extends TrayDialog implements PropertyChangeListener
{
  /**
   * The model.
   */
  private final ITreeNodeModel m_model;

  /**
   * The bean to edit.
   */
  private final LinearSumBean m_bean;

  /**
   * The main group.
   */
  private Group m_mainGroup;

  /**
   * The details group.
   */
  private Group m_detailsGroup;

  /**
   * The timeseries viewer.
   */
  private TableViewer m_timeseriesViewer;

  /**
   * The selected catchment.
   */
  protected CatchmentBean m_catchmentBean;

  /**
   * The data binding.
   */
  private IDataBinding m_dataBinding;

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
  public EditCatchmentsDialog( final Shell shell, final ITreeNodeModel model, final LinearSumBean bean )
  {
    super( shell );

    m_model = model;
    m_bean = bean;

    m_mainGroup = null;
    m_detailsGroup = null;
    m_timeseriesViewer = null;
    m_catchmentBean = null;
    m_dataBinding = null;

    m_bean.addPropertyChangeListener( ILinearSumGenerator.PROPERTY_PARAMETER_TYPE.toString(), this );
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( final Composite parent )
  {
    /* Set the title. */
    getShell().setText( "Edit Catchment Model" );

    /* Create the main composite. */
    final Composite main = (Composite) super.createDialogArea( parent );
    main.setLayout( new GridLayout( 1, false ) );
    final GridData mainData = new GridData( SWT.FILL, SWT.FILL, true, true );
    mainData.heightHint = 550;
    mainData.widthHint = 900;
    main.setLayoutData( mainData );

    /* Create the form. */
    final Form form = new Form( main, SWT.NONE );
    form.setLayoutData( mainData );

    /* Create the data binding. */
    m_dataBinding = new DatabindingForm( form, null );

    /* Get the body. */
    final Composite body = form.getBody();
    body.setLayout( new GridLayout( 2, false ) );

    /* Create the main group. */
    m_mainGroup = new Group( body, SWT.NONE );
    m_mainGroup.setLayout( new GridLayout( 1, false ) );
    final GridData mainGroupData = new GridData( SWT.FILL, SWT.FILL, true, true );
    mainGroupData.widthHint = 250;
    m_mainGroup.setLayoutData( mainGroupData );
    m_mainGroup.setText( "Generator" );

    /* Create the content of the main group. */
    createMainContent( m_mainGroup );

    /* Create the details group. */
    m_detailsGroup = new Group( body, SWT.NONE );
    m_detailsGroup.setLayout( new GridLayout( 1, false ) );
    m_detailsGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    m_detailsGroup.setText( "Details" );

    /* Create the content of the details group. */
    createDetailsContent( m_detailsGroup, null );

    return main;
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
    /* Create the linear sum new composite. */
    final LinearSumNewComposite composite = new LinearSumNewComposite( parent, m_bean, m_dataBinding );
    composite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    /* Create a label. */
    final Label label = new Label( parent, SWT.NONE );
    label.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    label.setText( "Catchments" );

    /* Create the catchment viewer. */
    final TableViewer catchmentViewer = new TableViewer( parent, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION | SWT.SINGLE );
    catchmentViewer.getTable().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    catchmentViewer.getTable().setLinesVisible( true );
    catchmentViewer.getTable().setHeaderVisible( true );
    catchmentViewer.getTable().addControlListener( new ColumnsResizeControlListener() );
    catchmentViewer.setContentProvider( new ArrayContentProvider() );

    /* Create the columns. */
    createCatchmentViewerColumns( catchmentViewer );

    /* Set the input. */
    catchmentViewer.setInput( m_bean.getCatchments() );

    /* Add a listener. */
    catchmentViewer.addSelectionChangedListener( new ISelectionChangedListener()
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
          updateDetailsGroup( null );
          return;
        }

        final Object firstElement = ((IStructuredSelection) selection).getFirstElement();
        if( !(firstElement instanceof CatchmentBean) )
        {
          m_catchmentBean = null;
          updateDetailsGroup( null );
          return;
        }

        m_catchmentBean = (CatchmentBean) firstElement;
        updateDetailsGroup( m_catchmentBean );
      }
    } );
  }

  private void createCatchmentViewerColumns( final TableViewer viewer )
  {
    /* Create the name column. */
    final TableViewerColumn nameColumn = new TableViewerColumn( viewer, SWT.LEFT );
    nameColumn.getColumn().setText( "Name" );
    nameColumn.getColumn().setWidth( 150 );
    nameColumn.setLabelProvider( new NameColumnLabelProvider() );
    ColumnsResizeControlListener.setMinimumPackWidth( nameColumn.getColumn() );
    ColumnViewerSorter.registerSorter( nameColumn, new NameComparator() );

    /* Create the description column. */
    if( m_bean.hasDescription() )
    {
      final TableViewerColumn descriptionColumn = new TableViewerColumn( viewer, SWT.LEFT );
      descriptionColumn.getColumn().setText( "Description" );
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
    final StatusComposite statusComposite = new StatusComposite( parent, SWT.NONE );
    statusComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    /* Set the status. */
    if( catchmentBean == null )
      statusComposite.setStatus( new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), "No catchment selected." ) );
    else
      statusComposite.setStatus( catchmentBean.checkFactor() );

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
        statusComposite.setStatus( catchmentBean.checkFactor() );
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
    groupColumn.getColumn().setText( "Group" );
    groupColumn.getColumn().setWidth( 150 );
    groupColumn.setLabelProvider( new GroupColumnLabelProvider() );
    ColumnsResizeControlListener.setMinimumPackWidth( groupColumn.getColumn() );
    ColumnViewerSorter.registerSorter( groupColumn, new GroupComparator() );

    /* Create the station column. */
    final TableViewerColumn stationColumn = new TableViewerColumn( viewer, SWT.LEFT );
    stationColumn.getColumn().setText( "Station" );
    stationColumn.getColumn().setWidth( 150 );
    stationColumn.setLabelProvider( new StationColumnLabelProvider() );
    ColumnsResizeControlListener.setMinimumPackWidth( stationColumn.getColumn() );
    ColumnViewerSorter.registerSorter( stationColumn, new StationComparator() );

    /* Create the timestep column. */
    final TableViewerColumn timestepColumn = new TableViewerColumn( viewer, SWT.LEFT );
    timestepColumn.getColumn().setText( "Timestep" );
    timestepColumn.getColumn().setWidth( 75 );
    timestepColumn.setLabelProvider( new TimestepColumnLabelProvider() );
    ColumnsResizeControlListener.setMinimumPackWidth( timestepColumn.getColumn() );
    ColumnViewerSorter.registerSorter( timestepColumn, new TimestepComparator() );

    /* Create the quality column. */
    final TableViewerColumn qualityColumn = new TableViewerColumn( viewer, SWT.LEFT );
    qualityColumn.getColumn().setText( "Quality" );
    qualityColumn.getColumn().setWidth( 150 );
    qualityColumn.setLabelProvider( new QualityColumnLabelProvider() );
    ColumnsResizeControlListener.setMinimumPackWidth( qualityColumn.getColumn() );
    ColumnViewerSorter.registerSorter( qualityColumn, new QualityComparator() );

    /* Create the factor column. */
    final TableViewerColumn factorColumn = new TableViewerColumn( viewer, SWT.LEFT );
    factorColumn.getColumn().setText( "Factor [%]" );
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
    m_mainGroup = null;
    m_detailsGroup = null;
    m_timeseriesViewer = null;
    m_catchmentBean = null;
    m_dataBinding = null;
  }

  /**
   * This function updates the details group.
   * 
   * @param catchmentBean
   *          The selected catchment.
   */
  public void updateDetailsGroup( final CatchmentBean catchmentBean )
  {
    /* Cannot do anything. */
    if( m_detailsGroup == null || m_detailsGroup.isDisposed() )
      return;

    /* Dispose all children. */
    ControlUtils.disposeChildren( m_detailsGroup );

    /* Create the content of the details group. */
    createDetailsContent( m_detailsGroup, catchmentBean );

    /* Layout. */
    m_detailsGroup.layout();
  }

  /**
   * @see java.beans.PropertyChangeListener#propertyChange(java.beans.PropertyChangeEvent)
   */
  @Override
  public void propertyChange( final PropertyChangeEvent evt )
  {
    final String parameterType = (String) evt.getNewValue();
    if( m_timeseriesViewer != null && !m_timeseriesViewer.getTable().isDisposed() )
      m_timeseriesViewer.setFilters( new ViewerFilter[] { new ParameterTypeViewerFilter( parameterType ) } );
  }
}