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

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.TrayDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
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
import org.kalypso.contribs.eclipse.swt.widgets.ControlUtils;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeModel;
import org.kalypsodeegree.model.feature.Feature;

/**
 * This dialog allowes the editing of catchments of a catchment model.
 * 
 * @author Holger Albert
 */
public class EditCatchmentsDialog extends TrayDialog
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
    m_catchmentBean = null;
    m_dataBinding = null;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( final Composite parent )
  {
    /* Set the title. */
    getShell().setText( "Edit Catchment Generator" );

    /* Create the main composite. */
    final Composite main = (Composite) super.createDialogArea( parent );
    main.setLayout( new GridLayout( 1, false ) );
    final GridData mainData = new GridData( SWT.FILL, SWT.FILL, true, true );
    mainData.heightHint = 400;
    mainData.widthHint = 750;
    main.setLayoutData( mainData );

    /* Create the form. */
    Form form = new Form( main, SWT.NONE );
    form.setLayoutData( mainData );

    /* Create the data binding. */
    m_dataBinding = new DatabindingForm( form, null );

    /* Get the body. */
    Composite body = form.getBody();
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
    LinearSumNewComposite composite = new LinearSumNewComposite( parent, m_bean, m_dataBinding );
    composite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    /* Create a label. */
    Label label = new Label( parent, SWT.NONE );
    label.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    label.setText( "Catchments" );

    /* Create a viewer. */
    ListViewer viewer = new ListViewer( parent, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL | SWT.SINGLE );
    viewer.getList().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setLabelProvider( new CatchmentsLabelProvider() );

    /* Set the input. */
    viewer.setInput( m_bean.getCatchments() );

    /* Add a listener. */
    viewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      /**
       * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
       */
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        ISelection selection = event.getSelection();
        if( selection.isEmpty() || !(selection instanceof IStructuredSelection) )
        {
          m_catchmentBean = null;
          updateDetailsGroup( null );
          return;
        }

        Object firstElement = ((IStructuredSelection) selection).getFirstElement();
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
    /* Create a table viewer. */
    TableViewer viewer = CheckboxTableViewer.newCheckList( parent, SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION | SWT.SINGLE );
    viewer.getTable().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    viewer.getTable().setLinesVisible( true );
    viewer.getTable().setHeaderVisible( true );
    viewer.setContentProvider( new ArrayContentProvider() );

    /* Create the columns. */
    createColumns( viewer );

    /* Set the input. */
    if( catchmentBean != null )
      viewer.setInput( catchmentBean.getTimeseries() );
  }

  private void createColumns( final TableViewer viewer )
  {
    /* Create the station column. */
    TableViewerColumn stationColumn = new TableViewerColumn( viewer, SWT.LEFT );
    stationColumn.getColumn().setText( "Station" );
    stationColumn.getColumn().setWidth( 150 );
    stationColumn.setLabelProvider( new StationColumnLabelProvider() );

    /* Create the timestep column. */
    TableViewerColumn timestepColumn = new TableViewerColumn( viewer, SWT.LEFT );
    timestepColumn.getColumn().setText( "Timestep" );
    timestepColumn.getColumn().setWidth( 75 );
    timestepColumn.setLabelProvider( new TimestepColumnLabelProvider() );

    /* Create the quality column. */
    TableViewerColumn qualityColumn = new TableViewerColumn( viewer, SWT.LEFT );
    qualityColumn.getColumn().setText( "Quality" );
    qualityColumn.getColumn().setWidth( 150 );
    qualityColumn.setLabelProvider( new QualityColumnLabelProvider() );

    /* Create the factor column. */
    TableViewerColumn factorColumn = new TableViewerColumn( viewer, SWT.LEFT );
    factorColumn.getColumn().setText( "Factor" );
    factorColumn.getColumn().setWidth( 75 );
    factorColumn.setLabelProvider( new FactorColumnLabelProvider() );
    factorColumn.setEditingSupport( new FactorEditingSupport( viewer ) );
  }

  /**
   * This function updates the details group.
   * 
   * @param catchmentBean
   *          The selected catchment.
   */
  public void updateDetailsGroup( CatchmentBean catchmentBean )
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
   * This function saves the changes.
   */
  private void performOk( )
  {
    try
    {
      /* Apply the changes. */
      Feature generator = m_bean.apply( m_model.getWorkspace() );

      /* Refresh the tree. */
      m_model.refreshTree( generator );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed to save the generator", e ); //$NON-NLS-1$
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
    m_catchmentBean = null;
    m_dataBinding = null;
  }
}