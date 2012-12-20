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
package org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.imports;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.set.WritableSet;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.databinding.viewers.IViewerObservableSet;
import org.eclipse.jface.databinding.viewers.IViewerObservableValue;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.IOpenListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.OpenEvent;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.internal.WorkbenchMessages;
import org.kalypso.commons.databinding.DataSetBinder;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.WaterBodyViewer;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
@SuppressWarnings("restriction")
public class ImportWaterbodiesPreviewPage extends WizardPage implements IUpdateable
{
  private final Map<WaterBody, IStatus> m_waterBodyStatus = new HashMap<>();

  private final ImportWaterBodiesData m_data;

  private DataBindingContext m_binding;

  private CheckboxTableViewer m_viewer;

  protected ImportWaterbodiesPreviewPage( final String pageName, final ImportWaterBodiesData data )
  {
    super( pageName );

    setTitle( Messages.getString( "ImportWaterbodiesPreviewPage.0" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "ImportWaterbodiesPreviewPage.1" ) ); //$NON-NLS-1$

    m_data = data;
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    setControl( panel );
    GridLayoutFactory.swtDefaults().applyTo( panel );

    m_binding = new DataBindingContext();

    createWaterTable( panel );
    createSelectButtons( panel );
    createModeChooser( panel );
  }

  private void createWaterTable( final Composite panel )
  {
    m_viewer = CheckboxTableViewer.newCheckList( panel, SWT.BORDER | SWT.FULL_SELECTION );
    m_viewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    WaterBodyViewer.configureViewer( m_viewer );
    m_viewer.setContentProvider( new ArrayContentProvider() );

    WaterBodyViewer.createRankColumn( m_viewer );

    final TableViewerColumn validColumn = new TableViewerColumn( m_viewer, SWT.LEFT );
    validColumn.getColumn().setText( Messages.getString( "ImportWaterbodiesPreviewPage.2" ) ); //$NON-NLS-1$
    validColumn.getColumn().setResizable( false );
    ColumnsResizeControlListener.setMinimumPackWidth( validColumn.getColumn() );
    ColumnViewerSorter.registerSorter( validColumn, new ViewerComparator() );
    validColumn.setLabelProvider( new WaterBodyImportStatusLabelProvider( m_data.getExistingWaterBodies(), m_waterBodyStatus ) );

    final IObservableValue target = ViewersObservables.observeInput( m_viewer );
    final IObservableValue model = BeansObservables.observeValue( m_data, ImportWaterBodiesData.PROPERTY_WATER_BODIES );

    m_binding.bindValue( target, model );

    final IViewerObservableSet targetCheck = ViewersObservables.observeCheckedElements( m_viewer, WaterBody.class );
    final WritableSet modelCheck = m_data.getSelectedWaterBodies();

    final DataSetBinder checkBinder = new DataSetBinder( targetCheck, modelCheck );
    checkBinder.apply( m_binding );

    m_viewer.addOpenListener( new IOpenListener()
    {
      @Override
      public void open( final OpenEvent event )
      {
        showElementStatus( (IStructuredSelection) event.getSelection() );
      }
    } );
  }

  protected void showElementStatus( final IStructuredSelection selection )
  {
    final Object element = selection.getFirstElement();
    if( !(element instanceof WaterBody) )
      return;

    final WaterBody water = (WaterBody) element;
    final IStatus status = m_waterBodyStatus.get( water );
    if( status == null )
      return;

    StatusDialog.open( getShell(), status, getWizard().getWindowTitle() );
  }

  private void createSelectButtons( final Composite parent )
  {
    final ImportWaterBodiesData data = m_data;

    final Composite panel = new Composite( parent, SWT.NONE );
    GridLayoutFactory.fillDefaults().numColumns( 2 ).applyTo( panel );
    panel.setLayoutData( new GridData( SWT.RIGHT, SWT.CENTER, true, false ) );

    final Button selectAllButton = new Button( panel, SWT.PUSH );
    selectAllButton.setText( WorkbenchMessages.SelectionDialog_selectLabel );
    selectAllButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final org.eclipse.swt.events.SelectionEvent e )
      {
        data.getSelectedWaterBodies().addAll( Arrays.asList( data.getWaterBodies() ) );
      }
    } );

    final Button deselectAllButton = new Button( panel, SWT.PUSH );
    deselectAllButton.setText( WorkbenchMessages.SelectionDialog_deselectLabel );
    deselectAllButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final org.eclipse.swt.events.SelectionEvent e )
      {
        data.getSelectedWaterBodies().clear();
      }
    } );
  }

  private void createModeChooser( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    GridLayoutFactory.fillDefaults().numColumns( 2 ).applyTo( panel );

    new Label( panel, SWT.NONE ).setText( Messages.getString( "ImportWaterbodiesPreviewPage.3" ) ); //$NON-NLS-1$

    final ComboViewer viewer = new ComboViewer( panel, SWT.DROP_DOWN | SWT.READ_ONLY );
    viewer.setLabelProvider( new LabelProvider() );
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setInput( ImportWaterBodiesData.INSERTION_MODE.values() );

    final IViewerObservableValue target = ViewersObservables.observeSinglePostSelection( viewer );
    final IObservableValue model = BeansObservables.observeValue( m_data, ImportWaterBodiesData.PROPERTY_INSERTION_MODE );
    m_binding.bindValue( target, model );
  }

  @Override
  public void update( )
  {
    m_data.getSelectedWaterBodies().clear();

    final WaterBody[] waterBodies = readWaterBodies();
    m_data.setWaterBodies( waterBodies );
  }

  private WaterBody[] readWaterBodies( )
  {
    final ReadWaterBodiesOperation operation = new ReadWaterBodiesOperation( m_data, m_waterBodyStatus );

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, operation );
    if( !status.isOK() )
      new StatusDialog( getShell(), status, Messages.getString( "ImportWaterbodiesPreviewPage.4" ) ).open(); //$NON-NLS-1$

    return operation.getWaterBodies();
  }
}