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

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.IOpenListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.OpenEvent;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.internal.WorkbenchMessages;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.wspm.WaterlevelsForStation;

/**
 * @author Gernot Belger
 */
@SuppressWarnings( "restriction" )
public class ImportWaterlevelsPreviewPage extends WizardPage implements IUpdateable
{
  private final ImportWaterLevelsData m_data;

  private CheckboxTableViewer m_viewer;

  private WaterlevelsForStation[] m_input;

  private Composite m_buttonsPanel;

  protected ImportWaterlevelsPreviewPage( final String pageName, final ImportWaterLevelsData data )
  {
    super( pageName );

    setTitle( Messages.getString( "ImportWaterlevelsPreviewPage.0" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "ImportWaterlevelsPreviewPage.1" ) ); //$NON-NLS-1$

    m_data = data;
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    setControl( panel );
    GridLayoutFactory.swtDefaults().applyTo( panel );

    createWaterTable( panel );
    createSelectButtons( panel );
  }

  private void createWaterTable( final Composite panel )
  {
    final Set<WaterlevelsForStation> checkedWaterlevels = m_data.getWaterlevels();

    m_viewer = CheckboxTableViewer.newCheckList( panel, SWT.BORDER | SWT.FULL_SELECTION );
    m_viewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    m_viewer.setUseHashlookup( true );

    final Table table = m_viewer.getTable();
    table.setHeaderVisible( true );
    table.addControlListener( new ColumnsResizeControlListener() );

    m_viewer.setContentProvider( new ArrayContentProvider() );
    m_viewer.setCheckStateProvider( new EventFixationsCheckstateProvider( checkedWaterlevels ) );

    final TableViewerColumn validColumn = new TableViewerColumn( m_viewer, SWT.LEFT );
    validColumn.getColumn().setText( Messages.getString( "ImportWaterlevelsPreviewPage.2" ) ); //$NON-NLS-1$
    validColumn.getColumn().setResizable( false );
    ColumnsResizeControlListener.setMinimumPackWidth( validColumn.getColumn() );
    ColumnViewerSorter.registerSorter( validColumn, new ViewerComparator() );
    validColumn.setLabelProvider( new WaterLevelImportStatusLabelProvider() );

    createWaterlevelColumn( m_viewer, WaterlevelFixationStrings.STATION, WaterlevelsForStation.PROPERTY_STATION, "%s", SWT.RIGHT ); //$NON-NLS-1$
    createWaterlevelColumn( m_viewer, Messages.getString( "ImportWaterlevelsPreviewPage.3" ), WaterlevelsForStation.PROPERTY_WATERLEVEL_COUNT, "%,d", SWT.RIGHT ); //$NON-NLS-1$ //$NON-NLS-2$
    createWaterlevelColumn( m_viewer, Messages.getString( "ImportWaterlevelsPreviewPage.4" ), WaterlevelsForStation.PROPERTY_WATERLEVEL_SIMPLIFIED_COUNT, "%,d", SWT.RIGHT ); //$NON-NLS-1$ //$NON-NLS-2$
    createWaterlevelColumn( m_viewer, Messages.getString( "ImportWaterlevelsPreviewPage.5" ), WaterlevelsForStation.PROPERTY_WATERLEVEL_SEGMENT_COUNT, "%,d", SWT.RIGHT ); //$NON-NLS-1$ //$NON-NLS-2$
    // createWaterlevelColumn( m_viewer, WaterlevelFixationStrings.WATERLEVEL, WaterlevelsForStation.PROPERTY_WATERLEVEL, "%s" ); //$NON-NLS-1$
    // createWaterlevelColumn( m_viewer, WaterlevelFixationStrings.DISCHARGE, WaterlevelsForStation.PROPERTY_DISCHARGE, "%s" ); //$NON-NLS-1$
    // createWaterlevelColumn( m_viewer, WaterlevelFixationStrings.MEASUREMENT, WaterlevelsForStation.PROPERTY_MEASURMENT_DATE, "%s" ); //$NON-NLS-1$
    // createWaterlevelColumn( m_viewer, WaterlevelFixationStrings.DESCRIPTION, WaterlevelsForStation.PROPERTY_DESCRIPTION, "%s" ); //$NON-NLS-1$

    m_viewer.addCheckStateListener( new ICheckStateListener()
    {
      @Override
      public void checkStateChanged( final CheckStateChangedEvent event )
      {
        final boolean checked = event.getChecked();
        final WaterlevelsForStation waterlevel = (WaterlevelsForStation)event.getElement();
        if( checked && waterlevel.isValid() )
          checkedWaterlevels.add( waterlevel );
        else
          checkedWaterlevels.remove( waterlevel );
      }
    } );

    m_viewer.addOpenListener( new IOpenListener()
    {
      @Override
      public void open( final OpenEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection)event.getSelection();
        final WaterlevelsForStation waterlevel = (WaterlevelsForStation)selection.getFirstElement();
        if( waterlevel == null )
          return;

        final IStatus validate = waterlevel.validate();
        StatusDialog.open( getShell(), validate, getWizard().getWindowTitle() );
      }
    } );

    // TODO: show list of read waterlevels on double click
  }

  private void createWaterlevelColumn( final CheckboxTableViewer viewer, final String label, final String property, final String format, final int alignment )
  {
    final TableViewerColumn viewerColumn = new TableViewerColumn( viewer, alignment );

    final TableColumn column = viewerColumn.getColumn();
    column.setText( label );
    column.setResizable( false );

    ColumnsResizeControlListener.setMinimumPackWidth( column );
    viewerColumn.setLabelProvider( new WaterLevelLabelProvider( property, format ) );
  }

  private void createSelectButtons( final Composite parent )
  {
    final Set<WaterlevelsForStation> checkedWaterlevels = m_data.getWaterlevels();

    m_buttonsPanel = new Composite( parent, SWT.NONE );
    GridLayoutFactory.fillDefaults().numColumns( 2 ).applyTo( m_buttonsPanel );
    m_buttonsPanel.setLayoutData( new GridData( SWT.RIGHT, SWT.CENTER, true, false ) );

    final Button selectAllButton = new Button( m_buttonsPanel, SWT.PUSH );
    selectAllButton.setText( WorkbenchMessages.SelectionDialog_selectLabel );
    selectAllButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final org.eclipse.swt.events.SelectionEvent e )
      {
        checkedWaterlevels.clear();

        final WaterlevelsForStation[] waterLevels = getWaterLevels();
        for( final WaterlevelsForStation waterlevel : waterLevels )
        {
          if( waterlevel.isValid() )
            checkedWaterlevels.add( waterlevel );
        }
        getViewer().update( waterLevels, null );
      }
    } );

    final Button deselectAllButton = new Button( m_buttonsPanel, SWT.PUSH );
    deselectAllButton.setText( WorkbenchMessages.SelectionDialog_deselectLabel );
    deselectAllButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final org.eclipse.swt.events.SelectionEvent e )
      {
        checkedWaterlevels.clear();
        getViewer().update( getWaterLevels(), null );
      }
    } );
  }

  protected WaterlevelsForStation[] getWaterLevels( )
  {
    return m_input;
  }

  protected CheckboxTableViewer getViewer( )
  {
    return m_viewer;
  }

  @Override
  public void update( )
  {
    /* reset table */
    final Set<WaterlevelsForStation> checkedWaterlevels = m_data.getWaterlevels();
    checkedWaterlevels.clear();
    m_viewer.setInput( new WaterlevelsForStation[] {} );

    final WaterlevelsForStation[] readWaterLevels = readWaterLevels();
    if( readWaterLevels == null )
    {
      /* cancelled or error */
      return;
    }

    m_input = readWaterLevels;

    /* update waterlevels in event */
    final Collection<WaterlevelsForStation> goodWaterlevels = new HashSet<>();
    for( final WaterlevelsForStation waterlevel : m_input )
    {
      if( waterlevel.isValid() )
        checkedWaterlevels.add( waterlevel );
    }
    checkedWaterlevels.addAll( goodWaterlevels );

    setPreviewInput();
  }

  private WaterlevelsForStation[] readWaterLevels( )
  {
    final ReadWaterLevelsOperation operation = new ReadWaterLevelsOperation( m_data );

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, true, operation );
    if( !status.isOK() )
      StatusDialog.open( getShell(), status, Messages.getString( "ImportWaterlevelsPreviewPage.8" ) ); //$NON-NLS-1$

    return operation.getWaterlevels();
  }

  private void setPreviewInput( )
  {
    if( m_input == null )
      return;

    m_viewer.setInput( m_input );

    ColumnsResizeControlListener.refreshColumnsWidth( m_viewer.getTable() );
  }
}