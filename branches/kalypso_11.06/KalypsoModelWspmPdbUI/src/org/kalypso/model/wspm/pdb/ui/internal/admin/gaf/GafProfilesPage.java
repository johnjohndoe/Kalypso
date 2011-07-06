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
package org.kalypso.model.wspm.pdb.ui.internal.admin.gaf;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.databinding.viewers.IViewerObservableValue;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.IOpenListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.OpenEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Table;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.contribs.eclipse.jface.viewers.ColumnViewerUtil;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.core.status.StatusCompositeValue;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.pdb.gaf.GafProfile;
import org.kalypso.model.wspm.pdb.gaf.GafProfiles;
import org.kalypso.model.wspm.pdb.gaf.ImportGafData;

/**
 * @author Gernot Belger
 */
public class GafProfilesPage extends WizardPage
{
  private final ImportGafData m_data;

  private TableViewer m_profileViewer;

  private StatusComposite m_logView;

  protected GafProfilesPage( final String pageName, final ImportGafData data )
  {
    super( pageName );

    m_data = data;

    setTitle( "GAF Inhalt" );
    setDescription( "The following cross sections have been read from the gaf file." );
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    setControl( panel );
    GridLayoutFactory.swtDefaults().numColumns( 2 ).equalWidth( true ).applyTo( panel );

    createProfileTable( panel ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    createLogView( panel ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    final DatabindingWizardPage binding = new DatabindingWizardPage( this, null );
    final StatusCompositeValue target = new StatusCompositeValue( m_logView );
    final IViewerObservableValue model = ViewersObservables.observeSinglePostSelection( m_profileViewer );
    final DataBinder binder = new DataBinder( target, model );
    binder.setModelToTargetConverter( new GafProfileToStatusConverter() );
    binding.bindValue( binder );
  }

  private Control createProfileTable( final Composite parent )
  {
    m_profileViewer = new TableViewer( parent, SWT.BORDER | SWT.FULL_SELECTION );
    m_profileViewer.setContentProvider( new ArrayContentProvider() );

    final Table table = m_profileViewer.getTable();
    table.setHeaderVisible( true );

    table.addControlListener( new ColumnsResizeControlListener() );

    ColumnViewerUtil.createEmptyColumn( m_profileViewer ).setLabelProvider( new ColumnLabelProvider() );

    final TableViewerColumn stationColumn = new TableViewerColumn( m_profileViewer, SWT.NONE );
    stationColumn.getColumn().setText( "Station [m]" );
    stationColumn.getColumn().setResizable( false );
    ColumnsResizeControlListener.setMinimumPackWidth( stationColumn.getColumn() );

    stationColumn.setLabelProvider( new GafProfileStationLabelProvider() );
    stationColumn.getColumn().setAlignment( SWT.RIGHT );

    ColumnViewerSorter.registerSorter( stationColumn, new StationViewerSorter() );

    m_profileViewer.addOpenListener( new IOpenListener()
    {
      @Override
      public void open( final OpenEvent event )
      {
        handleShowProfileStatus( (IStructuredSelection) event.getSelection() );
      }
    } );

    return table;
  }

  protected void handleShowProfileStatus( final IStructuredSelection selection )
  {
    final Object firstElement = selection.getFirstElement();
    if( !(firstElement instanceof GafProfile) )
      return;

    final IStatus status = ((GafProfile) firstElement).getStatus();
    final StatusDialog statusTableDialog = new StatusDialog( getShell(), status, "Details" );
    statusTableDialog.open();
  }

  private Control createLogView( final Composite parent )
  {
    final Group panel = new Group( parent, SWT.NONE );
    panel.setText( "Details" );
    GridLayoutFactory.swtDefaults().applyTo( panel );

    m_logView = new StatusComposite( panel, StatusComposite.DETAILS | StatusComposite.HIDE_DETAILS_IF_DISABLED );
    m_logView.setLayoutData( new GridData( SWT.FILL, SWT.TOP, true, true ) );

    return panel;
  }

  public void updateControl( )
  {
    setErrorMessage( null );
    setPageComplete( true );

    final GafProfiles profiles = m_data.getGafProfiles();

    m_profileViewer.getControl().setEnabled( profiles != null );
    m_logView.setStatus( null );

    if( profiles == null )
    {
      setErrorMessage( "Failed to read GAF file" );
      setPageComplete( false );
      m_profileViewer.setInput( null );
    }
    else
    {
      final GafProfile[] allProfiles = profiles.getProfiles();
      m_profileViewer.setInput( allProfiles );
    }
  }
}