/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Bj�rnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universit�t Hamburg-Harburg, Institut f�r Wasserbau, Hamburg, Germany
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
package org.kalypso.model.wspm.tuhh.ui.imports.ewawi;

import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerColumn;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.contribs.eclipse.jface.viewers.ColumnViewerUtil;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerColumnItem;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.model.wspm.ewawi.data.EwawiPlus;
import org.kalypso.model.wspm.ewawi.data.EwawiPro;

/**
 * @author Holger Albert
 */
public class EwawiPreviewProfilesPage extends WizardPage
{
  private final EwawiImportData m_data;

  private StatusComposite m_statusComposite;

  private TreeViewer m_profileViewer;

  public EwawiPreviewProfilesPage( final EwawiImportData data )
  {
    super( "ewawiPreviewProfilesPage" );

    m_data = data;
    m_statusComposite = null;
    m_profileViewer = null;

    setTitle( "Profilvorschau" );
    setDescription( "Vorschau der zu importierenden Profile." );
  }

  @Override
  public void createControl( final Composite parent )
  {
    /* Create the main composite. */
    final Composite main = new Composite( parent, SWT.NONE );
    final GridLayout mainLayout = new GridLayout( 1, false );
    mainLayout.marginHeight = 0;
    mainLayout.marginWidth = 0;
    main.setLayout( mainLayout );

    /* Create a group. */
    final Group statusGroup = new Group( main, SWT.NONE );
    statusGroup.setText( "Dateiinfo" );
    statusGroup.setLayout( new GridLayout( 1, false ) );
    statusGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    /* Create a status composite. */
    m_statusComposite = new StatusComposite( statusGroup, StatusComposite.DETAILS );
    m_statusComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* Create a group. */
    final Group profileGroup = new Group( main, SWT.NONE );
    profileGroup.setText( "Profilvorschau" );
    profileGroup.setLayout( new GridLayout( 1, false ) );
    profileGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* Create a tree viewer. */
    m_profileViewer = new TreeViewer( profileGroup, SWT.BORDER | SWT.SINGLE | SWT.FULL_SELECTION | SWT.H_SCROLL | SWT.V_SCROLL );
    final GridData treeData = new GridData( SWT.FILL, SWT.FILL, true, true );
    treeData.heightHint = 200;
    m_profileViewer.getTree().setLayoutData( treeData );
    m_profileViewer.getTree().setLinesVisible( true );
    m_profileViewer.getTree().setHeaderVisible( true );
    configureTreeViewer( m_profileViewer );
    m_profileViewer.setContentProvider( new EwawiProfilesContentProvider() );

    /* Add the column resize control listener. */
    m_profileViewer.getTree().addControlListener( new ColumnsResizeControlListener() );

    /* Set the control to the page. */
    setControl( main );
  }

  private void configureTreeViewer( final TreeViewer viewer )
  {
    /* Station. */
    final ViewerColumn stationColumn = ColumnViewerUtil.createViewerColumn( viewer, SWT.LEFT );
    stationColumn.setLabelProvider( new EwawiProfileStationLabelProvider() );

    final ViewerColumnItem stationItem = new ViewerColumnItem( stationColumn );
    stationItem.setText( "Station" );
    stationItem.setResizable( false );

    ColumnsResizeControlListener.setMinimumPackWidth( stationItem.getColumn() );

    /* Description. */
    final ViewerColumn descriptionColumn = ColumnViewerUtil.createViewerColumn( viewer, SWT.LEFT );
    descriptionColumn.setLabelProvider( new EwawiProfileDescriptionLabelProvider() );

    final ViewerColumnItem descriptionItem = new ViewerColumnItem( descriptionColumn );
    descriptionItem.setText( "GKZ" );
    descriptionItem.setResizable( false );

    ColumnsResizeControlListener.setMinimumPackWidth( descriptionItem.getColumn() );
  }

  public void updateControls( )
  {
    m_statusComposite.setStatus( m_data.getEwawiDataStatus() );

    final EwawiPlus ewawiData = m_data.getEwawiData();
    if( ewawiData != null )
    {
      final EwawiPro proIndex = ewawiData.getProIndex();
      if( proIndex != null )
        m_profileViewer.setInput( proIndex.getProfiles() );
      else
        m_profileViewer.setInput( null );
    }
    else
      m_profileViewer.setInput( null );
  }
}