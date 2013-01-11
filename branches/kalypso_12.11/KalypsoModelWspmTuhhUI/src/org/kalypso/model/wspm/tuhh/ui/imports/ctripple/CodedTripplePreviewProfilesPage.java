/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
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
package org.kalypso.model.wspm.tuhh.ui.imports.ctripple;

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
import org.kalypso.model.wspm.tuhh.core.ctripple.CodedTripple;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Holger Albert
 */
public class CodedTripplePreviewProfilesPage extends WizardPage
{
  private final CodedTrippleImportData m_data;

  private StatusComposite m_statusComposite;

  private TreeViewer m_profileViewer;

  public CodedTripplePreviewProfilesPage( CodedTrippleImportData data )
  {
    super( "codedTripplePreviewProfilesPage" ); //$NON-NLS-1$

    m_data = data;
    m_statusComposite = null;
    m_profileViewer = null;

    setTitle( Messages.getString( "CodedTripplePreviewProfilesPage.0" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "CodedTripplePreviewProfilesPage.1" ) ); //$NON-NLS-1$
  }

  @Override
  public void createControl( Composite parent )
  {
    /* Create the main composite. */
    final Composite main = new Composite( parent, SWT.NONE );
    final GridLayout mainLayout = new GridLayout( 1, false );
    mainLayout.marginHeight = 0;
    mainLayout.marginWidth = 0;
    main.setLayout( mainLayout );

    /* Create a group. */
    final Group statusGroup = new Group( main, SWT.NONE );
    statusGroup.setText( Messages.getString( "CodedTripplePreviewProfilesPage.2" ) ); //$NON-NLS-1$
    statusGroup.setLayout( new GridLayout( 1, false ) );
    statusGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    /* Create a status composite. */
    m_statusComposite = new StatusComposite( statusGroup, StatusComposite.DETAILS );
    m_statusComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* Create a group. */
    final Group profileGroup = new Group( main, SWT.NONE );
    profileGroup.setText( Messages.getString( "CodedTripplePreviewProfilesPage.3" ) ); //$NON-NLS-1$
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
    m_profileViewer.setContentProvider( new CodedTrippleProfilesContentProvider() );

    /* Add the column resize control listener. */
    m_profileViewer.getTree().addControlListener( new ColumnsResizeControlListener() );

    /* Set the control to the page. */
    setControl( main );
  }

  private void configureTreeViewer( final TreeViewer viewer )
  {
    /* Name. */
    final ViewerColumn nameColumn = ColumnViewerUtil.createViewerColumn( viewer, SWT.LEFT );
    nameColumn.setLabelProvider( new CodedTrippleProfileNameLabelProvider() );

    final ViewerColumnItem nameItem = new ViewerColumnItem( nameColumn );
    nameItem.setText( Messages.getString( "CodedTripplePreviewProfilesPage.4" ) ); //$NON-NLS-1$
    nameItem.setResizable( false );

    ColumnsResizeControlListener.setMinimumPackWidth( nameItem.getColumn() );

    /* Station. */
    final ViewerColumn stationColumn = ColumnViewerUtil.createViewerColumn( viewer, SWT.LEFT );
    stationColumn.setLabelProvider( new CodedTrippleProfileStationLabelProvider() );

    final ViewerColumnItem stationItem = new ViewerColumnItem( stationColumn );
    stationItem.setText( Messages.getString( "CodedTripplePreviewProfilesPage.5" ) ); //$NON-NLS-1$
    stationItem.setResizable( false );

    ColumnsResizeControlListener.setMinimumPackWidth( stationItem.getColumn() );

    /* Description. */
    final ViewerColumn descriptionColumn = ColumnViewerUtil.createViewerColumn( viewer, SWT.LEFT );
    descriptionColumn.setLabelProvider( new CodedTrippleProfileDescriptionLabelProvider() );

    final ViewerColumnItem descriptionItem = new ViewerColumnItem( descriptionColumn );
    descriptionItem.setText( Messages.getString( "CodedTripplePreviewProfilesPage.6" ) ); //$NON-NLS-1$
    descriptionItem.setResizable( false );

    ColumnsResizeControlListener.setMinimumPackWidth( descriptionItem.getColumn() );
  }

  public void updateControls( )
  {
    m_statusComposite.setStatus( m_data.getCodedTrippleDataStatus() );

    final CodedTripple codedTrippleData = m_data.getCodedTrippleData();
    if( codedTrippleData != null )
      m_profileViewer.setInput( codedTrippleData );
    else
      m_profileViewer.setInput( null );
  }
}