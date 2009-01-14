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
package org.kalypso.project.database.client.ui.project.wizard.info;

import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.project.database.client.KalypsoProjectDatabaseClient;
import org.kalypso.project.database.client.core.model.ProjectHandler;
import org.kalypso.project.database.client.core.utils.KalypsoProjectBeanHelper;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

/**
 * @author kuch
 */
public class RemoteInfoDialog extends TitleAreaDialog
{
  private final KalypsoProjectBean[] m_beans;

  private final boolean m_isExpert;

  protected final ProjectHandler m_handler;

  public RemoteInfoDialog( final ProjectHandler handler, final Shell parentShell, final boolean isExpert )
  {
    super( parentShell );
    m_handler = handler;
    m_isExpert = isExpert;

    setBlockOnOpen( true );
    m_beans = KalypsoProjectBeanHelper.getSortedBeans( handler.getBean() );
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createContents(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createContents( final Composite parent )
  {
    final Control contents = super.createContents( parent );

    setTitle( "Projekthistorie" );
    setMessage( null );

    return contents;
  }

  /**
   * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( final Composite parent )
  {
    final Composite composite = (Composite) super.createDialogArea( parent );
    composite.setLayout( new GridLayout() );
    final GridData data = new GridData( GridData.FILL, GridData.FILL, true, true );
    data.heightHint = 500;
    data.widthHint = 800;
    composite.setLayoutData( data );

    final Composite body = new Composite( composite, SWT.NULL );
    body.setLayout( new GridLayout( 2, true ) );
    body.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

    final Composite projectInfo = new Composite( body, SWT.NULL );
    projectInfo.setLayout( new GridLayout() );
    projectInfo.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

    final Composite projectHistory = new Composite( body, SWT.NULL );
    projectHistory.setLayout( new GridLayout() );
    projectHistory.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

    renderProjectInfo( projectInfo );
    renderProjectHistory( projectHistory );

    return composite;
  }

  private void renderProjectHistory( final Composite parent )
  {
    /* select version */
    final Group groupVersions = new Group( parent, SWT.NULL );
    groupVersions.setLayout( new GridLayout() );
    groupVersions.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    groupVersions.setText( String.format( "Version des Projektes: %s", m_handler.getBean().getName() ) );

    final ComboViewer viewerVersions = new ComboViewer( groupVersions );
    viewerVersions.getCombo().setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    viewerVersions.setContentProvider( new ArrayContentProvider() );
    viewerVersions.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {
        if( element instanceof KalypsoProjectBean )
        {
          final KalypsoProjectBean project = (KalypsoProjectBean) element;

          return String.format( "Version: %d vom %tF", project.getProjectVersion(), project.getCreationDate() );
        }

        return super.getText( element );
      }
    } );

    viewerVersions.setInput( m_beans );

    final Group groupDetails = new Group( parent, SWT.NULL );
    groupDetails.setLayout( new GridLayout( 2, false ) );
    groupDetails.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    groupDetails.setText( "Details" );

    /* version */
    new Label( groupDetails, SWT.NULL ).setText( "Version:" );

    final Text version = new Text( groupDetails, SWT.BORDER | SWT.READ_ONLY );
    version.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    if( m_isExpert )
    {
      /* type */
      new Label( groupDetails, SWT.NULL ).setText( "Projekttyp:" );

      final Text type = new Text( groupDetails, SWT.BORDER | SWT.READ_ONLY );
      type.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

      /* unix name */
      new Label( groupDetails, SWT.NULL ).setText( "Unix Name:" );

      final Text unix = new Text( groupDetails, SWT.BORDER | SWT.READ_ONLY );
      unix.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

      /* server url */
      new Label( groupDetails, SWT.NULL ).setText( "Url:" );

      final Text url = new Text( groupDetails, SWT.BORDER | SWT.READ_ONLY );
      url.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

      viewerVersions.addSelectionChangedListener( new ISelectionChangedListener()
      {
        @Override
        public void selectionChanged( final SelectionChangedEvent event )
        {
          final IStructuredSelection selection = (IStructuredSelection) viewerVersions.getSelection();
          final Object element = selection.getFirstElement();

          if( element instanceof KalypsoProjectBean )
          {
            try
            {
              final KalypsoProjectBean project = (KalypsoProjectBean) element;

              type.setText( project.getProjectType() );
              unix.setText( project.getUnixName() );
              url.setText( project.getUrl().toExternalForm() );
            }
            catch( final Exception e )
            {
              KalypsoProjectDatabaseClient.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
            }
          }
        }
      } );
    }

    final Group groupChanges = new Group( parent, SWT.NULL );
    groupChanges.setLayout( new GridLayout() );
    groupChanges.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    groupChanges.setText( String.format( "Änderungen" ) );

    final Text changes = new Text( groupChanges, SWT.BORDER | SWT.MULTI | SWT.WRAP | SWT.READ_ONLY | SWT.SCROLL_PAGE );
    changes.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    if( m_handler.getBean().getChanges() != null )
    {
      changes.setText( m_handler.getBean().getChanges() );
    }

    /* change listener */
    viewerVersions.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) viewerVersions.getSelection();
        final Object element = selection.getFirstElement();

        if( element instanceof KalypsoProjectBean )
        {
          final KalypsoProjectBean project = (KalypsoProjectBean) element;

          version.setText( String.format( "Version %d erstellt am  %tc", project.getProjectVersion(), project.getCreationDate() ) );

          if( project.getChanges() != null )
          {
            changes.setText( m_handler.getBean().getChanges() );
          }
        }
      }
    } );

    viewerVersions.setSelection( new StructuredSelection( m_handler.getBean() ) );

  }

  private void renderProjectInfo( final Composite parent )
  {
    /* name */
    new Label( parent, SWT.NULL ).setText( "Name:" );

    final Text name = new Text( parent, SWT.BORDER | SWT.READ_ONLY );
    name.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    /* description */
    final Label labelDescription = new Label( parent, SWT.TOP );
    labelDescription.setText( "Beschreibung:" );
    labelDescription.setLayoutData( new GridData( GridData.FILL, GridData.FILL, false, false ) );

    final Text description = new Text( parent, SWT.BORDER | SWT.MULTI | SWT.WRAP );
    description.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

    description.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        try
        {
          final IProjectDescription project = m_handler.getProject().getDescription();
          project.setComment( description.getText() );

          m_handler.getProject().setDescription( project, new NullProgressMonitor() );
        }
        catch( final CoreException e1 )
        {
          KalypsoProjectDatabaseClient.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e1 ) );
        }
      }
    } );

    try
    {
      final IProjectDescription project = m_handler.getProject().getDescription();

      name.setText( project.getName() );
      description.setText( project.getComment() );
    }
    catch( final CoreException e )
    {
      KalypsoProjectDatabaseClient.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
  }

  @Override
  protected void createButtonsForButtonBar( final Composite parent )
  {
    // create OK and Cancel buttons by default
    createButton( parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL, true );
// createButton(parent, IDialogConstants.CANCEL_ID,
// IDialogConstants.CANCEL_LABEL, false);
  }
}
