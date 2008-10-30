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
package org.kalypso.project.database.client.ui.project.list.internal;

import java.net.MalformedURLException;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.kalypso.contribs.eclipse.core.resources.ProjectTemplate;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.project.database.client.KalypsoProjectDatabaseClient;
import org.kalypso.project.database.client.core.model.ProjectDataBaseController;
import org.kalypso.project.database.client.core.model.ProjectHandler;
import org.kalypso.project.database.client.core.project.workspace.DeleteLocalProjectHandler;
import org.kalypso.project.database.client.core.utils.ProjectDatabaseServerUtils;
import org.kalypso.project.database.common.nature.IRemoteProjectPreferences;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

/**
 * Row builder for projects, which existing locally and remote
 * 
 * @author Dirk Kuch
 */
public class LocalServerProjectRowBuilder extends AbstractProjectRowBuilder implements IProjectRowBuilder
{
  protected final ProjectHandler m_handler;

  public LocalServerProjectRowBuilder( final ProjectHandler handler )
  {
    m_handler = handler;
  }

  /**
   * @see org.kalypso.project.database.client.ui.project.internal.IProjectRowBuilder#render(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  @Override
  public void render( final Composite parent, final FormToolkit toolkit )
  {
    try
    {
      /* settings */
      final IRemoteProjectPreferences preferences = m_handler.getRemotePreferences();

      final Composite body = toolkit.createComposite( parent );
      body.setLayout( new GridLayout( 6, false ) );
      body.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

      final ImageHyperlink lnk = toolkit.createImageHyperlink( body, SWT.NONE );
      lnk.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

      if( preferences.isLocked() )
        lnk.setImage( IMG_LORE_LOCKED );
      else if( m_handler.getBean().isProjectLockedForEditing() )
        lnk.setImage( IMG_LORE_OTHER_LOCK );
      else
        lnk.setImage( IMG_LORE_PROJECT );

      lnk.setToolTipText( String.format( "Öffne Projekt: %s", m_handler.getName() ) );
      lnk.setText( m_handler.getName() );

      // info
      RemoteProjectRowBuilder.getInfoLink( m_handler, body, toolkit );

      // lock project
      createLockHyperlink( body, toolkit );

      getSpacer( body, toolkit );

      // export
      LocalProjectRowBuilder.getExportLink( m_handler, body, toolkit );

      /* delete */
      LocalProjectRowBuilder.getDeleteLink( m_handler, body, toolkit );
    }
    catch( final CoreException e1 )
    {
      KalypsoProjectDatabaseClient.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e1 ) );
    }
  }

  private void createLockHyperlink( final Composite body, final FormToolkit toolkit ) throws CoreException
  {

    final IRemoteProjectPreferences preferences = m_handler.getRemotePreferences();

    if( preferences.isLocked() )
    {
      final ImageHyperlink lnkLock = toolkit.createImageHyperlink( body, SWT.NONE );
      lnkLock.setToolTipText( String.format( "Übertrage Projekt \"%s\" in Modelldaten-Basis und gebe Projekt vom Editieren frei.", m_handler.getName() ) );

      if( ProjectDatabaseServerUtils.isServerOnline() )
      {
        lnkLock.setImage( IMG_LORE_COMMIT_AND_UNLOCK );

        lnkLock.addHyperlinkListener( new HyperlinkAdapter()
        {
          /**
           * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
           */
          @Override
          public void linkActivated( final HyperlinkEvent e )
          {
            final Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();

            /* commit */
            final IStatus commitStatus = ProjectDataBaseController.updateProject( m_handler );
            if( !shell.isDisposed() )
              ErrorDialog.openError( shell, "Fehler", "Aktualisieren des Projektes ist fehlgeschlagen.", commitStatus );

            if( !commitStatus.isOK() )
              return;

            /* release */
            final IStatus lockStatus = ProjectDataBaseController.releaseProjectLock( m_handler );
            if( !shell.isDisposed() )
              ErrorDialog.openError( shell, "Fehler", "Freigeben des Projektes ist fehlgeschlagen.", lockStatus );
          }
        } );
      }
      else
      {
        lnkLock.setImage( IMG_LORE_COMMIT_AND_UNLOCK_DISABLED );
        lnkLock.setEnabled( false );
      }

    }
    else
    {

      if( ProjectDatabaseServerUtils.isServerOnline() )
      {
        if( m_handler.getBean().isProjectLockedForEditing() )
        {
          if( ProjectDatabaseServerUtils.isUpdateAvailable( m_handler ) )
          {
            getUpdateLink( body, toolkit );
          }
          else
          {
            getSpacer( body, toolkit );
          }

        }
        else
        {
          if( ProjectDatabaseServerUtils.isUpdateAvailable( m_handler ) )
          {
            getUpdateLink( body, toolkit );
          }
          else
          {

            final ImageHyperlink lnkLock = toolkit.createImageHyperlink( body, SWT.NONE );
            lnkLock.setToolTipText( String.format( "Sperre Projekt \"%s\" zum Editieren.", m_handler.getName() ) );
            lnkLock.setImage( IMG_LORE_LOCK );

            lnkLock.addHyperlinkListener( new HyperlinkAdapter()
            {
              /**
               * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
               */
              @Override
              public void linkActivated( final HyperlinkEvent e )
              {
                final IStatus lockStatus = ProjectDataBaseController.acquireProjectLock( m_handler );

                final Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
                if( !shell.isDisposed() )
                  ErrorDialog.openError( shell, "Fehler", "Sperren des Projektes zum Editieren ist fehlgeschlagen.", lockStatus );
              }
            } );
          }
        }
      }
      else
      {
        final ImageHyperlink lnkLock = toolkit.createImageHyperlink( body, SWT.NONE );
        lnkLock.setImage( IMG_LORE_LOCK_DISABLED );
        lnkLock.setEnabled( false );
      }

    }

  }

  private void getUpdateLink( final Composite body, final FormToolkit toolkit )
  {
    try
    {
      final Integer remoteVersion = m_handler.getBean().getProjectVersion();
      final Integer localVersion = m_handler.getRemotePreferences().getVersion();

      final ImageHyperlink lnkUpdate = toolkit.createImageHyperlink( body, SWT.NONE );
      lnkUpdate.setImage( IMG_LORE_UPDATEABLE );
      lnkUpdate.setToolTipText( String.format( "Neue Version des Projektes auf dem Server verfügbar. Lokale Version: %d Server Version: %d", localVersion, remoteVersion ) );

      lnkUpdate.addHyperlinkListener( new HyperlinkAdapter()
      {
        /**
         * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
         */
        @Override
        public void linkActivated( final HyperlinkEvent e )
        {
          // delete local project
          final DeleteLocalProjectHandler delete = new DeleteLocalProjectHandler( m_handler.getProject() );
          final IStatus status = ProgressUtilities.busyCursorWhile( delete );

          final Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
          if( !shell.isDisposed() )
            ErrorDialog.openError( shell, "Löschen fehlgeschlagen", "Fehler beim Löschen des Projektes", status );

          // import head version of project from server
          try
          {
            /* sort beans */

            // bad hack - to determine which project was newly created
            final Map<ProjectTemplate, KalypsoProjectBean> mapping = new HashMap<ProjectTemplate, KalypsoProjectBean>();

            final KalypsoProjectBean bean = m_handler.getBean();
            final ProjectTemplate template = new ProjectTemplate( String.format( "%s - Version %d", bean.getName(), bean.getProjectVersion() ), bean.getUnixName(), bean.getDescription(), null, bean.getUrl() );
            mapping.put( template, bean );

            RemoteProjectHelper.importRemoteProject( new ProjectTemplate[] { template }, mapping );
          }
          catch( final MalformedURLException e1 )
          {
            KalypsoProjectDatabaseClient.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e1 ) );
          }
        }
      } );

    }
    catch( final CoreException e )
    {
      KalypsoProjectDatabaseClient.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
  }
}
