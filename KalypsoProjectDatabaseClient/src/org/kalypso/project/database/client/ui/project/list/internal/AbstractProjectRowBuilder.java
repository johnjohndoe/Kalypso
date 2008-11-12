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
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.kalypso.contribs.eclipse.core.resources.ProjectTemplate;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.project.database.client.KalypsoProjectDatabaseClient;
import org.kalypso.project.database.client.core.model.ProjectDataBaseController;
import org.kalypso.project.database.client.core.model.ProjectHandler;
import org.kalypso.project.database.client.core.project.workspace.DeleteLocalProjectHandler;
import org.kalypso.project.database.client.core.utils.ProjectDatabaseServerUtils;
import org.kalypso.project.database.client.ui.project.list.IProjectDatabaseUiLocker;
import org.kalypso.project.database.client.ui.project.wizard.commit.WizardCommitProject;
import org.kalypso.project.database.client.ui.project.wizard.export.WizardProjectExport;
import org.kalypso.project.database.client.ui.project.wizard.info.RemoteInfoDialog;
import org.kalypso.project.database.common.nature.IRemoteProjectPreferences;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

/**
 * @author Dirk Kuch
 */
public abstract class AbstractProjectRowBuilder implements IProjectRowBuilder
{

  private final ProjectHandler m_handler;

  private final IProjectDatabaseUiLocker m_locker;

  public AbstractProjectRowBuilder( final ProjectHandler handler, final IProjectDatabaseUiLocker locker )
  {
    m_handler = handler;
    m_locker = locker;

  }

  protected void getSpacer( final Composite parent, final FormToolkit toolkit )
  {
    final ImageHyperlink lnk = toolkit.createImageHyperlink( parent, SWT.NONE );
    lnk.setText( "" );
    final GridData data = new GridData( GridData.FILL, GridData.FILL, false, false );
    data.minimumWidth = data.widthHint = 18;
    lnk.setLayoutData( data );
    lnk.setEnabled( false );
    lnk.setUnderlined( false );
  }

  protected ProjectHandler getHandler( )
  {
    return m_handler;
  }

  protected IProjectDatabaseUiLocker getLocker( )
  {
    return m_locker;
  }

  protected void getInfoLink( final Composite body, final FormToolkit toolkit, final boolean isExpert )
  {
    final ImageHyperlink lnkInfo = toolkit.createImageHyperlink( body, SWT.NONE );
    lnkInfo.setToolTipText( String.format( "Projekthistorie: %s", getHandler().getName() ) );

    if( getHandler().isRemote() && ProjectDatabaseServerUtils.isServerOnline() )
    {
      lnkInfo.setImage( IMG_REMOTE_INFO );

      lnkInfo.addHyperlinkListener( new HyperlinkAdapter()
      {
        /**
         * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
         */
        @Override
        public void linkActivated( final HyperlinkEvent e )
        {
          try
          {
            getLocker().acquireUiUpdateLock();

            final RemoteInfoDialog dialog = new RemoteInfoDialog( getHandler().getBean(), lnkInfo.getShell(), isExpert );
            dialog.open();
          }
          finally
          {
            getLocker().releaseUiUpdateLock();
          }

        }
      } );
    }
    else
    {
      lnkInfo.setImage( IMG_REMOTE_INFO_DISABLED );
      lnkInfo.setEnabled( false );
    }
  }

  protected void getDeleteLink( final Composite body, final FormToolkit toolkit )
  {
    final ImageHyperlink lnkDelete = toolkit.createImageHyperlink( body, SWT.NONE );
    lnkDelete.setImage( IMG_DELETE_LOCAL );
    lnkDelete.setToolTipText( String.format( "Lösche Projekt: %s", getHandler().getName() ) );

    lnkDelete.addHyperlinkListener( new HyperlinkAdapter()
    {
      /**
       * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
       */
      @Override
      public void linkActivated( final HyperlinkEvent e )
      {
        try
        {
          getLocker().acquireUiUpdateLock();

          if( MessageDialog.openConfirm( lnkDelete.getShell(), "Lösche Projekt", String.format( "Projekt \"%s\" wirklich löschen?", getHandler().getName() ) ) )
          {
            final DeleteLocalProjectHandler delete = new DeleteLocalProjectHandler( getHandler().getProject() );
            final IStatus status = ProgressUtilities.busyCursorWhile( delete );

            final Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
            if( shell != null && !shell.isDisposed() )
              ErrorDialog.openError( shell, "Löschen fehlgeschlagen", "Fehler beim Löschen des Projektes", status );
          }
        }
        finally
        {
          getLocker().releaseUiUpdateLock();
        }
      }
    } );

  }

  protected void getExportLink( final Composite body, final FormToolkit toolkit )
  {
    final ImageHyperlink lnkExport = toolkit.createImageHyperlink( body, SWT.NONE );
    lnkExport.setImage( IMG_EXPORT_LOCAL );
    lnkExport.setToolTipText( String.format( "Exportiere Projekt: %s", getHandler().getName() ) );

    lnkExport.addHyperlinkListener( new HyperlinkAdapter()
    {
      /**
       * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
       */
      @Override
      public void linkActivated( final HyperlinkEvent e )
      {
        try
        {
          getLocker().acquireUiUpdateLock();

          final WizardProjectExport wizard = new WizardProjectExport( getHandler().getProject() );
          wizard.init( PlatformUI.getWorkbench(), new StructuredSelection( getHandler().getProject() ) );

          final WizardDialog dialog = new WizardDialog( null, wizard );
          dialog.open();
        }
        finally
        {
          getLocker().releaseUiUpdateLock();
        }
      }
    } );

  }

  protected void createLockHyperlink( final Composite body, final FormToolkit toolkit ) throws CoreException
  {

    final IRemoteProjectPreferences preferences = getHandler().getRemotePreferences();

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
            try
            {
              getLocker().acquireUiUpdateLock();

              final Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();

              /* commit */
              final WizardCommitProject wizard = new WizardCommitProject( getHandler() );
              final WizardDialog2 dialog = new WizardDialog2( null, wizard );
              dialog.open();

              /* release */
              final IStatus lockStatus = ProjectDataBaseController.releaseProjectLock( getHandler() );
              if( !shell.isDisposed() )
                ErrorDialog.openError( shell, "Fehler", "Freigeben des Projektes ist fehlgeschlagen.", lockStatus );
            }
            finally
            {
              getLocker().releaseUiUpdateLock();
            }
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
            lnkLock.setToolTipText( String.format( "Sperre Projekt \"%s\" zum Editieren.", getHandler().getName() ) );
            lnkLock.setImage( IMG_LORE_LOCK );

            lnkLock.addHyperlinkListener( new HyperlinkAdapter()
            {
              /**
               * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
               */
              @Override
              public void linkActivated( final HyperlinkEvent e )
              {
                try
                {
                  getLocker().acquireUiUpdateLock();

                  final IStatus lockStatus = ProjectDataBaseController.acquireProjectLock( getHandler() );

                  final Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
                  if( shell == null )
                    return;

                  if( !shell.isDisposed() )
                    ErrorDialog.openError( shell, "Fehler", "Sperren des Projektes zum Editieren ist fehlgeschlagen.", lockStatus );
                }
                finally
                {
                  getLocker().releaseUiUpdateLock();
                }
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

  protected void getUpdateLink( final Composite body, final FormToolkit toolkit )
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
          try
          {
            getLocker().acquireUiUpdateLock();

            // delete local project
            final DeleteLocalProjectHandler delete = new DeleteLocalProjectHandler( getHandler().getProject() );
            final IStatus status = ProgressUtilities.busyCursorWhile( delete );

            final Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
            if( !shell.isDisposed() )
              ErrorDialog.openError( shell, "Löschen fehlgeschlagen", "Fehler beim Löschen des Projektes", status );

            // import head version of project from server
            try
            {
              // bad hack - to determine which project was newly created
              final Map<ProjectTemplate, KalypsoProjectBean> mapping = new HashMap<ProjectTemplate, KalypsoProjectBean>();

              final KalypsoProjectBean bean = getHandler().getBean();
              final ProjectTemplate template = new ProjectTemplate( String.format( "%s - Version %d", bean.getName(), bean.getProjectVersion() ), bean.getUnixName(), bean.getDescription(), null, bean.getUrl() );
              mapping.put( template, bean );

              RemoteProjectHelper.importRemoteProject( new ProjectTemplate[] { template }, mapping );
            }
            catch( final MalformedURLException e1 )
            {
              KalypsoProjectDatabaseClient.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e1 ) );
            }
          }
          finally
          {
            getLocker().releaseUiUpdateLock();
          }
        }
      } );

    }
    catch( final CoreException e )
    {
      KalypsoProjectDatabaseClient.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
  }

  protected void getImportLink( final Composite body, final FormToolkit toolkit )
  {
    final ImageHyperlink lnkImport = toolkit.createImageHyperlink( body, SWT.NONE );
    lnkImport.setToolTipText( String.format( "Importiere Projekt: %s", getHandler().getName() ) );

    if( getHandler().isRemote() && ProjectDatabaseServerUtils.isServerOnline() )
    {
      lnkImport.setImage( IMG_IMPORT_REMOTE );
      lnkImport.addHyperlinkListener( new HyperlinkAdapter()
      {
        /**
         * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
         */
        @Override
        public void linkActivated( final HyperlinkEvent e )
        {
          try
          {
            getLocker().acquireUiUpdateLock();

            /* sort beans */
            final KalypsoProjectBean bean = getHandler().getBean();
            final ProjectTemplate template = new ProjectTemplate( String.format( "%s - Version %d", bean.getName(), bean.getProjectVersion() ), bean.getUnixName(), bean.getDescription(), null, bean.getUrl() );

            final Map<ProjectTemplate, KalypsoProjectBean> mapping = new HashMap<ProjectTemplate, KalypsoProjectBean>();
            mapping.put( template, bean );

            RemoteProjectHelper.importRemoteProject( new ProjectTemplate[] { template }, mapping );
          }
          catch( final MalformedURLException e1 )
          {
            KalypsoProjectDatabaseClient.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e1 ) );
          }
          finally
          {
            getLocker().releaseUiUpdateLock();
          }

        }
      } );

    }
    else
    {
      lnkImport.setImage( IMG_IMPORT_REMOTE_DISABLED );
      lnkImport.setEnabled( false );
    }
  }

  protected void getCommitLink( final Composite body, final FormToolkit toolkit )
  {
    final ImageHyperlink lnkCommit = toolkit.createImageHyperlink( body, SWT.NONE );
    lnkCommit.setToolTipText( String.format( "Übtrage Projekt \"%s\" in Modelldaten-Basis.", m_handler.getName() ) );

    if( ProjectDatabaseServerUtils.isServerOnline() )
    {
      lnkCommit.setImage( IMG_LOCAL_COMMIT );

      lnkCommit.addHyperlinkListener( new HyperlinkAdapter()
      {
        /**
         * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
         */
        @Override
        public void linkActivated( final HyperlinkEvent e )
        {
          try
          {
            getLocker().acquireUiUpdateLock();

            final IStatus status = ProjectDataBaseController.createRemoteProject( getHandler() );

            final Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
            if( !shell.isDisposed() )
              ErrorDialog.openError( shell, "Fehler", "Übertragen des Projektes ist fehlgeschlagen.", status );
          }
          finally
          {
            getLocker().releaseUiUpdateLock();
          }
        }
      } );
    }
    else
    {
      lnkCommit.setImage( IMG_LOCAL_COMMIT_DISABLED );
      lnkCommit.setEnabled( false );
    }
  }

}
