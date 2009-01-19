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
package org.kalypso.project.database.client.ui.project.database.internal;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.kalypso.afgui.extension.IKalypsoProjectOpenAction;
import org.kalypso.afgui.extension.IProjectDatabaseUiLocker;
import org.kalypso.contribs.eclipse.core.resources.ProjectTemplate;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.project.database.client.KalypsoProjectDatabaseClient;
import org.kalypso.project.database.client.core.ProjectDataBaseController;
import org.kalypso.project.database.client.core.model.interfaces.ITranscendenceProject;
import org.kalypso.project.database.client.core.project.workspace.DeleteLocalProjectHandler;
import org.kalypso.project.database.client.core.utils.ProjectDatabaseServerUtils;
import org.kalypso.project.database.client.ui.project.wizard.commit.WizardCommitProject;
import org.kalypso.project.database.common.nature.IRemoteProjectPreferences;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

/**
 * @author Dirk Kuch
 */
public class TranscendenceProjectRowBuilder extends AbstractLocalProjectRowBuilder
{
  public static Image IMG_LORE_LOCK_DISABLED = new Image( null, AbstractProjectRowBuilder.class.getResourceAsStream( "icons/lore_lock_disabled.gif" ) );

  public static Image IMG_LORE_COMMIT_AND_UNLOCK_DISABLED = new Image( null, AbstractProjectRowBuilder.class.getResourceAsStream( "icons/lore_commit_unlock_disabled.gif" ) );

  public static Image IMG_LORE_UPDATEABLE = new Image( null, AbstractProjectRowBuilder.class.getResourceAsStream( "icons/lore_update.gif" ) );

  public static Image IMG_LORE_COMMIT_AND_UNLOCK = new Image( null, AbstractProjectRowBuilder.class.getResourceAsStream( "icons/lore_commit_unlock.gif" ) );

  public static Image IMG_LORE_PROJECT = new Image( null, AbstractProjectRowBuilder.class.getResourceAsStream( "icons/lore.gif" ) );

  public static Image IMG_LORE_PROJECT_DISABLED = new Image( null, AbstractProjectRowBuilder.class.getResourceAsStream( "icons/lore_disabled.gif" ) );

  public static Image IMG_LORE_LOCKED = new Image( null, AbstractProjectRowBuilder.class.getResourceAsStream( "icons/lore_locked.gif" ) );

  public static Image IMG_LORE_OTHER_LOCK = new Image( null, AbstractProjectRowBuilder.class.getResourceAsStream( "icons/lore_other_locked.gif" ) );

  public static Image IMG_LORE_LOCK = new Image( null, AbstractProjectRowBuilder.class.getResourceAsStream( "icons/lore_lock.gif" ) );

  protected final ITranscendenceProject m_transcendence;

  public TranscendenceProjectRowBuilder( final ITranscendenceProject transcendence, final IKalypsoProjectOpenAction action, final IProjectDatabaseUiLocker locker )
  {
    super( transcendence, action, locker );
    m_transcendence = transcendence;
  }

  /**
   * @see org.kalypso.afgui.extension.IProjectRowBuilder#render(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  @Override
  public void render( final Composite parent, final FormToolkit toolkit )
  {
    try
    {
      /* settings */
      final IRemoteProjectPreferences preferences = getLocalProject().getRemotePreferences();

      final Composite body = toolkit.createComposite( parent );
      body.setLayout( new GridLayout( 6, false ) );
      body.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

      final ImageHyperlink lnk = toolkit.createImageHyperlink( body, SWT.NONE );
      lnk.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

      if( preferences.isLocked() )
      {
        lnk.setImage( IMG_LORE_LOCKED );
      }
      else if( !ProjectDatabaseServerUtils.isServerOnline() )
      {
        lnk.setImage( IMG_LORE_PROJECT_DISABLED );
      }
      else if( m_transcendence.getBean().isProjectLockedForEditing() )
      {
        lnk.setImage( IMG_LORE_OTHER_LOCK );
      }
      else
      {
        lnk.setImage( IMG_LORE_PROJECT );
      }

      lnk.setToolTipText( String.format( "Öffne Projekt: %s", getLocalProject().getName() ) );
      lnk.setText( getLocalProject().getName() );

      addProjectOpenListener( lnk );

      // info
      RemoteProjectHelper.getRemoteInfoLink( m_transcendence, body, toolkit, getLocker() );

      // lock project
      createLockHyperlink( body, toolkit );

      getSpacer( body, toolkit );

      // export
      getExportLink( body, toolkit );

      /* delete */
      getDeleteLink( body, toolkit );
    }
    catch( final CoreException e1 )
    {
      KalypsoProjectDatabaseClient.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e1 ) );
    }

  }

  protected void createLockHyperlink( final Composite body, final FormToolkit toolkit ) throws CoreException
  {
    final IRemoteProjectPreferences preferences = getLocalProject().getRemotePreferences();

    if( preferences.isLocked() )
    {
      final ImageHyperlink lnkLock = toolkit.createImageHyperlink( body, SWT.NONE );
      lnkLock.setToolTipText( String.format( "Übertrage Projekt \"%s\" in Modelldaten-Basis und gebe Projekt vom Editieren frei.", getLocalProject().getName() ) );

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
              final WizardCommitProject wizard = new WizardCommitProject( m_transcendence );
              final WizardDialog2 dialog = new WizardDialog2( null, wizard );
              dialog.open();

              /* release */
              final IStatus lockStatus = ProjectDataBaseController.releaseProjectLock( m_transcendence );
              if( !shell.isDisposed() )
              {
                ErrorDialog.openError( shell, "Fehler", "Freigeben des Projektes ist fehlgeschlagen.", lockStatus );
              }
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
        if( m_transcendence.getBean().isProjectLockedForEditing() )
        {
          if( ProjectDatabaseServerUtils.isUpdateAvailable( m_transcendence ) )
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
          if( ProjectDatabaseServerUtils.isUpdateAvailable( m_transcendence ) )
          {
            getUpdateLink( body, toolkit );
          }
          else
          {
            final ImageHyperlink lnkLock = toolkit.createImageHyperlink( body, SWT.NONE );
            lnkLock.setToolTipText( String.format( "Sperre Projekt \"%s\" zum Editieren.", m_transcendence.getName() ) );
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

                  final IStatus lockStatus = ProjectDataBaseController.acquireProjectLock( m_transcendence );

                  final Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
                  if( shell == null )
                  {
                    return;
                  }

                  if( !shell.isDisposed() )
                  {
                    ErrorDialog.openError( shell, "Fehler", "Sperren des Projektes zum Editieren ist fehlgeschlagen.", lockStatus );
                  }
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
      final Integer remoteVersion = m_transcendence.getBean().getProjectVersion();
      final Integer localVersion = m_transcendence.getRemotePreferences().getVersion();

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
            final DeleteLocalProjectHandler delete = new DeleteLocalProjectHandler( m_transcendence.getProject() );
            final IStatus status = ProgressUtilities.busyCursorWhile( delete );

            final Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
            if( !shell.isDisposed() )
            {
              ErrorDialog.openError( shell, "Löschen fehlgeschlagen", "Fehler beim Löschen des Projektes", status );
            }

            // import head version of project from server
            try
            {
              // bad hack - to determine which project was newly created
              final Map<ProjectTemplate, KalypsoProjectBean> mapping = new HashMap<ProjectTemplate, KalypsoProjectBean>();

              final KalypsoProjectBean bean = m_transcendence.getBean();
              final ProjectTemplate template = new ProjectTemplate( String.format( "%s - Version %d", bean.getName(), bean.getProjectVersion() ), bean.getUnixName(), bean.getDescription(), null, bean.getUrl() );
              mapping.put( template, bean );

              RemoteProjectHelper.importRemoteProject( new ProjectTemplate[] { template }, mapping );
            }
            catch( final Exception e1 )
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
}
