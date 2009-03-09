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
package org.kalypso.project.database.client.ui.project.database.internal;

import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
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
import org.kalypso.project.database.client.core.model.interfaces.ILocalProject;
import org.kalypso.project.database.client.core.model.interfaces.ITranscendenceProject;
import org.kalypso.project.database.client.core.model.local.LocalWorkspaceModel;
import org.kalypso.project.database.client.core.project.workspace.DeleteLocalProjectHandler;
import org.kalypso.project.database.client.core.utils.ProjectDatabaseServerUtils;
import org.kalypso.project.database.client.i18n.Messages;
import org.kalypso.project.database.client.ui.project.wizard.commit.WizardCommitProject;
import org.kalypso.project.database.common.nature.IRemoteProjectPreferences;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

/**
 * @author Dirk Kuch
 */
public class TranscendenceProjectRowBuilder extends AbstractLocalProjectRowBuilder
{
  public static Image IMG_LORE_LOCK_DISABLED = new Image( null, AbstractProjectRowBuilder.class.getResourceAsStream( "icons/lore_lock_disabled.gif" ) ); //$NON-NLS-1$

  public static Image IMG_LORE_RELEASE_LOCK = new Image( null, AbstractProjectRowBuilder.class.getResourceAsStream( "icons/lore_release_lock.gif" ) ); //$NON-NLS-1$

  public static Image IMG_LORE_COMMIT = new Image( null, AbstractProjectRowBuilder.class.getResourceAsStream( "icons/local_commit.gif" ) ); //$NON-NLS-1$

  public static Image IMG_LORE_COMMIT_AND_UNLOCK_DISABLED = new Image( null, AbstractProjectRowBuilder.class.getResourceAsStream( "icons/lore_commit_unlock_disabled.gif" ) ); //$NON-NLS-1$

  public static Image IMG_LORE_UPDATEABLE = new Image( null, AbstractProjectRowBuilder.class.getResourceAsStream( "icons/lore_update.gif" ) ); //$NON-NLS-1$

  public static Image IMG_LORE_COMMIT_AND_UNLOCK = new Image( null, AbstractProjectRowBuilder.class.getResourceAsStream( "icons/lore_commit_unlock.gif" ) ); //$NON-NLS-1$

  public static Image IMG_LORE_PROJECT = new Image( null, AbstractProjectRowBuilder.class.getResourceAsStream( "icons/lore.gif" ) ); //$NON-NLS-1$

  public static Image IMG_LORE_PROJECT_DISABLED = new Image( null, AbstractProjectRowBuilder.class.getResourceAsStream( "icons/lore_disabled.gif" ) ); //$NON-NLS-1$

  public static Image IMG_LORE_LOCKED = new Image( null, AbstractProjectRowBuilder.class.getResourceAsStream( "icons/lore_locked.gif" ) ); //$NON-NLS-1$

  public static Image IMG_LORE_OTHER_LOCK = new Image( null, AbstractProjectRowBuilder.class.getResourceAsStream( "icons/lore_other_locked.gif" ) ); //$NON-NLS-1$

  public static Image IMG_LORE_LOCK = new Image( null, AbstractProjectRowBuilder.class.getResourceAsStream( "icons/lore_lock.gif" ) ); //$NON-NLS-1$

  public static Image IMG_CHANGE_VERSION = new Image( null, AbstractProjectRowBuilder.class.getResourceAsStream( "icons/transcendence_change_version.gif" ) ); //$NON-NLS-1$

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
      body.setLayout( new GridLayout( 7, false ) );
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

      lnk.setToolTipText( String.format( Messages.getString("org.kalypso.project.database.client.ui.project.database.internal.TranscendenceProjectRowBuilder.12"), getLocalProject().getName() ) ); //$NON-NLS-1$
      lnk.setText( getLocalProject().getName() );

      addProjectOpenListener( lnk );

      // info
      RemoteProjectHelper.getRemoteInfoLink( m_transcendence, body, toolkit, getLocker() );

      // lock project
      getCommitHyperlink( body, toolkit );

      getChangeVersion( body, toolkit );

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

  private void getChangeVersion( final Composite body, final FormToolkit toolkit ) throws CoreException
  {
    final IRemoteProjectPreferences preferences = getLocalProject().getRemotePreferences();
    if( preferences.isLocked() )
    {
      final ImageHyperlink lnkChangeVersion = toolkit.createImageHyperlink( body, SWT.NONE );
      lnkChangeVersion.setImage( IMG_CHANGE_VERSION );
      lnkChangeVersion.setToolTipText( Messages.getString("org.kalypso.project.database.client.ui.project.database.internal.TranscendenceProjectRowBuilder.13") ); //$NON-NLS-1$

      if( ProjectDatabaseServerUtils.isServerOnline() )
      {
        lnkChangeVersion.addHyperlinkListener( new HyperlinkAdapter()
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

              /* delete old bean */
              if( MessageDialog.openConfirm( lnkChangeVersion.getShell(), Messages.getString("org.kalypso.project.database.client.ui.project.database.internal.TranscendenceProjectRowBuilder.14"), String.format( Messages.getString("org.kalypso.project.database.client.ui.project.database.internal.TranscendenceProjectRowBuilder.15"), getLocalProject().getName(), getLocalProject().getName() ) ) ) //$NON-NLS-1$ //$NON-NLS-2$
              {
                final String editTicket = preferences.getEditTicket();
                int version = preferences.getVersion();

                final DeleteLocalProjectHandler delete = new DeleteLocalProjectHandler( getLocalProject().getProject() );
                final IStatus status = ProgressUtilities.busyCursorWhile( delete );

                final Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
                if( shell != null && !shell.isDisposed() )
                {
                  ErrorDialog.openError( shell, Messages.getString("org.kalypso.project.database.client.ui.project.database.internal.TranscendenceProjectRowBuilder.16"), Messages.getString("org.kalypso.project.database.client.ui.project.database.internal.TranscendenceProjectRowBuilder.17"), status ); //$NON-NLS-1$ //$NON-NLS-2$
                }

                /* download bean */
                final Map<ProjectTemplate, KalypsoProjectBean> mapping = new HashMap<ProjectTemplate, KalypsoProjectBean>();
                final Set<ProjectTemplate> templates = new LinkedHashSet<ProjectTemplate>();

                final KalypsoProjectBean bean = m_transcendence.getBean();
                final ProjectTemplate head = new ProjectTemplate( String.format( "%s - Version %d", bean.getName(), bean.getProjectVersion() ), bean.getUnixName(), bean.getDescription(), null, bean.getUrl() ); //$NON-NLS-1$
                templates.add( head );
                mapping.put( head, bean );

                final KalypsoProjectBean[] children = bean.getChildren();
                for( int i = children.length - 1; i >= 0; i-- )
                {
                  final KalypsoProjectBean child = children[i];
                  final ProjectTemplate childTemplate = new ProjectTemplate( String.format( "%s - Version %d", child.getName(), child.getProjectVersion() ), child.getUnixName(), child.getDescription(), null, child.getUrl() ); //$NON-NLS-1$
                  templates.add( childTemplate );
                  mapping.put( childTemplate, child );
                }

                /* reset edit ticket and update version of project! */
                final IProject project = RemoteProjectHelper.importRemoteProject( templates.toArray( new ProjectTemplate[] {} ), mapping );
                final LocalWorkspaceModel model = getLocalProject().getLocalWorkspaceModel();
                final ILocalProject local = model.getProject( project );
                final IRemoteProjectPreferences p = local.getRemotePreferences();
                p.setEditTicket( editTicket );
                p.setVersion( ++version );
              }
            }
            catch( final Exception e1 )
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

    }
    else
    {
      getSpacer( body, toolkit );
    }

  }

  private void getCommitHyperlink( final Composite body, final FormToolkit toolkit ) throws CoreException
  {
    final IRemoteProjectPreferences preferences = getLocalProject().getRemotePreferences();

    if( preferences.isLocked() )
    {
      /* open project lock */
      if( ProjectDatabaseServerUtils.isServerOnline() )
      {
        getRelaseLockHyperlink( body, toolkit );
      }
      else
      {
        final ImageHyperlink lnkLock = toolkit.createImageHyperlink( body, SWT.NONE );
        lnkLock.setImage( IMG_LORE_COMMIT_AND_UNLOCK_DISABLED );
        lnkLock.setEnabled( false );
      }
    }
    else if( getLocalProject().isModified() && !preferences.getChangesCommited() )
    {
      /* commit project */
      if( ProjectDatabaseServerUtils.isServerOnline() )
      {
        getCommitProjectHyperlink( body, toolkit );
      }
      else
      {
        final ImageHyperlink lnkLock = toolkit.createImageHyperlink( body, SWT.NONE );
        lnkLock.setImage( IMG_LORE_COMMIT_AND_UNLOCK_DISABLED );
        lnkLock.setEnabled( false );
      }
    }
    else
    {
      if( ProjectDatabaseServerUtils.isServerOnline() )
      {
        /* require project lock */
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
          getCreateLockHyperlink( body, toolkit );
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

  private void getCommitProjectHyperlink( final Composite body, final FormToolkit toolkit )
  {
    if( ProjectDatabaseServerUtils.isUpdateAvailable( m_transcendence ) )
    {
      getUpdateLink( body, toolkit );
    }
    else
    {
      final ImageHyperlink lnkLock = toolkit.createImageHyperlink( body, SWT.NONE );
      lnkLock.setToolTipText( String.format( Messages.getString("org.kalypso.project.database.client.ui.project.database.internal.TranscendenceProjectRowBuilder.20"), getLocalProject().getName() ) ); //$NON-NLS-1$
      lnkLock.setImage( IMG_LORE_COMMIT );

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

            final WizardCommitProject wizard = new WizardCommitProject( m_transcendence );
            final WizardDialog2 dialog = new WizardDialog2( null, wizard );
            dialog.open();

            try
            {
              final IRemoteProjectPreferences preferences = m_transcendence.getRemotePreferences();
              preferences.setChangesCommited( true );

            }
            catch( final CoreException e1 )
            {
              KalypsoProjectDatabaseClient.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e1 ) );
            }

            final Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
            if( shell != null && !shell.isDisposed() && Window.OK != dialog.getReturnCode() )
            {
              ErrorDialog.openError( shell, Messages.getString("org.kalypso.project.database.client.ui.project.database.internal.TranscendenceProjectRowBuilder.21"), Messages.getString("org.kalypso.project.database.client.ui.project.database.internal.TranscendenceProjectRowBuilder.22"), StatusUtilities.createErrorStatus( "" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
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

  private void getCreateLockHyperlink( final Composite body, final FormToolkit toolkit )
  {
    if( ProjectDatabaseServerUtils.isUpdateAvailable( m_transcendence ) )
    {
      getUpdateLink( body, toolkit );
    }
    else
    {
      final ImageHyperlink lnkLock = toolkit.createImageHyperlink( body, SWT.NONE );
      lnkLock.setToolTipText( String.format( Messages.getString("org.kalypso.project.database.client.ui.project.database.internal.TranscendenceProjectRowBuilder.24"), m_transcendence.getName() ) ); //$NON-NLS-1$
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
              ErrorDialog.openError( shell, Messages.getString("org.kalypso.project.database.client.ui.project.database.internal.TranscendenceProjectRowBuilder.25"), Messages.getString("org.kalypso.project.database.client.ui.project.database.internal.TranscendenceProjectRowBuilder.26"), lockStatus ); //$NON-NLS-1$ //$NON-NLS-2$
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

  private void getRelaseLockHyperlink( final Composite body, final FormToolkit toolkit )
  {

    final ImageHyperlink lnkLock = toolkit.createImageHyperlink( body, SWT.NONE );
    lnkLock.setToolTipText( String.format( Messages.getString("org.kalypso.project.database.client.ui.project.database.internal.TranscendenceProjectRowBuilder.27"), getLocalProject().getName() ) ); //$NON-NLS-1$

    lnkLock.setImage( IMG_LORE_RELEASE_LOCK );

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
          final Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
          getLocker().acquireUiUpdateLock();

          final IStatus lockStatus = ProjectDataBaseController.releaseProjectLock( m_transcendence );
          if( !shell.isDisposed() )
          {
            ErrorDialog.openError( shell, Messages.getString("org.kalypso.project.database.client.ui.project.database.internal.TranscendenceProjectRowBuilder.28"), Messages.getString("org.kalypso.project.database.client.ui.project.database.internal.TranscendenceProjectRowBuilder.29"), lockStatus ); //$NON-NLS-1$ //$NON-NLS-2$
          }

          try
          {
            final IRemoteProjectPreferences preferences = getLocalProject().getRemotePreferences();
            preferences.setChangesCommited( false );
          }
          catch( final CoreException e1 )
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

  protected void getUpdateLink( final Composite body, final FormToolkit toolkit )
  {
    try
    {
      final Integer remoteVersion = m_transcendence.getBean().getProjectVersion();
      final Integer localVersion = m_transcendence.getRemotePreferences().getVersion();

      final ImageHyperlink lnkUpdate = toolkit.createImageHyperlink( body, SWT.NONE );
      lnkUpdate.setImage( IMG_LORE_UPDATEABLE );
      lnkUpdate.setToolTipText( String.format( Messages.getString("org.kalypso.project.database.client.ui.project.database.internal.TranscendenceProjectRowBuilder.30"), localVersion, remoteVersion ) ); //$NON-NLS-1$

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
              ErrorDialog.openError( shell, Messages.getString("org.kalypso.project.database.client.ui.project.database.internal.TranscendenceProjectRowBuilder.31"), Messages.getString("org.kalypso.project.database.client.ui.project.database.internal.TranscendenceProjectRowBuilder.32"), status ); //$NON-NLS-1$ //$NON-NLS-2$
            }

            // import head version of project from server
            try
            {
              // bad hack - to determine which project was newly created
              final Map<ProjectTemplate, KalypsoProjectBean> mapping = new HashMap<ProjectTemplate, KalypsoProjectBean>();

              final KalypsoProjectBean bean = m_transcendence.getBean();
              final ProjectTemplate template = new ProjectTemplate( String.format( "%s - Version %d", bean.getName(), bean.getProjectVersion() ), bean.getUnixName(), bean.getDescription(), null, bean.getUrl() ); //$NON-NLS-1$
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
