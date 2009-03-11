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
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.project.database.client.KalypsoProjectDatabaseClient;
import org.kalypso.project.database.client.core.ProjectDataBaseController;
import org.kalypso.project.database.client.core.model.interfaces.ILocalProject;
import org.kalypso.project.database.client.core.utils.ProjectDatabaseServerUtils;
import org.kalypso.project.database.client.i18n.Messages;
import org.kalypso.project.database.common.nature.IRemoteProjectPreferences;

/**
 * @author Dirk Kuch
 */
public class LocalProjectRowBuilder extends AbstractLocalProjectRowBuilder
{
  public static Image IMG_LOCAL_COMMIT = new Image( null, AbstractLocalProjectRowBuilder.class.getResourceAsStream( "icons/local_commit.gif" ) ); //$NON-NLS-1$

  public static Image IMG_LOCAL_COMMIT_DISABLED = new Image( null, AbstractProjectRowBuilder.class.getResourceAsStream( "icons/local_commit_disabled.gif" ) ); //$NON-NLS-1$

  public LocalProjectRowBuilder( final ILocalProject local, final IKalypsoProjectOpenAction action, final IProjectDatabaseUiLocker locker )
  {
    super( local, action, locker );
  }

  /**
   * @see org.kalypso.afgui.extension.IProjectRowBuilder#render(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  @Override
  public void render( final Composite parent, final FormToolkit toolkit )
  {
    final ILocalProject project = getLocalProject();

    final Composite body = toolkit.createComposite( parent );

    if( isCommitableProject() )
    {
      body.setLayout( new GridLayout( 7, false ) );
    }
    else
    {
      body.setLayout( new GridLayout( 6, false ) );
    }
    body.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    final ImageHyperlink lnk = toolkit.createImageHyperlink( body, SWT.NONE );
    lnk.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    lnk.setImage( IMG_LOCAL_PROJECT );
    lnk.setToolTipText( String.format( Messages.getString("org.kalypso.project.database.client.ui.project.database.internal.LocalProjectRowBuilder.2"), project.getName() ) ); //$NON-NLS-1$
    lnk.setText( project.getName() );

    addProjectOpenListener( lnk );

    /* info */
    getLocalInfoLink( body, toolkit );

    /* commit */
    if( isCommitableProject() )
    {
      getCommitLink( body, toolkit );
    }

    /* export */
    getExportLink( body, toolkit );

    // spacer
    getSpacer( body, toolkit );
    getSpacer( body, toolkit );

    /* delete */
    getDeleteLink( body, toolkit );

  }

  private boolean isCommitableProject( )
  {
    try
    {
      final ILocalProject project = getLocalProject();
      final IRemoteProjectPreferences preferences = project.getRemotePreferences();

      if( preferences != null )
      {
        return true;
      }
    }
    catch( final CoreException e )
    {
      KalypsoProjectDatabaseClient.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }

    return false;
  }

  protected void getCommitLink( final Composite body, final FormToolkit toolkit )
  {
    final ImageHyperlink lnkCommit = toolkit.createImageHyperlink( body, SWT.NONE );
    lnkCommit.setToolTipText( String.format( Messages.getString("org.kalypso.project.database.client.ui.project.database.internal.LocalProjectRowBuilder.3"), getLocalProject().getName() ) ); //$NON-NLS-1$

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

            final IStatus status = ProjectDataBaseController.createRemoteProject( getLocalProject() );

            final Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
            if( shell != null && !shell.isDisposed() )
            {
              ErrorDialog.openError( shell, Messages.getString("org.kalypso.project.database.client.ui.project.database.internal.LocalProjectRowBuilder.4"), Messages.getString("org.kalypso.project.database.client.ui.project.database.internal.LocalProjectRowBuilder.5"), status ); //$NON-NLS-1$ //$NON-NLS-2$
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
      lnkCommit.setImage( IMG_LOCAL_COMMIT_DISABLED );
      lnkCommit.setEnabled( false );
    }
  }

}
