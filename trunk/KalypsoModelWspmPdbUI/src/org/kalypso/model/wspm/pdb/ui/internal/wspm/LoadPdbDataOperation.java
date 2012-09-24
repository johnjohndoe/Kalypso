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
package org.kalypso.model.wspm.pdb.ui.internal.wspm;

import java.net.URL;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.ide.undo.CreateProjectOperation;
import org.kalypso.afgui.wizards.NewProjectData;
import org.kalypso.afgui.wizards.UnpackProjectTemplateOperation;
import org.kalypso.contribs.eclipse.EclipsePlatformContributionsExtensions;
import org.kalypso.contribs.eclipse.core.resources.ProjectTemplate;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.swt.awt.SWT_AWT_Utilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.core.util.pool.IPoolableObjectType;
import org.kalypso.core.util.pool.PoolableObjectType;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.ui.IWspmTuhhUIConstants;
import org.kalypso.model.wspm.tuhh.ui.extension.KalypsoWspmTuhhModule;
import org.kalypso.ogc.gml.PoolFeaturesProvider;
import org.kalypso.ogc.gml.PoolGmlWorkspaceProvider;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;

/**
 * @author Gernot Belger
 */
public class LoadPdbDataOperation implements ICoreRunnableWithProgress
{
  private static final String STR_ACCESSING_WSPM_PROJECT_DATA = Messages.getString( "LoadPdbDataOperation.0" ); //$NON-NLS-1$

  private final PdbWspmProject m_pdbProject;

  public LoadPdbDataOperation( final PdbWspmProject project )
  {
    m_pdbProject = project;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( Messages.getString( "LoadPdbDataOperation.1" ), 100 ); //$NON-NLS-1$

    monitor.subTask( Messages.getString( "LoadPdbDataOperation.2" ) ); //$NON-NLS-1$
    final IProject project = ensureProject( new SubProgressMonitor( monitor, 45 ) );
    if( project == null )
      return new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, Messages.getString( "LoadPdbDataOperation.3" ) ); //$NON-NLS-1$

    /* Access wspm data */
    monitor.subTask( Messages.getString( "LoadPdbDataOperation.4" ) ); //$NON-NLS-1$
    final URL projectLocation = ResourceUtilities.createQuietURL( project );
    final IPoolableObjectType key = new PoolableObjectType( "gml", IWspmTuhhConstants.FILE_MODELL_GML, projectLocation ); //$NON-NLS-1$
    final PoolFeaturesProvider provider = new PoolFeaturesProvider( key, StringUtils.EMPTY );
    final TuhhWspmProject wspmProject = waitForworkspaceLoad( provider, new SubProgressMonitor( monitor, 50 ) );
    if( wspmProject == null )
    {
      final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, Messages.getString( "LoadPdbDataOperation.5" ) ); //$NON-NLS-1$
      throw new CoreException( status );
    }

    m_pdbProject.setData( provider, project );

    /* set data to views */
    monitor.subTask( Messages.getString( "LoadPdbDataOperation.6" ) ); //$NON-NLS-1$
    initPerspective();
    monitor.worked( 5 );

    monitor.done();

    return Status.OK_STATUS;
  }

  private IProject ensureProject( final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      monitor.beginTask( STR_ACCESSING_WSPM_PROJECT_DATA, 100 );

      final IWorkspace workspace = ResourcesPlugin.getWorkspace();

      final IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject( PdbWspmProject.WSPM_PROJECT_NAME );
      try
      {
        if( checkProjectExists( project, monitor ) )
          return project;
      }
      catch( final CoreException e )
      {
        if( !askForDeletion( project, e.getStatus() ) )
          return null;
        // Fall through, project was deleted, we try to create it again.
      }

      final IProjectDescription description = workspace.newProjectDescription( project.getName() );
      // description.setLocationURI(location)
      final CreateProjectOperation op = new CreateProjectOperation( description, StringUtils.EMPTY );
      // WorkspaceUndoUtil.getUIInfoAdapter(getShell())
      op.execute( new SubProgressMonitor( monitor, 50 ), null );

      final ProjectTemplate[] templates = EclipsePlatformContributionsExtensions.getProjectTemplates( IWspmTuhhUIConstants.WSPM_TUHH_PROJECT_TEMPLATE_CATEGORY );
      final ProjectTemplate template = templates[0]; // we know there is exactly one...

      final String moduleID = KalypsoWspmTuhhModule.ID;
      final NewProjectData data = new NewProjectData( null, template, project, moduleID );
      final WorkspaceModifyOperation operation = new UnpackProjectTemplateOperation( data );
      operation.run( new SubProgressMonitor( monitor, 50 ) );
      return project;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, Messages.getString( "LoadPdbDataOperation.7" ), e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
    finally
    {
      monitor.done();
    }
  }

  private boolean askForDeletion( final IProject project, final IStatus status )
  {
    final String message = String.format( Messages.getString( "LoadPdbDataOperation.8" ), status.getMessage() ); //$NON-NLS-1$

    final boolean confirm = SWT_AWT_Utilities.showSwtMessageBoxConfirm( STR_ACCESSING_WSPM_PROJECT_DATA, message );
    if( !confirm )
      return false;

    try
    {
      project.delete( true, new NullProgressMonitor() );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();

      final Shell shell = SWT_AWT_Utilities.findActiveShell();

      final IStatus error = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, Messages.getString( "LoadPdbDataOperation.9" ), e ); //$NON-NLS-1$

      final StatusDialog dialog = new StatusDialog( shell, error, STR_ACCESSING_WSPM_PROJECT_DATA );
      SWT_AWT_Utilities.openSwtWindow( dialog );

      return false;
    }

    return true;
  }

  private boolean checkProjectExists( final IProject project, final IProgressMonitor monitor ) throws CoreException
  {
    if( project.isOpen() )
    {
      project.refreshLocal( IResource.DEPTH_INFINITE, new SubProgressMonitor( monitor, 100 ) );
      return checkProjectContent( project );
    }

    if( project.exists() )
    {
      project.open( new SubProgressMonitor( monitor, 100 ) );
      return checkProjectContent( project );
    }

    // Project does not exist
    return false;
  }

  private boolean checkProjectContent( final IProject project )
  {
    final IFile modelGML = project.getFile( IWspmTuhhConstants.FILE_MODELL_GML );
    if( !modelGML.exists() )
      return false;

    // TODO: check for other data

    return true;
  }

  private TuhhWspmProject waitForworkspaceLoad( final PoolFeaturesProvider provider, final IProgressMonitor monitor )
  {
    monitor.beginTask( Messages.getString( "LoadPdbDataOperation.10" ), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$

    provider.startLoading();

    try
    {
      while( true )
      {
        final IStatus status = provider.getStatus();
        if( PoolGmlWorkspaceProvider.LOADING_STATUS != status )
        {
          final CommandableWorkspace workspace = provider.getWorkspace();
          if( workspace == null )
            return null;

          return (TuhhWspmProject) workspace.getRootFeature();
        }

        Thread.sleep( 100 );
      }
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
      return null;
    }
  }

  private void initPerspective( )
  {
    final InitPerspectiveJob job = new InitPerspectiveJob( m_pdbProject );
    job.setSystem( true );
    job.schedule();
  }
}
