/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import java.io.IOException;
import java.io.StringWriter;
import java.net.URL;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.apache.commons.lang.CharEncoding;
import org.apache.commons.lang.StringUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.IWorkbenchSite;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.ide.undo.CreateProjectOperation;
import org.kalypso.afgui.wizards.NewProjectData;
import org.kalypso.afgui.wizards.UnpackProjectTemplateOperation;
import org.kalypso.contribs.eclipse.EclipsePlatformContributionsExtensions;
import org.kalypso.contribs.eclipse.core.resources.ProjectTemplate;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.resources.StringStorage;
import org.kalypso.contribs.eclipse.ui.editorinput.StorageEditorInput;
import org.kalypso.core.jaxb.TemplateUtilities;
import org.kalypso.core.util.pool.IPoolableObjectType;
import org.kalypso.core.util.pool.PoolableObjectType;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.ui.IWspmTuhhUIConstants;
import org.kalypso.model.wspm.tuhh.ui.extension.KalypsoWspmTuhhModule;
import org.kalypso.model.wspm.tuhh.ui.light.WspmGmvViewPart;
import org.kalypso.ogc.gml.PoolGmlWorkspaceProvider;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.template.gistreeview.Gistreeview;
import org.kalypso.template.types.LayerType;

/**
 * Encapsulates the data project of a pdb connection.
 * 
 * @author Gernot Belger
 */
public class PdbWspmProject
{
  private static final String WSPM_PROJECT_NAME = "PDBWspmData"; //$NON-NLS-1$

  private PoolGmlWorkspaceProvider m_provider;

  private IProject m_project;

  private final IWorkbenchSite m_site;

  public PdbWspmProject( final IWorkbenchSite site )
  {
    m_site = site;
  }

  public void dispose( )
  {
    if( m_provider != null )
      m_provider.dispose();
    m_provider = null;
  }

  public void loadData( final IProgressMonitor monitor ) throws CoreException
  {
    Assert.isTrue( m_provider == null );

    monitor.beginTask( "Initializing Cross Section Database WSPM Project", 100 );

    m_project = ensureProject( new SubProgressMonitor( monitor, 33 ) );

    /* Access wspm data */
    final URL projectLocation = ResourceUtilities.createQuietURL( m_project );
    final IPoolableObjectType key = new PoolableObjectType( "gml", IWspmTuhhConstants.FILE_MODELL_GML, projectLocation ); //$NON-NLS-1$
    m_provider = new PoolGmlWorkspaceProvider( key );
    final TuhhWspmProject wspmProject = waitForworkspaceLoad( new SubProgressMonitor( monitor, 33 ) );
    if( wspmProject == null )
    {
      final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, "Failed to load wspm data" );
      throw new CoreException( status );
    }

    /* set data to views */
    initPerspective( new SubProgressMonitor( monitor, 34 ) );

    monitor.done();
  }

  public IProject ensureProject( final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      monitor.beginTask( "Accessing WSPM Project Data", 100 );

      final IWorkspace workspace = ResourcesPlugin.getWorkspace();

      final IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject( WSPM_PROJECT_NAME );
      if( project.isOpen() )
        return project;

      if( project.exists() )
      {
        project.open( new SubProgressMonitor( monitor, 100 ) );
        return project;
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
      final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, "Failed to create PDB data project", e );
      throw new CoreException( status );
    }
    finally
    {
      monitor.done();
    }
  }

  private TuhhWspmProject waitForworkspaceLoad( final IProgressMonitor monitor )
  {
    monitor.beginTask( "Loading project data", IProgressMonitor.UNKNOWN );

    m_provider.startLoading();

    try
    {
      while( true )
      {
        // TODO: check if this is always safe
        final IStatus status = m_provider.getStatus();
        if( PoolGmlWorkspaceProvider.LOADING_STATUS != status )
        {
          final CommandableWorkspace workspace = m_provider.getWorkspace();
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

  private void initPerspective( final IProgressMonitor monitor )
  {
    monitor.beginTask( "Initializing WSPM Views", 100 );

    initGmvView();
    monitor.worked( 50 );

    // TODO: open and update map in map view

    monitor.done();
  }

  private void initGmvView( )
  {
    final String inputContent = createGmvInput();

    final StringStorage storage = new StringStorage( inputContent, null );
    storage.setName( "Local Data" );

    final IStorageEditorInput input = new StorageEditorInput( storage );
    setGmvInput( input );
  }

  private String createGmvInput( )
  {
    try
    {
      final IFile modelFile = getModelFile();
      final URL modelURL = ResourceUtilities.createQuietURL( modelFile );

      final Gistreeview gistreeview = TemplateUtilities.OF_GISTREEVIEW.createGistreeview();
      final LayerType layerType = TemplateUtilities.OF_TEMPLATE_TYPES.createLayerType();

      layerType.setFeatureXPath( "id( 'root' )/wspm:waterBodyMember" ); // root feature
      layerType.setHref( modelURL.toExternalForm() );
      layerType.setLinktype( "gml" ); //$NON-NLS-1$

      gistreeview.setInput( layerType );

      final Marshaller marshaller = TemplateUtilities.createGistreeviewMarshaller( CharEncoding.UTF_8 );
      final StringWriter sw = new StringWriter();
      marshaller.marshal( gistreeview, sw );
      sw.close();
      return sw.toString();
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();
      return null;
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      return null;
    }
  }

  private void setGmvInput( final IStorageEditorInput input )
  {
    final FindViewRunnable<WspmGmvViewPart> runnable = new FindViewRunnable<WspmGmvViewPart>( WspmGmvViewPart.ID, m_site );
    final WspmGmvViewPart view = runnable.execute();
    if( view == null )
      return;

    view.setInput( input );
  }

  // /**
  // * Forces a reload of the underlying data.<br/>
  // * Any changes will be lost.
  // */
  // public void reload( )
  // {
  // final GmlTreeView viewer = getViewer();
  // viewer.reload();
  // }

  // public static IFile ensureMapFile( )
// {
// final IFile mapFile = getMapFile();
// if( mapFile.exists() )
// return mapFile;
//
// try
// {
// final File mapJavaFile = mapFile.getLocation().toFile();
//      final URL mapData = getClass().class.getResource( "mapTemplate.gmt" ); //$NON-NLS-1$
// FileUtils.copyURLToFile( mapData, mapJavaFile );
//
// mapFile.refreshLocal( IResource.DEPTH_ONE, new NullProgressMonitor() );
// }
// catch( final IOException e )
// {
// e.printStackTrace();
// }
// catch( final CoreException e )
// {
// e.printStackTrace();
// }
//
// return mapFile;
// }


// protected static IProject getProject( )
// {
// final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
// return root.getProject( WSPM_PROJECT_NAME );
// }

  private IFile getModelFile( )
  {
    return m_project.getFile( IWspmTuhhConstants.FILE_MODELL_GML );
  }

// public static IFile getMapFile( )
// {
// final IProject project = getProject();
//    return project.getFile( "PDB.gmt" ); //$NON-NLS-1$
// }

  public TuhhWspmProject getWspmProject( )
  {
    if( m_provider == null )
      return null;

    final CommandableWorkspace workspace = m_provider.getWorkspace();
    if( workspace == null )
      return null;

    return (TuhhWspmProject) workspace.getRootFeature();
  }

  public void saveProject( final IProgressMonitor monitor ) throws CoreException
  {
    if( m_provider == null )
      return;

    m_provider.save( monitor );
  }

// public static TuhhWspmProject createModel( )
// {
// try
// {
// ensureProject();
//
// final IFile modelFile = getModelFile();
//
// final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( modelFile );
// return (TuhhWspmProject) workspace.getRootFeature();
// }
// catch( final Exception e )
// {
// e.printStackTrace();
// return null;
// }
// }

}
