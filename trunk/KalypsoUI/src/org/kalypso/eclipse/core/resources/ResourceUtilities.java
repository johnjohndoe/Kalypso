package org.kalypso.eclipse.core.resources;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;

import org.eclipse.core.internal.resources.PlatformURLResourceConnection;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

/**
 * @author belger
 */
public class ResourceUtilities
{
  private ResourceUtilities( )
  {
    // do not instantiate
  }

  /**
   * Creates an URL given a resource. Uses the eclipse scheme defined in
   * PlatformURLResourceConnection.RESOURCE_URL_STRING.
   * 
   * @see PlatformURLResourceConnection#RESOURCE_URL_STRING
   * 
   * @param resource
   * @return platform URL
   * @throws MalformedURLException
   */
  public static URL createURL( final IResource resource )
      throws MalformedURLException
  {
    String strUrl = createURLSpec( resource.getFullPath() );

    if( resource instanceof IContainer )
      strUrl += '/';

    return new URL( strUrl );
  }

  /**
   * Creates the string representation of an URL given an IPath.
   * 
   * @param path
   * @return platform URL
   */
  public static String createURLSpec( final IPath path )
  {
    return PlatformURLResourceConnection.RESOURCE_URL_STRING + path.toString();
  }

  public static IFile findFileFromURL( final URL u )
  {
    final IPath path = findPathFromURL( u );
    if( path == null )
      return null;

    final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
    return root.getFile( path );
  }

  public static IPath findPathFromURL( final URL u )
  {
    final String utostring = u.toString();
    final String urlpath;
    int ix = utostring.indexOf( '?' );
    if( ix != -1 )
      urlpath = utostring.substring( 0, ix );
    else
      urlpath = utostring;

    if( urlpath != null
        && urlpath
            .startsWith( PlatformURLResourceConnection.RESOURCE_URL_STRING ) )
    {
      final String path = urlpath
          .substring( PlatformURLResourceConnection.RESOURCE_URL_STRING
              .length() - 1 );

      final Path path2 = new Path( path );
      return path2;
    }

    return null;
  }

  /** 
   * Findet alle Projekte einer Selektion von Resourcen
   * @param selection
   * @return list of projects (not null)
   */
  public static IProject[] findeProjectsFromSelection(
      final ISelection selection )
  {
    // gleiche Projekte sollen nur einen Eintrag gebens
    final Collection projects = new HashSet();
    if( selection != null && !selection.isEmpty()
        && selection instanceof IStructuredSelection )
    {
      final IStructuredSelection ssel = (IStructuredSelection) selection;
      for( final Iterator iter = ssel.iterator(); iter.hasNext(); )
      {
        final Object resource = iter.next();
        if( resource instanceof IResource )
          projects.add( ((IResource) resource).getProject() );
        else if( resource instanceof IAdaptable )
        {
          final IResource res = (IResource) ((IAdaptable) resource)
              .getAdapter( IResource.class );
          if( res != null )
            projects.add( res.getProject() );
        }
      }
    }

    return (IProject[]) projects.toArray( new IProject[projects.size()] );
  }

  /**
   * TODO does this work? seems not... Note from Marc: this only works
   * when the navigator has an active selection
   * 
   * Returns the currently selected project from the navigator.
   * @return list of selected projects
   */
  public static IProject[] getSelectedProjects()
  {
    final IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
    final ISelection selection = window.getSelectionService().getSelection(
        IPageLayout.ID_RES_NAV );

    final IProject[] projects = findeProjectsFromSelection( selection );

    return projects;
  }
  
  public static File makeFileFromPath( final IPath resource )
  {
    final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
    final IPath rootLocation = root.getLocation();
    final File rootFile = rootLocation.toFile();
    return new File( rootFile, resource.toString() );
  }

  public static IProject findProjectFromURL( final URL baseURL )
  {
    final IPath path = findPathFromURL( baseURL );
    if( path == null || path.isRoot() || path.segmentCount() < 1
        || !path.isAbsolute() )
      return null;

    final String projectName = path.segment( 0 );
    return ResourcesPlugin.getWorkspace().getRoot().getProject( projectName );
  }
}