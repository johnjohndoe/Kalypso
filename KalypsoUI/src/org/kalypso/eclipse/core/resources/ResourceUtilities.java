package org.kalypso.eclipse.core.resources;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;

/**
 * @author belger
 */
public class ResourceUtilities
{
  private ResourceUtilities()
  {
    // do not instantiate
  }

  public static URL createURL( final IResource resource ) throws MalformedURLException
  {
    return new URL( "platform:/resource/" + resource.getFullPath().toString() );
  }

  /** Findet alle Projekte einer Selektion von Resourcen */
  public static IProject[] findeProjectsFromSelection( final ISelection selection )
  {
    // gleiche Projekte sollen nur einen Eintrag gebens
    final Collection projects = new HashSet();
    if( selection != null && !selection.isEmpty() && selection instanceof IStructuredSelection )
    {
      final IStructuredSelection ssel = (IStructuredSelection)selection;
      for( final Iterator iter = ssel.iterator(); iter.hasNext(); )
      {
        final Object resource = iter.next();
        if( resource instanceof IResource )
          projects.add( ((IResource)resource).getProject() );
        else if( resource instanceof IAdaptable )
        {
          final IResource res = (IResource)((IAdaptable)resource).getAdapter( IResource.class );
          if( res != null )
            projects.add( res.getProject() );
        }
      }
    }

    return (IProject[])projects.toArray( new IProject[projects.size()] );
  }
}
