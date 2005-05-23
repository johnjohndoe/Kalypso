package org.kaylpso.ui.dialog;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

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

public class ResourceContentProvider implements ITreeContentProvider
{

  private boolean m_showClosedProjects = true;

  private String[] m_allowedResourceExtensions;

  public ResourceContentProvider( String[] allowedResourceExtensions )
  {
    super();
    m_allowedResourceExtensions = allowedResourceExtensions;
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
   */
  public Object[] getChildren( Object element )
  {
    if( element instanceof IWorkspace )
    {
      // check if closed projects should be shown
      IProject[] allProjects = ( (IWorkspace)element ).getRoot().getProjects();
      if( m_showClosedProjects )
        return allProjects;

      ArrayList accessibleProjects = new ArrayList();
      for( int i = 0; i < allProjects.length; i++ )
      {
        if( allProjects[i].isOpen() )
        {
          accessibleProjects.add( allProjects[i] );
        }
      }
      return accessibleProjects.toArray();
    }
    else if( element instanceof IContainer )
    {
      IContainer container = (IContainer)element;
      if( container.isAccessible() )
      {
        try
        {
          List children = new ArrayList();
          IResource[] members = container.members();
          for( int i = 0; i < members.length; i++ )
          {
            if( members[i].getType() == IResource.FILE )
            {
              if( checkExtension( ( (IFile)members[i] ).getFileExtension() ) )
                children.add( members[i] );
            }
            else
            {
              children.add( members[i] );
            }
          }
          return children.toArray();
        }
        catch( CoreException e )
        {
          // this should never happen because we call #isAccessible before
          // invoking #members
        }
      }
    }
    return new Object[0];
  }

  private boolean checkExtension( String extension )
  {
    if( extension == null )
      return false;
    boolean returnValue = false;
    for( int i = 0; i < m_allowedResourceExtensions.length; i++ )
    {
      if( extension.equals( m_allowedResourceExtensions[i] ) )
      {
        returnValue = true;
      }
    }
    return returnValue;
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
   */
  public Object getParent( Object element )
  {
    if( element instanceof IResource )
      return ( (IResource)element ).getParent();
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
   */
  public boolean hasChildren( Object element )
  {
    return getChildren( element ).length > 0;
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( Object inputElement )
  {
    return getChildren( inputElement );
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose()
  {
  //do nothing
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer,
   *      java.lang.Object, java.lang.Object)
   */
  public void inputChanged( Viewer viewer, Object oldInput, Object newInput )
  {
  // do nothing
  }

  public void showClosedProjects( boolean show )
  {
    m_showClosedProjects = show;
  }

}