/***********************************************************************************************************************
 * Copyright (c) 2000, 2003 IBM Corporation and others. All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0 which accompanies this distribution, and is
 * available at http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors: IBM Corporation - initial API and implementation
 **********************************************************************************************************************/
package org.kalypso.contribs.eclipse.ui.dialogs;

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

public class ResourceContentProvider implements ITreeContentProvider
{

  private boolean m_showClosedProjects = true;

  private String[] m_allowedResourceExtensions;

  /*
   * abgeleitet von ContainerContentProvider @author N. Peiler
   */
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
    if( m_allowedResourceExtensions == null )
      return true;
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
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
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