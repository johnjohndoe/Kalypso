/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.nature.prognose;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.ui.nature.CalcCaseCollector;
import org.kalypso.ui.nature.ModelNature;

/**
 * TreeContentProvider to show calc cases in form of a tree. Show s all subdirectories of the input element (which must
 * be a {@link org.eclipse.core.resources.IFolder}which are calc cases.
 * 
 * @author belger
 */
public class CalcCaseTreeContentProvider implements ITreeContentProvider
{
  private final CalcCaseCollector m_collector = new CalcCaseCollector();

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
   */
  public Object[] getChildren( final Object parentElement )
  {
    return getCalcCasesForElement( parentElement );
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
   */
  public Object getParent( final Object element )
  {
    if( element instanceof IResource )
      return ( (IResource)element ).getParent();

    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
   */
  public boolean hasChildren( final Object element )
  {
    final Object[] children = getChildren( element );
    return children != null && children.length > 0;
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( final Object inputElement )
  {
    final Object[] calcCasesForElement = getCalcCasesForElement( inputElement );
    
    if( calcCasesForElement == null || calcCasesForElement.length == 0 )
      return new Object[] { "<keine Rechenvarianten vorhanden>" };
    
    return calcCasesForElement;
  }

  /**
   * If the element is a folder, return its direct subfolders which are calc cases.
   */
  private Object[] getCalcCasesForElement( final Object element )
  {
    if( element instanceof IContainer )
    {
      try
      {
        final IContainer container = (IContainer)element;
        if( container instanceof IFolder && ModelNature.isCalcCalseFolder( (IFolder)container ) )
          return null;
        
        final IResource[] members = container.members( 0 );
        final List children = new ArrayList( members.length );
        for( int i = 0; i < members.length; i++ )
        {
          final IResource resource = members[i];
          if( resource.getType() == IResource.FOLDER )
          {
            final IFolder childFolder = (IFolder)resource;
            m_collector.clear();
            childFolder.accept( m_collector, IResource.DEPTH_INFINITE, false );
            if( m_collector.getCalcCases().length > 0 )
              children.add( childFolder );
          }
        }
        
        return children.toArray();
      }
      catch( final CoreException e )
      {
        e.printStackTrace();
        // ignore, return null
      }
    }

    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose()
  {}

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {}
}
