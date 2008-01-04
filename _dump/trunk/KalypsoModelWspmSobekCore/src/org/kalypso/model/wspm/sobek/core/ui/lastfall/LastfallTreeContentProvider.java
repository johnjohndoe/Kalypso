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
package org.kalypso.model.wspm.sobek.core.ui.lastfall;

import java.util.HashSet;
import java.util.Set;

import org.apache.commons.lang.NotImplementedException;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNode;
import org.kalypso.model.wspm.sobek.core.interfaces.ILastfall;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;

/**
 * @author kuch
 */
public class LastfallTreeContentProvider implements ITreeContentProvider
{
  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose( )
  {
    // atm nothing to do...
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
   */
  public Object[] getChildren( final Object parentElement )
  {
    if( parentElement instanceof ILastfall )
      return getLastfallChildren( (ILastfall) parentElement );

    throw new IllegalStateException( "Only Lastfalls can have children." );
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( final Object inputElement )
  {
    if( inputElement instanceof ILastfall[] )
      return (ILastfall[]) inputElement;

    throw new NotImplementedException();
  }

  /**
   * @return all boundary nodes, because, all boundary nodes have to be paramized inherit a lastfall
   */
  private Object[] getLastfallChildren( final ILastfall lastfall )
  {
    final Set<IBoundaryNode> myNodes = new HashSet<IBoundaryNode>();

    final INode[] nodes = lastfall.getModelMember().getNodeMembers();
    for( final INode node : nodes )
      if( node instanceof IBoundaryNode )
        myNodes.add( (IBoundaryNode) node );

    return myNodes.toArray();
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
   */
  public Object getParent( final Object element )
  {
    throw new NotImplementedException();
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
   */
  public boolean hasChildren( final Object element )
  {
    if( element instanceof ILastfall )
    {
      final Object[] children = getLastfallChildren( (ILastfall) element );
      if( children.length == 0 )
        return false;

      return true;
    }

    return false;
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {
    // atm nothing to do...
  }
}
