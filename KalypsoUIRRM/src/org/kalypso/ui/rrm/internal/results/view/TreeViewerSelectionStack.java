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
package org.kalypso.ui.rrm.internal.results.view;

import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;

/**
 * additional selected items of a tree view. items was added by double clicking of tree viewer elements
 * 
 * @author Dirk Kuch
 */
public class TreeViewerSelectionStack
{
  Set<Object> m_stack = new LinkedHashSet<>();

  public ISelection getSelection( final IStructuredSelection selection )
  {
    final Set<Object> localStack = new LinkedHashSet<>();
    final Iterator< ? > itr = selection.iterator();
    while( itr.hasNext() )
    {
      localStack.add( itr.next() );
    }

    localStack.addAll( m_stack );

    return new StructuredSelection( localStack.toArray() );
  }

  public void add( final IStructuredSelection selection )
  {
    final Iterator< ? > itr = selection.iterator();
    while( itr.hasNext() )
    {
      m_stack.add( itr.next() );
    }
  }

  public void reset( )
  {
    m_stack.clear();
  }

}
