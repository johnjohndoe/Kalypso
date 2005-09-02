/*--------------- Kalypso-Header ----------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 University of Technology Hamburg-Harburg (TUHH)
 Institute of River and Coastal Engineering
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
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

 Contact:

 E-Mail:
 g.belger@bjoernsen.de
 m.schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ------------------------------------------------------------------------*/
package org.kalypso.contribs.eclipse.jface.viewers;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.util.SafeRunnable;
import org.eclipse.jface.viewers.IPostSelectionProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;

/**
 * Default implementation of {@link org.eclipse.jface.viewers.IPostSelectionProvider}.
 * 
 * <p>
 * Implements support for the listeners and the getter / setter.
 * 
 * Subclasses may reimplement the getter/setter.
 * </p>
 * 
 * @author belger
 */
public class SelectionProviderAdapter implements IPostSelectionProvider
{
  private final List m_listeners = new ArrayList();

  private final List m_postListeners = new ArrayList();

  private ISelection m_selection = null;

  public final void addSelectionChangedListener( final ISelectionChangedListener listener )
  {
    m_listeners.add( listener );
  }

  public final void removeSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_listeners.remove( listener );
  }

  public void setSelection( final ISelection selection )
  {
    m_selection = selection;
    fireSelectionChanged();
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
   */
  public ISelection getSelection()
  {
    if( m_selection == null )
      m_selection = StructuredSelection.EMPTY;
    return m_selection;
  }

  /**
   * @see org.eclipse.jface.viewers.IPostSelectionProvider#addPostSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void addPostSelectionChangedListener( final ISelectionChangedListener listener )
  {
    m_postListeners.add( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.IPostSelectionProvider#removePostSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void removePostSelectionChangedListener( final ISelectionChangedListener listener )
  {
    m_postListeners.remove( listener );
  }

  private final void fireSelectionChanged()
  {
    final ISelectionChangedListener[] listenersArray = (ISelectionChangedListener[])m_listeners
        .toArray( new ISelectionChangedListener[m_listeners.size()] );

    fireSelectionChanged( new SelectionChangedEvent( this, getSelection() ), listenersArray );
  }

  public final void firePostSelectionChanged()
  {
    final ISelectionChangedListener[] listenersArray = (ISelectionChangedListener[])m_postListeners
        .toArray( new ISelectionChangedListener[m_postListeners.size()] );

    fireSelectionChanged( new SelectionChangedEvent( this, getSelection() ), listenersArray );
  }

  private void fireSelectionChanged( final SelectionChangedEvent e, final ISelectionChangedListener[] listenersArray )
  {
    for( int i = 0; i < listenersArray.length; i++ )
    {
      final ISelectionChangedListener l = listenersArray[i];
      final SafeRunnable safeRunnable = new SafeRunnable()
      {
        public void run()
        {
          l.selectionChanged( e );
        }
      };

      Platform.run( safeRunnable );
    }
  }

}