/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypsodeegree_impl.model.feature.selection;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.util.ListenerList;
import org.eclipse.jface.util.SafeRunnable;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.kalypso.contribs.eclipse.jface.viewers.KalypsoSelectionChangedEvent;
import org.kalypso.contribs.java.util.Arrays;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.event.IEventSourceProvider;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.sort.SplitSort;

/**
 *  
 * 
 * @author doemming/k�pferle
 */
public class FeatureSelectionManager implements IFeatureSelectionManager, ISelectionProvider
{
  private FeatureList m_selection = new SplitSort( null, null );

  private ListenerList m_listeners = new ListenerList();

  /**
   * @see org.kalypsodeegree_impl.model.feature.selection.IFeatureSelectionManager#addToSelection(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean addToSelection( Feature feature )
  {
    if( !m_selection.contains( feature ) )
    {
      m_selection.add( feature );
      final KalypsoSelectionChangedEvent event = new KalypsoSelectionChangedEvent( this, null, getSelection() );
      fireSelectionChanged( event );
      return true;
    }
    return false;
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.selection.IFeatureSelectionManager#addToSelection(java.util.List)
   */
  public void addToSelection( List listOfFeatures )
  {
    m_selection.addAll( listOfFeatures );
    final KalypsoSelectionChangedEvent event = new KalypsoSelectionChangedEvent( this, null, getSelection() );
    fireSelectionChanged( event );
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.selection.IFeatureSelectionManager#addToSelection(org.kalypsodeegree.model.feature.Feature[])
   */
  public void addToSelection( Feature[] feature )
  {
    m_selection.addAll( Arrays.asList( feature ) );
    final KalypsoSelectionChangedEvent event = new KalypsoSelectionChangedEvent( this, null, getSelection() );
    fireSelectionChanged( event );
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.selection.IFeatureSelectionManager#setSelection(Object, Feature[])
   */
  public void setSelection( Object eventSource, Feature[] feature )
  {
    m_selection = FeatureFactory.createFeatureList( null, null, Arrays.asList( feature ) );
    final KalypsoSelectionChangedEvent event = new KalypsoSelectionChangedEvent( this, eventSource, getSelection() );
    fireSelectionChanged( event );
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.selection.IFeatureSelectionManager#setSelection(Object, List)
   */
  public void setSelection( Object eventSource, List listOfFeatures )
  {
    m_selection = FeatureFactory.createFeatureList( null, null, listOfFeatures );
    final KalypsoSelectionChangedEvent event = new KalypsoSelectionChangedEvent( this, eventSource, getSelection() );
    fireSelectionChanged( event );
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.selection.IFeatureSelectionManager#removeFromSelection(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean removeFromSelection( Feature feature )
  {
    return m_selection.remove( feature );
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.selection.IFeatureSelectionManager#getFeatureSelection()
   */
  public Feature[] getFeatureSelection()
  {
    final List result = m_selection.queryAll( null );
    return (Feature[])result.toArray( new Feature[result.size()] );
  }

  /**
   * Clears this current selection.
   * 
   * @see org.kalypsodeegree_impl.model.feature.selection.IFeatureSelectionManager#clear()
   */
  public void clear()
  {
    m_selection = new SplitSort( null, null );
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.selection.IFeatureSelectionManager#getStructuredSelection()
   */
  public IStructuredSelection getStructuredSelection()
  {
    return new StructuredSelection( m_selection );
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.selection.IFeatureSelectionManager#isSelected(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean isSelected( Feature feature )
  {
    return m_selection.contains( feature );
  }

  /**
   * Returns a list with all feature id's in this selection manager
   * 
   * @return list of feature id's
   * @see org.kalypsodeegree_impl.model.feature.selection.IFeatureSelectionManager#getSelectedIds()
   */
  public List getSelectedIds()
  {
    final List result = new ArrayList();
    final Iterator iterator = m_selection.iterator();
    while( iterator.hasNext() )
      result.add( ( (Feature)iterator.next() ).getId() );
    return result;
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void addSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_listeners.add( listener );

  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
   */
  public ISelection getSelection()
  {
    IStructuredSelection ss = null;
    if( m_selection != null )
      ss = new StructuredSelection( m_selection.toArray() );
    else
      ss = new StructuredSelection( StructuredSelection.EMPTY );
    return ss;
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#removeSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void removeSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_listeners.remove( listener );
  }

  /**
   * This method looks into the selection and adds all features in to this selection managers
   * 
   * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
   */

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( ISelection selection )
  {
    if( selection instanceof IStructuredSelection && !selection.isEmpty() )
    {
      ArrayList list = new ArrayList();
      Object[] objects = ( (IStructuredSelection)selection ).toArray();
      for( int i = 0; i < objects.length; i++ )
      {
        Object o = objects[i];
        if( o instanceof Feature )
          list.add( o );
      }
      m_selection = FeatureFactory.createFeatureList( null, null, list );
      KalypsoSelectionChangedEvent event = null;
      if( selection instanceof IEventSourceProvider )
      {
        event = new KalypsoSelectionChangedEvent( this, ( (IEventSourceProvider)selection ).getEventSource(),
            getSelection() );
      }
      fireSelectionChanged( event );
    }
  }

  private final void fireSelectionChanged( final SelectionChangedEvent event )
  {
    Object[] listeners = m_listeners.getListeners();
    for( int i = 0; i < listeners.length; i++ )
    {
      final Object listener = listeners[i];
      if( listener instanceof ISelectionChangedListener )
      {
        final ISelectionChangedListener l = (ISelectionChangedListener)listener;
        Platform.run( new SafeRunnable()
        {
          public void run()
          {
            try
            {
              l.selectionChanged( event );
            }
            catch( Exception e )
            {
              // it is assumed that listener is not a valid reference anymore and was not removed
              // from the listener list
              m_listeners.remove( l );
            }
          }
        } );
      }
    }
  }

  public ISelectionChangedListener[] getListenerList()
  {
    return (ISelectionChangedListener[])Arrays.castArray( m_listeners.getListeners(),
        new ISelectionChangedListener[m_listeners.size()] );
  }

  public boolean hasListeners()
  {
    return !m_listeners.isEmpty();
  }

  /**
   * This method moves the listeners from the source to this selection manager.
   * 
   * @param source
   *          the manager from where to move the listeners to this manager
   * 
   * @see org.kalypsodeegree_impl.model.feature.selection.IFeatureSelectionManager#moveListenersTo(org.kalypsodeegree_impl.model.feature.selection.IFeatureSelectionManager)
   */
  public void moveListenersTo( IFeatureSelectionManager source )
  {
    ISelectionChangedListener[] listenerList = source.getListenerList();
    for( int i = 0; i < listenerList.length; i++ )
    {
      ISelectionChangedListener listener = listenerList[i];
      m_listeners.add( listener );
    }
  }
}
