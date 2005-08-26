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

import java.util.List;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.kalypsodeegree.model.feature.Feature;

/**
 * 
 * TODO: insert type comment here
 * 
 * @author doemming
 */
public class InverseFeatureSelectionManager implements IFeatureSelectionManager
{

  private final IFeatureSelectionManager m_selectionManager;

  public InverseFeatureSelectionManager( IFeatureSelectionManager selectionManager )
  {
    m_selectionManager = selectionManager;
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.selection.IFeatureSelectionManager#isSelected(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean isSelected( Feature feature )
  {
    return !m_selectionManager.isSelected( feature );
  }

  //  public void addModellListener( ModellEventListener listener )
  //  {
  //// m_selectionManager.addModellListener( listener );
  //  }

  public boolean addToSelection( Feature feature )
  {
    return m_selectionManager.addToSelection( feature );
  }

  public void addToSelection( Feature[] feature )
  {
    m_selectionManager.addToSelection( feature );
  }

  public void addToSelection( List listOfFeatures )
  {
    m_selectionManager.addToSelection( listOfFeatures );
  }

  public void clear()
  {
    m_selectionManager.clear();
  }

  //
  //  public void fireModellEvent( ModellEvent event )
  //  {
  //    m_selectionManager.fireModellEvent( event );
  //  }

  public List getSelectedIds()
  {
    return m_selectionManager.getSelectedIds();
  }

  public Feature[] getFeatureSelection()
  {
    return m_selectionManager.getFeatureSelection();
  }

  public IStructuredSelection getStructuredSelection()
  {
    return m_selectionManager.getStructuredSelection();
  }

  public boolean removeFromSelection( Feature feature )
  {
    return m_selectionManager.removeFromSelection( feature );
  }

  //  public void removeModellListener( ModellEventListener listener )
  //  {
  //    m_selectionManager.removeModellListener( listener );
  //  }

  public void setSelection( Object eventSource, Feature[] feature )
  {
    m_selectionManager.setSelection( eventSource, feature );
  }

  public void setSelection( Object eventSource, List listOfFeatures )
  {
    m_selectionManager.setSelection( eventSource, listOfFeatures );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void addSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_selectionManager.addSelectionChangedListener( listener );

  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
   */
  public ISelection getSelection()
  {
    return m_selectionManager.getSelection();
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#removeSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void removeSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_selectionManager.removeSelectionChangedListener( listener );

  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( ISelection selection )
  {
    m_selectionManager.setSelection( selection );

  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.selection.IFeatureSelectionManager#hasListeners()
   */
  public boolean hasListeners()
  {
    return m_selectionManager.hasListeners();
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.selection.IFeatureSelectionManager#getListenerList()
   */
  public ISelectionChangedListener[] getListenerList()
  {
    return m_selectionManager.getListenerList();
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.selection.IFeatureSelectionManager#moveListenersTo(org.kalypsodeegree_impl.model.feature.selection.IFeatureSelectionManager)
   */
  public void moveListenersTo( IFeatureSelectionManager source )
  {
    m_selectionManager.moveListenersTo( source );
  }
}
