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

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.kalypso.contribs.java.util.Arrays;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.sort.SplitSort;

/**
 * 
 * TODO: insert type comment here
 * 
 * @author doemming
 */
public class FeatureSelectionManager implements IFeatureSelectionManager
{
  private FeatureList m_selection = new SplitSort( null, null );

  /**
   * @see org.kalypsodeegree_impl.model.feature.selection.IFeatureSelectionManager#addToSelection(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean addToSelection( Feature feature )
  {
    if( !m_selection.contains( feature ) )
    {
      m_selection.add( feature );
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
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.selection.IFeatureSelectionManager#addToSelection(org.kalypsodeegree.model.feature.Feature[])
   */
  public void addToSelection( Feature[] feature )
  {
    m_selection.addAll( Arrays.asList( feature ) );
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.selection.IFeatureSelectionManager#setSelection(org.kalypsodeegree.model.feature.Feature[])
   */
  public void setSelection( Feature[] feature )
  {
    m_selection = FeatureFactory.createFeatureList( null, null, Arrays.asList( feature ) );
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.selection.IFeatureSelectionManager#setSelection(java.util.List)
   */
  public void setSelection( List listOfFeatures )
  {
    m_selection = FeatureFactory.createFeatureList( null, null, listOfFeatures );
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.selection.IFeatureSelectionManager#removeFromSelection(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean removeFromSelection( Feature feature )
  {
    return m_selection.remove( feature );
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.selection.IFeatureSelectionManager#getSelection()
   */
  public Feature[] getSelection()
  {
    final List result = m_selection.queryAll( null );
    return (Feature[])result.toArray( new Feature[result.size()] );
  }

  /**
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
}
