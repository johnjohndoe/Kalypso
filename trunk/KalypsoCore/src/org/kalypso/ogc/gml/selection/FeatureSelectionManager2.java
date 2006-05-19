/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.ogc.gml.selection;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author belger
 */
public class FeatureSelectionManager2 implements IFeatureSelectionManager
{
  /** feature -> easyWrapper */
  private final Map<Feature, EasyFeatureWrapper> m_map = new HashMap<Feature, EasyFeatureWrapper>();

  private List<IFeatureSelectionListener> m_listener = new ArrayList<IFeatureSelectionListener>( 5 );

  /**
   * @see org.kalypso.ogc.gml.selection.IFeatureSelectionManager#changeSelection(org.kalypsodeegree.model.feature.Feature[],
   *      org.kalypso.ogc.gml.selection.EasyFeatureWrapper[])
   */
  public void changeSelection( final Feature[] featuresToRemove, final EasyFeatureWrapper[] featuresToAdd )
  {
    final Set<Feature> oldState = new HashSet<Feature>( m_map.keySet() );

    for( int i = 0; i < featuresToRemove.length; i++ )
    {
      final Feature f = featuresToRemove[i];
      m_map.remove( f );
    }

    for( int i = 0; i < featuresToAdd.length; i++ )
    {
      final EasyFeatureWrapper wrapper = featuresToAdd[i];
      m_map.put( wrapper.getFeature(), wrapper );
    }

    if( !m_map.keySet().equals( oldState ) )
      fireSelectionChanged();
  }

  private void fireSelectionChanged( )
  {
    final IFeatureSelectionListener[] ls = m_listener.toArray( new IFeatureSelectionListener[m_listener.size()] );
    for( int i = 0; i < ls.length; i++ )
    {
      final IFeatureSelectionListener listener = ls[i];
      listener.selectionChanged( this );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.selection.IFeatureSelectionManager#clear()
   */
  public void clear( )
  {
    m_map.clear();

    fireSelectionChanged();
  }

  /**
   * @see org.kalypso.ogc.gml.selection.IFeatureSelectionManager#addSelectionListener(org.kalypso.ogc.gml.selection.IFeatureSelectionListener)
   */
  public void addSelectionListener( final IFeatureSelectionListener l )
  {
    m_listener.add( l );
  }

  /**
   * @see org.kalypso.ogc.gml.selection.IFeatureSelectionManager#removeSelectionListener(org.kalypso.ogc.gml.selection.IFeatureSelectionListener)
   */
  public void removeSelectionListener( final IFeatureSelectionListener l )
  {
    m_listener.remove( l );
  }

  /**
   * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getWorkspace(org.kalypsodeegree.model.feature.Feature)
   */
  public CommandableWorkspace getWorkspace( final Feature feature )
  {
    final EasyFeatureWrapper wrapper = getWrapper( feature );
    if( wrapper == null )
      return null;

    return wrapper.getWorkspace();
  }

  private EasyFeatureWrapper getWrapper( final Feature feature )
  {
    return m_map.get( feature );
  }

  /**
   * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getParentFeature(org.kalypsodeegree.model.feature.Feature)
   */
  public Feature getParentFeature( final Feature feature )
  {
    final EasyFeatureWrapper wrapper = getWrapper( feature );
    if( wrapper == null )
      return null;

    return wrapper.getParentFeature();
  }

  /**
   * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getParentFeatureProperty(org.kalypsodeegree.model.feature.Feature)
   */
  public IRelationType getParentFeatureProperty( final Feature feature )
  {
    final EasyFeatureWrapper wrapper = getWrapper( feature );
    if( wrapper == null )
      return null;

    return wrapper.getParentFeatureProperty();
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredSelection#getFirstElement()
   */
  public Object getFirstElement( )
  {
    if( !m_map.isEmpty() )
      return m_map.keySet().iterator().next();
    return new Object();
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredSelection#iterator()
   */
  public Iterator iterator( )
  {
    return m_map.keySet().iterator();
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredSelection#size()
   */
  public int size( )
  {
    return m_map.size();
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredSelection#toArray()
   */
  public Object[] toArray( )
  {
    return m_map.keySet().toArray();
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredSelection#toList()
   */
  public List<Feature> toList( )
  {
    return new ArrayList<Feature>( m_map.keySet() );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelection#isEmpty()
   */
  public boolean isEmpty( )
  {
    return m_map.isEmpty();
  }

  /**
   * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getAllFeatures()
   */
  public EasyFeatureWrapper[] getAllFeatures( )
  {
    return m_map.values().toArray( new EasyFeatureWrapper[m_map.size()] );
  }

  /**
   * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getSelectionManager()
   */
  public IFeatureSelectionManager getSelectionManager( )
  {
    return this;
  }

  /**
   * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getFocusedFeature()
   */
  public Feature getFocusedFeature( )
  {
    // The feature manager doesn't support the focused feature
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getFocusedProperty()
   */
  public IPropertyType getFocusedProperty( )
  {
    // The feature manager doesn't support the focused feature
    return null;
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return m_map.toString();
  }
}
