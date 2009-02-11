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
package org.kalypso.ui.editor.actions;

import java.util.Comparator;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * A comparator on features. The compared property values must implement {@link Comparable}.
 * 
 * @author Gernot Belger
 */
public class FeatureComparator implements Comparator<Object>
{
  private final IPropertyType m_pt;

  private final GMLWorkspace m_workspace;

  /**
   * The qname of the property, which will be used for comparison.
   */
  private final QName m_qname;

  public FeatureComparator( Feature parentFeature, IPropertyType pt )
  {
    m_workspace = parentFeature.getWorkspace();
    m_pt = pt;
    m_qname = null;
  }

  /**
   * The constructor.
   * 
   * @param parentFeature
   *            The parent of the list.
   * @param qname
   *            The qname of the property, which should be sorted.
   */
  public FeatureComparator( Feature parentFeature, QName qname )
  {
    m_workspace = parentFeature.getWorkspace();
    m_pt = null;
    m_qname = qname;
  }

  /**
   * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
   */
  @SuppressWarnings("unchecked") //$NON-NLS-1$
  public int compare( final Object o1, final Object o2 )
  {
    final Feature f1 = FeatureHelper.getFeature( m_workspace, o1 );
    final Feature f2 = FeatureHelper.getFeature( m_workspace, o2 );

    Object property1 = null;
    Object property2 = null;
    if( m_pt != null )
    {
      property1 = f1.getProperty( m_pt );
      property2 = f2.getProperty( m_pt );
    }
    else if( m_qname != null )
    {
      property1 = f1.getProperty( m_qname );
      property2 = f2.getProperty( m_qname );
    }

    if( m_pt != null && m_pt.isList() )
      return compareLists( (List<Comparable<Object>>) property1, (List<Comparable<Object>>) property2 );

    return compareComparable( (Comparable<Object>) property1, (Comparable<Object>) property2 );
  }

  private int compareComparable( final Comparable<Object> c1, final Comparable<Object> c2 )
  {
    if( c1 == null && c2 == null )
      return 0;

    if( c1 == null && c2 != null )
      return -1;

    if( c2 == null )
      return +1;

    return c1.compareTo( c2 );
  }

  /**
   * Compares two lists.
   * <p>
   * If the lists have not the same size, the size is compared.
   * </p>
   * <p>
   * If the list do have the same size, they are compared entry-wise.
   * </p>
   */
  private int compareLists( final List<Comparable<Object>> list1, final List<Comparable<Object>> list2 )
  {
    final int size1 = list1.size();
    final int size2 = list2.size();

    if( size1 != size2 )
      return Double.compare( size1, size2 );

    for( int i = 0; i < size1; i++ )
    {
      final Comparable<Object> c1 = list1.get( i );
      final Comparable<Object> c2 = list2.get( i );
      final int dif = compareComparable( c1, c2 );
      if( dif != 0 )
        return dif;
    }

    return 0;
  }
}