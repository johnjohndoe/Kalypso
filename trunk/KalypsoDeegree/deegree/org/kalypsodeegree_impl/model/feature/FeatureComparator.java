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

package org.kalypsodeegree_impl.model.feature;

import java.util.Comparator;

import org.kalypsodeegree.model.feature.Feature;

/**
 * A simple comparator for features which actually compares the value of the given property. A specific comparator can
 * be provided for comparing the values of the property.
 * 
 * @author schlienger
 */
public class FeatureComparator implements Comparator
{
  private final String m_propertyName;
  private final Comparator m_propertyValueComparator;

  /**
   * Constructor without specific property comparator, values must be comparable.
   * 
   * @param propertyName
   *          name of the property to be compared
   */
  public FeatureComparator( final String propertyName )
  {
    this( propertyName, null );
  }

  /**
   * Constructor with specific property comparator.
   * 
   * @param propertyName
   *          name of the property to be compared
   * @param propertyValueComparator
   *          comparator to use for comparing the values of the given property. Optional, can be null.
   */
  public FeatureComparator( final String propertyName, final Comparator propertyValueComparator )
  {
    m_propertyName = propertyName;
    m_propertyValueComparator = propertyValueComparator;
  }

  /**
   * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
   */
  public int compare( Object o1, Object o2 )
  {
    if( o1 == o2 )
      return 0;

    final Feature f1 = (Feature)o1;
    final Feature f2 = (Feature)o2;

    final Object value1 = f1.getProperty( m_propertyName );
    final Object value2 = f2.getProperty( m_propertyName );

    if( m_propertyValueComparator != null )
      return m_propertyValueComparator.compare( value1, value2 );

    return ( (Comparable)value1 ).compareTo( value2 );
  }
}
