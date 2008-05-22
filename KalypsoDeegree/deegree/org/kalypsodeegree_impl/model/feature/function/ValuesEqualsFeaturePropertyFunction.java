/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.model.feature.function;

import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;

/**
 * This function property simply compares a value with the value of a property.
 * <p>
 * Parameters:
 * <ul>
 * <li>compareToValue (Double): The value to be compared with.</li>
 * <li>compareValueProperty (String): Property which should be compared.</li>
 * </ul>
 * </p>
 * 
 * @author thuel2
 */
public class ValuesEqualsFeaturePropertyFunction extends FeaturePropertyFunction
{

  private Object m_compareToValue;

  private QName m_compareValueName = null;

  /**
   * @see org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction#init(java.util.Map)
   */
  @Override
  public void init( Map<String, String> properties )
  {

    m_compareToValue = Double.valueOf( properties.get( "compareToValue" ) );
    final String compareValueProperty = properties.get( "compareValueProperty" );

    try
    {
      m_compareValueName = compareValueProperty == null ? null : QName.valueOf( compareValueProperty );
    }
    catch( final IllegalArgumentException e )
    {
      // ignore
    }

  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object getValue( Feature feature, IPropertyType pt, Object currentValue )
  {
    if( m_compareValueName == null )
      return null;

    final IFeatureType featureType = feature.getFeatureType();
    final IPropertyType compareValueProperty = featureType.getProperty( m_compareValueName );
    if( compareValueProperty == null )
      return null;

    final Object propCompareValue = feature.getProperty( compareValueProperty );
    if( propCompareValue == null )
      return null;

    return propCompareValue.equals( m_compareToValue );
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#setValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object setValue( Feature feature, IPropertyType pt, Object valueToSet )
  {

    return null;
  }
}
