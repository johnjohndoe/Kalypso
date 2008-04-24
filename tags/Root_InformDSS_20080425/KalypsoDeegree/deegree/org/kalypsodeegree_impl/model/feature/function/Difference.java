/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypsodeegree_impl.model.feature.function;

import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;

/**
 * calculates difference minuend - subtrahend
 * 
 * @author thuel2
 */
public class Difference extends FeaturePropertyFunction
{
  private QName m_minuendName;

  private QName m_subtrahendName;

  /**
   * @see org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction#init(java.util.Map)
   */
  @Override
  public void init( Map<String, String> properties )
  {
    final String minuendProp = properties.get( "minuendProperty" );
    final String subtrahendProp = properties.get( "subtrahendProperty" );

    try
    {
      m_minuendName = minuendProp == null ? null : QName.valueOf( minuendProp );
      m_subtrahendName = subtrahendProp == null ? null : QName.valueOf( subtrahendProp );
    }
    catch( final IllegalArgumentException e )
    {
      e.printStackTrace();
    }

  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object getValue( Feature feature, IPropertyType pt, Object currentValue )
  {
    final IFeatureType featureType = feature.getFeatureType();

    final IPropertyType minuendProperty = featureType.getProperty( m_minuendName );
    final IPropertyType subtrahendProperty = featureType.getProperty( m_subtrahendName );

    if( minuendProperty == null || subtrahendProperty == null )
      return null;

    final Object objMinuend = feature.getProperty( minuendProperty );
    final Object objSubtrahend = feature.getProperty( subtrahendProperty );

    if( !(objMinuend instanceof Number) || !(objSubtrahend instanceof Number) )
      return null;

    final double minuend = ((Number) objMinuend).doubleValue();
    final double subtrahend = ((Number) objSubtrahend).doubleValue();

    return minuend - subtrahend;
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#setValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object setValue( Feature feature, IPropertyType pt, Object valueToSet )
  {
    // TODO Auto-generated method stub
    return null;
  }

}
