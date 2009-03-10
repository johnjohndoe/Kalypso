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
package org.kalypsodeegree_impl.gml.binding.commons;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class NamedFeatureHelper
{

  public static final QName GML_LOCATION = new QName( NS.GML3, "location" );

  private NamedFeatureHelper( )
  {
  }

  /**
   * Gets the 'gml:name' property which all (normal) feature have. Handles the undbounded and restricted case.
   * 
   * @return null, if the given feature has no 'gml:name' property. The empty string if the property is not set.
   */
  public static String getName( final Feature namedFeature )
  {
    final IFeatureType featureType = namedFeature.getFeatureType();
    final IPropertyType nameProperty = featureType.getProperty( Feature.QN_NAME );
    if( nameProperty == null )
      return null;

    final Object value = namedFeature.getProperty( nameProperty );

    if( value instanceof String )
      return (String) value;
    else if( value instanceof List )
    {
      final List nameList = (List) value;
      if( nameList == null || nameList.isEmpty() )
        return "";

      final String name = (String) nameList.get( 0 );
      return name == null ? "" : (String) name;
    }

    return "";
  }

  /** Sets the 'gml:name' property which all (normal) feature have. Handles the undbounded and restricted case. */
  public static void setName( final Feature namedFeature, final String name )
  {
    final IValuePropertyType vpt = (IValuePropertyType) namedFeature.getFeatureType().getProperty( Feature.QN_NAME );
    if( vpt.isList() )
    {
      final ArrayList<String> nameList = new ArrayList<String>( 1 );
      nameList.add( name );
      namedFeature.setProperty( Feature.QN_NAME, nameList );
    }
    else
      namedFeature.setProperty( Feature.QN_NAME, name );
  }

  /**
   * @return null, if the given feature has no 'gml:description' property. The empty string if the property is not set.
   */
  public static String getDescription( final Feature namedFeature )
  {
    final IFeatureType featureType = namedFeature.getFeatureType();
    final IPropertyType descProperty = featureType.getProperty( Feature.QN_DESCRIPTION );
    if( descProperty == null )
      return null;

    final Object desc = namedFeature.getProperty( descProperty );
    return desc == null ? "" : (String) desc;
  }

  public static void setDescription( final Feature namedFeature, final String desc )
  {
    final IPropertyType gmlDescProp = namedFeature.getFeatureType().getProperty( Feature.QN_DESCRIPTION );
    namedFeature.setProperty( gmlDescProp, desc );
  }

}
