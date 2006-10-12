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
package org.kalypsodeegree_impl.model.feature.binding;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class NamedFeatureHelper
{
  private static final QName GML_NAME = new QName( NS.GML3, "name" );

  private static final QName GML_DESCRIPTION = new QName( NS.GML3, "description" );

  private NamedFeatureHelper( )
  {
  }

  /** Gets the 'gml:name' property which all (normal) feature have. Handles the undbounded and restricted case. */
  public static String getName( final Feature namedFeature )
  {
    final Object value = namedFeature.getProperty( GML_NAME );

    if( value instanceof String )
      return (String) value;
    else if( value instanceof List )
    {
      final List nameList =  (List) value;
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
    final IValuePropertyType vpt = (IValuePropertyType) namedFeature.getFeatureType().getProperty( GML_NAME );
    if( vpt.isList() )
    {
      final ArrayList<String> nameList = new ArrayList<String>( 1 );
      nameList.add( name );
      namedFeature.setProperty( GML_NAME, nameList );
    }
    else
      namedFeature.setProperty( GML_NAME, name );
  }

  public static String getDescription( final Feature namedFeature )
  {
    final Object desc = namedFeature.getProperty( GML_DESCRIPTION );
    return desc == null ? "" : (String) desc;
  }

  public static void setDescription( final Feature namedFeature, final String desc )
  {
    final IPropertyType gmlDescProp = namedFeature.getFeatureType().getProperty( GML_DESCRIPTION );
    namedFeature.setProperty( gmlDescProp, desc );
  }

}
