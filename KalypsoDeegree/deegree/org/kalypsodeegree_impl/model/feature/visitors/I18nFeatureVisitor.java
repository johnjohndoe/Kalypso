/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypsodeegree_impl.model.feature.visitors;

import java.util.List;
import java.util.Properties;

import org.apache.commons.lang.ObjectUtils;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;

/**
 * This visitor is used to i10 gml-dictionaries.<br>
 * For every string-property (even lists of strings) that start with '%', the value is retrieved from the given
 * properties for the key (after the '%').
 *
 * @author Monika Thül
 * @author Gernot Belger
 */
public class I18nFeatureVisitor implements FeatureVisitor
{
  private final Properties m_properties;

  public I18nFeatureVisitor( Properties properties )
  {
    m_properties = properties;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( Feature f )
  {
    IPropertyType[] properties = f.getFeatureType().getProperties();
    for( IPropertyType propertyType : properties )
    {
      if( propertyType instanceof IValuePropertyType )
      {
        final IValuePropertyType vpt = (IValuePropertyType) propertyType;
        final Class< ? > valueClass = vpt.getValueClass();
        if( String.class.isAssignableFrom( valueClass ) )
        {
          final Object property = f.getProperty( propertyType );
          if( propertyType.isList() )
          {
            final List<Object> list = (List<Object>) property;
            for( int i = 0; i < list.size(); i++ )
            {
              final String entry = (String) list.get( i );
              final String newValue = replace( entry );
              if( !ObjectUtils.equals( newValue, entry ) )
                list.set( i, newValue );
            }
          }
          else
          {
            final String newValue = replace( (String) property );
            if( !ObjectUtils.equals( newValue, property ) )
              f.setProperty( propertyType, newValue );
          }
        }
      }
    }

    return true;
  }

  private String replace( final String entry )
  {
    if( entry == null )
      return null;

    if( entry.length() == 0 )
      return entry;

    if( entry.charAt( 0 ) != '%' )
      return entry;

    String key = entry.substring( 1 );
    if( m_properties.containsKey( key ) )
      return m_properties.getProperty( key );

    return entry;
  }

}
