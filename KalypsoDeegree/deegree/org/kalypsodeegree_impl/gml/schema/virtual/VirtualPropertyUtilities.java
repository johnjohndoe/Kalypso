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
package org.kalypsodeegree_impl.gml.schema.virtual;

import java.util.Collection;
import java.util.HashMap;
import java.util.Set;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author doemming
 */
public class VirtualPropertyUtilities
{
  final static private HashMap<IFeatureType, HashMap> m_cache = new HashMap<IFeatureType, HashMap>();

  final static private VirtualFeatureTypeProperty[] m_noVTP = new VirtualFeatureTypeProperty[0];

  /**
   * @deprecated use getPropertyType( IFeatureType ,QName )
   */
  public static VirtualFeatureTypeProperty getPropertyType( final IFeatureType featureType, final String propNameLocalPart )
  {
    final HashMap map = getMap( featureType );
    return (VirtualFeatureTypeProperty) map.get( propNameLocalPart );
  }

  public static VirtualFeatureTypeProperty[] getVirtualProperties( final IFeatureType featureType )
  {
    final HashMap map = getMap( featureType );
      Collection collection = map.values();
    return (VirtualFeatureTypeProperty[]) collection.toArray( new VirtualFeatureTypeProperty[collection.size()] );
    // final Collection<VirtualFeatureTypeProperty> collection = map.k();
    // return (VirtualFeatureTypeProperty[]) collection.toArray( new VirtualFeatureTypeProperty[collection.size()] );
  }

  private static HashMap getMap( IFeatureType ft )
  {
    if( !m_cache.containsKey( ft ) )
    {
      final HashMap map = new HashMap();
      // collect them
      final VirtualFeatureTypeRegistry registry = VirtualFeatureTypeRegistry.getInstance();
      final VirtualFeatureTypeProperty[] vpt4FT = registry.getVirtualFeatureTypePropertiesFor( ft );
      for( int i = 0; i < vpt4FT.length; i++ )
      {
        final VirtualFeatureTypeProperty vpt = vpt4FT[i];
        map.put( vpt.getName(), vpt );
        map.put( vpt.getQName(), vpt );
      }
      final IPropertyType[] properties = ft.getProperties();
      for( int i = 0; i < properties.length; i++ )
      {
        final IPropertyType pt = properties[i];
        final VirtualFeatureTypeProperty[] vpt4PT = registry.getVirtualFeatureTypePropertiesFor( pt );
        for( int j = 0; j < vpt4PT.length; j++ )
        {
          final VirtualFeatureTypeProperty vpt = vpt4PT[j];
          map.put( vpt.getName(), vpt );
          map.put( vpt.getQName(), vpt );
        }
      }
      m_cache.put( ft, map );
    }
    return m_cache.get( ft );
  }
}
