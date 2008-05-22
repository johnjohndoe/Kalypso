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
package org.kalypsodeegree_impl.model.feature;

import java.util.HashMap;
import java.util.Map;

import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureProvider;

/**
 * Abstract feature provider factory which parses the href and caches the providers.
 * 
 * @author Gernot Belger
 */
public class FeatureProviderFactoryCache
{
  private final Map<String, IFeatureProvider> m_providers = new HashMap<String, IFeatureProvider>();

  private final IFeatureProviderFactory m_factory;

  public FeatureProviderFactoryCache( final IFeatureProviderFactory factory )
  {
    m_factory = factory;
  }

  public void dispose( )
  {
    for( final IFeatureProvider provider : m_providers.values() )
      provider.dispose();
  }

  public IFeatureProviderFactory getFactory( )
  {
    return m_factory;
  }

  public IFeatureProvider getFeatureProvider( final GMLWorkspace context, final String urn )
  {
    if( m_providers.containsKey( urn ) )
      return m_providers.get( urn );

    final IFeatureProvider provider = m_factory.createFeatureProvider( context, urn );
    m_providers.put( urn, provider );
    return provider;
  }
}
