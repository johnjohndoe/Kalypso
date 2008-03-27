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
