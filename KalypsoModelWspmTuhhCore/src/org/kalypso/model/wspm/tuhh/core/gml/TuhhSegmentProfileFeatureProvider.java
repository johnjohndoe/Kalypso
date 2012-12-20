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
package org.kalypso.model.wspm.tuhh.core.gml;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.IProfileFeatureProvider;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultFactory;
import org.kalypsodeegree.model.feature.Feature;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.CacheLoader.InvalidCacheLoadException;
import com.google.common.cache.LoadingCache;

/**
 * @author Gernot Belger
 */
public class TuhhSegmentProfileFeatureProvider implements IProfileFeatureProvider
{
  // TODO: fixes a slight performance problem for now: results where reloaded for every profile, when selected.
  // Problem is, that the cache does not know when to clear its state (wspm results are changed, fixations are change and so on).
  // We use a small cache expiration time for now, so user will not have problems with that.
  private final LoadingCache<Feature, IWspmResultNode> m_cache = CacheBuilder.newBuilder().expireAfterWrite( 30, TimeUnit.SECONDS ).maximumSize( 100 ).build( new CacheLoader<Feature, IWspmResultNode>()
  {
    @Override
    public IWspmResultNode load( final Feature parent ) throws Exception
    {
      return WspmResultFactory.createResultNode( null, parent );
    }
  } );

  @Override
  public IWspmResultNode getResult( final IProfileFeature profile )
  {
    if( profile == null )
      return null;

    final Feature parent = profile.getOwner();
    if( parent == null )
      return null;

    try
    {
      return m_cache.get( parent );
    }
    catch( final InvalidCacheLoadException e )
    {
      // REMARK: Happens if the loader returns null.
      return null;
    }
    catch( final ExecutionException e )
    {
      e.printStackTrace();
      return null;
    }
  }
}