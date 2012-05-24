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
package org.kalypso.ui.rrm.internal.results.view.tree.strategies.collector;

import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.model.hydrology.binding.model.channels.Channel;
import org.kalypso.model.hydrology.binding.model.channels.IStorageChannel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollectionVisitor;

/**
 * @author Dirk Kuch
 */
public class ChannelResultCategoriesCollector extends AbstractResultCategoriesCollector implements IFeatureBindingCollectionVisitor<Channel>
{
  public ChannelResultCategoriesCollector( final Map<String, Set<Feature>> categories )
  {
    super( categories );
  }

  @Override
  public void visit( final Channel channel )
  {
    if( channel instanceof IStorageChannel )
    {
      final IStorageChannel storage = (IStorageChannel) channel;
      final String category = storage.getResultCategory();
      if( StringUtils.isNotBlank( category ) )
        doAdd( category, storage );
    }

  }

}
