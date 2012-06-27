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
package org.kalypso.ui.rrm.internal.timeseries.view.dnd;

import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNode;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Dirk Kuch
 */
public final class TimeseriesManagementTreeDndSupport
{
  private TimeseriesManagementTreeDndSupport( )
  {
  }

  public static ITimeseries[] doSelection( final IStructuredSelection selection )
  {
    final Set<ITimeseries> timeserieses = new LinkedHashSet<>();

    final Iterator< ? > iterator = selection.iterator();
    while( iterator.hasNext() )
    {
      final Object next = iterator.next();
      if( next instanceof TreeNode )
      {
        final TreeNode node = (TreeNode) next;

        final ITimeseries[] selected = doSelection( node );
        if( ArrayUtils.isNotEmpty( selected ) )
          Collections.addAll( timeserieses, selected );
        else
        {
          // parameter tree item selected? so iterate over children

          final TreeNode[] children = node.getChildren();
          for( final TreeNode child : children )
          {
            Collections.addAll( timeserieses, doSelection( child ) );
          }
        }
      }
    }

    return timeserieses.toArray( new ITimeseries[] {} );
  }

  private static ITimeseries[] doSelection( final TreeNode node )
  {
    final Object objTimeseries = node.getAdapter( ITimeseries.class );
    final Object objStation = node.getAdapter( IStation.class );

    if( objTimeseries instanceof ITimeseries )
      return new ITimeseries[] { (ITimeseries) objTimeseries };
    else if( objStation instanceof IStation )
    {
      final IStation station = (IStation) objStation;
      final IFeatureBindingCollection<ITimeseries> collection = station.getTimeseries();

      return collection.toArray( new ITimeseries[] {} );
    }

    return new ITimeseries[] {};
  }
}
