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
package org.kalypso.ui.rrm.internal.diagram;

import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.channels.IStorageChannel;
import org.kalypso.model.hydrology.binding.model.nodes.INode;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ui.rrm.internal.results.view.base.IHydrologyResultReference;
import org.kalypso.ui.rrm.internal.results.view.tree.filter.IRrmDiagramFilterControl;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNode;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Dirk Kuch
 */
public class RrmDiagramSelectionConverter
{
  private final IRrmDiagramFilterControl m_filter;

  private final int m_minTraverseLevel;

  public RrmDiagramSelectionConverter( final IRrmDiagramFilterControl filter, final int minTraverseLevel )
  {
    m_filter = filter;
    m_minTraverseLevel = minTraverseLevel;
  }

  public IStructuredSelection doConvert( final IStructuredSelection selection )
  {
    final Set<Object> items = new LinkedHashSet<>();

    final Iterator< ? > iterator = selection.iterator();
    while( iterator.hasNext() )
    {
      final Object ptr = iterator.next();
      if( ptr instanceof TreeNode )
      {
        final Object[] tsObject = doConvert( (TreeNode)ptr );
        for( final Object object : tsObject )
        {
          if( object != null )
            items.add( object );
        }
      }
    }

    return new StructuredSelection( items.toArray() );
  }

  private Object[] doConvert( final TreeNode node )
  {
    final Set<Object> items = new LinkedHashSet<>();

    /* IStation */
    if( isStationItem( node ) )
    {
      Collections.addAll( items, doConvertStation( node ) );
    }
    /* ITimeseries */
    else if( isTimeseriesItem( node ) )
    {
      final ITimeseries timeseries = doConvertTimeseries( node );
      if( timeseries != null )
        items.add( timeseries );
    }
    else if( isResultReference( node ) )
    {
      items.add( doConvertResultReference( node ) );
    }
    else if( isResultNode( node ) )
    {
      Collections.addAll( items, doConvertResultNode( node ) );
    }
    else if( doTravese( node ) )
    {
      final TreeNode[] children = node.getChildren();
      for( final TreeNode child : children )
      {
        Collections.addAll( items, doConvert( child ) );
      }
    }

    return items.toArray();
  }

  private boolean doTravese( final TreeNode node )
  {
    final int level = getHierarchy( node );

    return level > m_minTraverseLevel;
  }

  private int getHierarchy( final TreeNode node )
  {
    int count = 0;

    TreeNode ptr = node;
    while( ptr != null )
    {
      ptr = ptr.getParent();
      count++;
    }

    return count;
  }

  private boolean isResultReference( final TreeNode node )
  {
    return Objects.isNotNull( node.getAdapter( IHydrologyResultReference.class ) );
  }

  private boolean isTimeseriesItem( final TreeNode node )
  {
    return Objects.isNotNull( node.getAdapter( ITimeseries.class ) );
  }

  private ITimeseries[] doConvertStation( final TreeNode node )
  {
    final Object objStation = node.getAdapter( IStation.class );
    if( !(objStation instanceof IStation) )
      return null;

    final Set<ITimeseries> selected = new LinkedHashSet<>();

    final IStation station = (IStation)objStation;
    final IFeatureBindingCollection<ITimeseries> timeserieses = station.getTimeseries();
    for( final ITimeseries timeseries : timeserieses )
    {
      if( doSelectTimeseries( timeseries ) )
        selected.add( timeseries );
    }

    return selected.toArray( new ITimeseries[] {} );

  }

  private boolean isStationItem( final TreeNode node )
  {
    return Objects.isNotNull( node.getAdapter( IStation.class ) );
  }

  private ITimeseries doConvertTimeseries( final TreeNode node )
  {
    final ITimeseries timeseries = (ITimeseries)node.getAdapter( ITimeseries.class );
    if( doSelectTimeseries( timeseries ) )
      return timeseries;

    return null;
  }

  private boolean doSelectTimeseries( final ITimeseries timeseries )
  {
    return m_filter.doSelect( timeseries.getParameterType() );
  }

  /**
   * @return observations from nanode, catchment or storage channel result node
   */
  private IHydrologyResultReference[] doConvertResultNode( final TreeNode node )
  {
    final Set<IHydrologyResultReference> references = new LinkedHashSet<>();

    final TreeNode[] children = node.getChildren();
    for( final TreeNode child : children )
    {
      if( isResultReference( child ) )
      {
        /*
         * FIXME at the moment result observations contains only one value axis. we will show all value axes of a
         * observation by default. perhaps (in future) this can be a problem!
         */
        final IHydrologyResultReference reference = doConvertResultReference( child );
        if( reference != null )
          references.add( reference );
      }
    }

    return references.toArray( new IHydrologyResultReference[] {} );
  }

  /**
   * @return node is result group element - like NaNode, Catchment or StorageChannel
   */
  private boolean isResultNode( final TreeNode node )
  {
    if( Objects.isNotNull( node.getAdapter( Catchment.class ) ) )
      return true;
    else if( Objects.isNotNull( node.getAdapter( INode.class ) ) )
      return true;
    else if( Objects.isNotNull( node.getAdapter( IStorageChannel.class ) ) )
      return true;

    return false;
  }

  private IHydrologyResultReference doConvertResultReference( final TreeNode node )
  {
    final Object objReference = node.getAdapter( IHydrologyResultReference.class );
    if( !(objReference instanceof IHydrologyResultReference) )
      return null;

    final IHydrologyResultReference reference = (IHydrologyResultReference)objReference;
    if( !reference.isValid() )
      return null;

    if( m_filter != null && !m_filter.doSelect( reference ) )
      return null;

    return reference;
  }
}
