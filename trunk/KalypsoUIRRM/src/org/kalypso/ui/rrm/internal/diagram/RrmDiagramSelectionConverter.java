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
package org.kalypso.ui.rrm.internal.diagram;

import java.net.URL;
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
import org.kalypso.model.hydrology.timeseries.binding.IStation;
import org.kalypso.model.hydrology.timeseries.binding.ITimeseries;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ui.rrm.internal.results.view.base.IHydrologyResultReference;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNode;

/**
 * @author Dirk Kuch
 */
public class RrmDiagramSelectionConverter
{

  public static IStructuredSelection doConvert( final IStructuredSelection selection )
  {
    final Set<Object> items = new LinkedHashSet<>();

    final Iterator< ? > iterator = selection.iterator();
    while( iterator.hasNext() )
    {
      final Object next = iterator.next();

      if( next instanceof TreeNode )
      {
        final TreeNode node = (TreeNode) next;
        final Object objStation = node.getAdapter( IStation.class );
        final Object objTimeseries = node.getAdapter( ITimeseries.class );

        /** result references tree node */
        final Object objResultReference = node.getAdapter( IHydrologyResultReference.class );

        final Object objCatchment = node.getAdapter( Catchment.class );
        final Object objNode = node.getAdapter( INode.class );
        final Object objStorageChannel = node.getAdapter( IStorageChannel.class );

        if( objStation instanceof IStation )
        {
          Collections.addAll( items, ((IStation) objStation).getTimeseries().toArray() );
        }
        else if( objResultReference instanceof IHydrologyResultReference )
        {
          items.add( doConvert( (IHydrologyResultReference) objResultReference ) );
        }
        else if( objCatchment != null || objNode != null || objStorageChannel != null )
        {
          final TreeNode[] children = node.getChildren();
          for( final TreeNode child : children )
          {
            final Object resultRefernce = child.getAdapter( IHydrologyResultReference.class );
            if( resultRefernce != null )
              items.add( doConvert( (IHydrologyResultReference) resultRefernce ) );
          }

        }
        else if( Objects.isNull( objTimeseries ) ) // parameter tree item
        {
          Collections.addAll( items, node.getChildren() );
        }
        else
          items.add( next );
      }

    }

    return new StructuredSelection( items.toArray() );
  }

  private static Object doConvert( final IHydrologyResultReference reference )
  {
    /*
     * TODO perhaps own zmlsourceelement - we don't know, how good or bad the meta data of the underlying zml is
     * (diagramm legend, table header, aso)
     */

    try
    {
      final URL urlZmlSource = reference.getUrl();

      return ZmlFactory.parseXML( urlZmlSource );
    }
    catch( final Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

    return null;
  }
}
