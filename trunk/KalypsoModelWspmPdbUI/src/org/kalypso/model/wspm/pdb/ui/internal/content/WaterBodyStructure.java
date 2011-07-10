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
package org.kalypso.model.wspm.pdb.ui.internal.content;

import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.IWaterBodyStructure;

/**
 * @author Gernot Belger
 */
public class WaterBodyStructure implements IWaterBodyTreeVisitor, IWaterBodyStructure
{
  private final Map<WaterBody, WaterBodyTreeNode> m_waterToNodes = new IdentityHashMap<WaterBody, WaterBodyTreeNode>();

  private final Map<Object, WaterBody> m_parents = new IdentityHashMap<Object, WaterBody>();

  private final WaterBodyTreeNode m_rootNode;

  public WaterBodyStructure( final List<WaterBody> waterBodies )
  {
    m_rootNode = WaterBodyTreeNode.buildTree( waterBodies );

    m_rootNode.accept( this );
  }

  public WaterBody getRoot( )
  {
    return m_rootNode.getWaterBody();
  }

  public WaterBody findWaterBodyByName( final String waterBodyName )
  {
    final WaterBodyByNameFinder visitor = new WaterBodyByNameFinder( waterBodyName );
    m_rootNode.accept( visitor );
    return visitor.getResult();
  }

  public Event findEventName( final String eventName )
  {
    final EventByNameFinder visitor = new EventByNameFinder( eventName );
    m_rootNode.accept( visitor );
    return visitor.getResult();
  }

  @Override
  public Object[] getChildren( final Object waterBody )
  {
    if( waterBody == null || waterBody instanceof WaterBody )
    {
      final WaterBodyTreeNode node = findNode( (WaterBody) waterBody );
      return node.getAllChildren();
    }

    return ArrayUtils.EMPTY_OBJECT_ARRAY;
  }

  @Override
  public Object getParent( final Object element )
  {
    return m_parents.get( element );
  }

  private WaterBodyTreeNode findNode( final WaterBody waterBody )
  {
    return m_waterToNodes.get( waterBody );
  }

  @Override
  public void visit( final WaterBodyTreeNode node )
  {
    final WaterBody waterBody = node.getWaterBody();
    m_waterToNodes.put( waterBody, node );

    final Object[] children = node.getAllChildren();
    for( final Object child : children )
      m_parents.put( child, waterBody );
  }
}