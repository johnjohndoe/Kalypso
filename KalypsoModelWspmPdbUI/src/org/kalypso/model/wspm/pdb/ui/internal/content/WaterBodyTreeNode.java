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
package org.kalypso.model.wspm.pdb.ui.internal.content;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;

/**
 * @author Gernot Belger
 */
class WaterBodyTreeNode implements Comparable<WaterBodyTreeNode>
{
  private final SortedSet<WaterBodyTreeNode> m_waterChildren = new TreeSet<>();

  private Object[] m_allChildren = null;

  private final WaterBody m_water;

  public WaterBodyTreeNode( final WaterBody waterBody )
  {
    m_water = waterBody;
  }

  @Override
  public int compareTo( final WaterBodyTreeNode other )
  {
    final String code1 = getCode();
    final String code2 = other.getCode();

    final int l1 = code1.length();
    final int l2 = code2.length();

    final int lComp = l1 - l2;
    if( lComp == 0 )
      return code1.compareTo( code2 );

    return lComp;
  }

  @Override
  public String toString( )
  {
    return getCode();
  }

  public String getCode( )
  {
    if( m_water == null )
      return StringUtils.EMPTY;

    return m_water.getName();
  }

  public String getLabel( )
  {
    return m_water.getLabel();
  }

  public WaterBody getWaterBody( )
  {
    return m_water;
  }

  public Object[] getAllChildren( )
  {
    if( m_allChildren == null )
    {
      final Object[] realChildren = getRealChildren();
      final Collection<Object> all = new ArrayList<>( m_waterChildren.size() + realChildren.length );

      for( final WaterBodyTreeNode childNode : m_waterChildren )
        all.add( childNode.getWaterBody() );

      all.addAll( Arrays.asList( realChildren ) );
      m_allChildren = all.toArray( new Object[all.size()] );
      return m_allChildren;
    }

    return m_allChildren;
  }

  private Object[] getRealChildren( )
  {
    if( m_water == null )
      return ArrayUtils.EMPTY_OBJECT_ARRAY;

    final Set<Object> children = new HashSet<>();

    final Set<CrossSection> crossSections = m_water.getCrossSections();
    for( final CrossSection crossSection : crossSections )
      children.add( crossSection.getState() );

    /* Only add events not contained in a state */
    final Set<Event> events = m_water.getEvents();
    for( final Event event : events )
    {
      final State state = event.getState();
      if( state == null )
        children.add( event );
    }

    return children.toArray( new Object[children.size()] );
  }

  private void addChild( final WaterBodyTreeNode childNode )
  {
    /* find parent */
    final WaterBodyTreeNode parent = findParent( childNode );
    if( parent == null )
      m_waterChildren.add( childNode );
    else
      parent.addChild( childNode );
  }

  private WaterBodyTreeNode findParent( final WaterBodyTreeNode childNode )
  {
    final String code = childNode.getCode();
    for( final WaterBodyTreeNode node : m_waterChildren )
    {
      if( code.startsWith( node.getCode() ) )
        return node;
    }

    return null;
  }

  public static WaterBodyTreeNode buildTree( final Collection<WaterBody> waterBodies )
  {
    /* Build flat structure in order to sort the elements */
    final SortedSet<WaterBodyTreeNode> allNodes = new TreeSet<>();
    for( final WaterBody water : waterBodies )
      allNodes.add( new WaterBodyTreeNode( water ) );

    final WaterBodyTreeNode rootNode = new WaterBodyTreeNode( null );
    for( final WaterBodyTreeNode node : allNodes )
      rootNode.addChild( node );

    return rootNode;
  }

  public void accept( final IWaterBodyTreeVisitor visitor )
  {
    try
    {
      visitor.visit( this );
    }
    catch( final IWaterBodyTreeVisitor.CancelException e )
    {
      return;
    }

    for( final WaterBodyTreeNode node : m_waterChildren )
      node.accept( visitor );
  }
}