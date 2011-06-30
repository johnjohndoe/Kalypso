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
package org.kalypso.model.wspm.pdb.internal.wspm;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;

/**
 * @author Gernot Belger
 */
public class WaterBodyTreeNode implements Comparable<WaterBodyTreeNode>
{
  private final SortedSet<WaterBodyTreeNode> m_children = new TreeSet<WaterBodyTreeNode>();

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
    return String.format( "%s (%s)", m_water.getLabel(), m_water.getName() );
  }

  public WaterBody getWaterBody( )
  {
    return m_water;
  }

  public Object[] getAllChildren( )
  {
    if( m_allChildren == null )
    {
      final Object[] states = getStates();
      final Collection<Object> all = new ArrayList<Object>( m_children.size() + states.length );
      all.addAll( m_children );
      all.addAll( Arrays.asList( states ) );
      m_allChildren = all.toArray( new Object[all.size()] );
      return m_allChildren;
    }

    return m_allChildren;
  }

  private Object[] getStates( )
  {
    if( m_water == null )
      return ArrayUtils.EMPTY_OBJECT_ARRAY;

    final Set<CrossSection> crossSections = m_water.getCrossSections();
    final Set<State> states = new HashSet<State>();
    for( final CrossSection crossSection : crossSections )
      states.add( crossSection.getState() );

    return states.toArray( new State[states.size()] );
  }

  private void addChild( final WaterBodyTreeNode childNode )
  {
    /* find parent */
    final WaterBodyTreeNode parent = findParent( childNode );
    if( parent == null )
      m_children.add( childNode );
    else
      parent.addChild( childNode );
  }

  private WaterBodyTreeNode findParent( final WaterBodyTreeNode childNode )
  {
    final String code = childNode.getCode();
    for( final WaterBodyTreeNode node : m_children )
    {
      if( code.startsWith( node.getCode() ) )
        return node;
    }

    return null;
  }

  public static WaterBodyTreeNode buildTree( final List<WaterBody> waterBodies )
  {
    /* Build flat structure in order to sort the elements */
    final SortedSet<WaterBodyTreeNode> allNodes = new TreeSet<WaterBodyTreeNode>();
    for( final WaterBody water : waterBodies )
      allNodes.add( new WaterBodyTreeNode( water ) );

    final WaterBodyTreeNode rootNode = new WaterBodyTreeNode( null );
    for( final WaterBodyTreeNode node : allNodes )
      rootNode.addChild( node );
    return rootNode;
  }

  public boolean containsChildWithName( final String name )
  {
    final String myName = getWaterBody().getLabel().toLowerCase();
    if( myName.contains( name ) )
      return true;

    for( final WaterBodyTreeNode childNode : m_children )
    {
      if( childNode.containsChildWithName( name ) )
        return true;
    }

    return false;
  }
}