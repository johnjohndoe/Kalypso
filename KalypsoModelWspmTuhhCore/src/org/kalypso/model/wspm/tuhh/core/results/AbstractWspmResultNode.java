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
package org.kalypso.model.wspm.tuhh.core.results;

/**
 * @author Gernot Belger
 */
public abstract class AbstractWspmResultNode implements IWspmResultNode
{
  private final IWspmResultNode m_parentNode;

  private IWspmResultNode[] m_children;

  public AbstractWspmResultNode( final IWspmResultNode parentNode )
  {
    m_parentNode = parentNode;
  }

  @Override
  public final IWspmResultNode getParent( )
  {
    return m_parentNode;
  }

  @Override
  public final String getName( )
  {
    final IWspmResultNode parent = getParent();
    final String id = getInternalName();

    if( parent == null )
      return id;

    return String.format( "%s/%s", parent.getName(), id ); //$NON-NLS-1$
  }

  protected abstract String getInternalName( );

  @Override
  public boolean equals( final Object other )
  {
    if( !(other instanceof AbstractWspmResultNode) )
      return false;

    final IWspmResultNode otherNode = (IWspmResultNode) other;
    return getName().equals( otherNode.getName() );
  }

  @Override
  public int hashCode( )
  {
    return getName().hashCode();
  }

  @Override
  public String toString( )
  {
    return getName();
  }

  @Override
  public synchronized final IWspmResultNode[] getChildResults( )
  {
    if( m_children == null )
      m_children = createChildren();

    return m_children;
  }

  protected abstract IWspmResultNode[] createChildren( );
}