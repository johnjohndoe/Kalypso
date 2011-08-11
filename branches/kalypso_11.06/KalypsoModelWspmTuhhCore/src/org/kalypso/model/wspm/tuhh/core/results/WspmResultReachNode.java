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
package org.kalypso.model.wspm.tuhh.core.results;

import java.util.ArrayList;
import java.util.Collection;

import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;

/**
 * @author Gernot Belger
 */
public class WspmResultReachNode extends AbstractWspmResultNode
{
  private final TuhhReach m_reach;

  public WspmResultReachNode( final IWspmResultNode parent, final TuhhReach reach )
  {
    super( parent );

    m_reach = reach;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode#getChildResults()
   */
  @Override
  public IWspmResultNode[] getChildResults( )
  {
    final Collection<IWspmResultNode> results = new ArrayList<IWspmResultNode>();

    final TuhhCalculation[] calculations = m_reach.findCalculations();
    for( final TuhhCalculation calculation : calculations )
    {
      final IWspmResultNode node = WspmResultFactory.createCalculationNode( this, calculation );
      if( node != null )
      {
        results.add( node );
      }
    }

    return results.toArray( new IWspmResultNode[results.size()] );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode#getLabel()
   */
  @Override
  public String getLabel( )
  {
    return m_reach.getName();
  }

  @Override
  protected String getInternalName( )
  {
    return m_reach.getId();
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode#getObject()
   */
  @Override
  public Object getObject( )
  {
    return m_reach;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.results.AbstractWspmResultNode#toString()
   */
  @Override
  public String toString( )
  {
    return String.format( "%s\n%s", super.toString(), m_reach.getName() );
  }
}
