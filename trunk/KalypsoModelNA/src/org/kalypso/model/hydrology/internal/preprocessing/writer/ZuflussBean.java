/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.hydrology.internal.preprocessing.writer;

import org.eclipse.core.runtime.Assert;
import org.kalypso.model.hydrology.binding.model.nodes.Node;

final class ZuflussBean
{
  private final int m_izug;

  private final int m_iabg;

  private final int m_iueb;

  private final int m_izuf;

  private final int m_ivzwg;

  private final Node m_branchNode;

  private final String m_argument;

  public ZuflussBean( final int izug, final int iabg, final int iueb, final int izuf, final int ivzwg, final Node branchNode, final String argument )
  {
    m_izug = izug;
    m_iabg = iabg;
    m_iueb = iueb;
    m_izuf = izuf;
    m_ivzwg = ivzwg;
    m_branchNode = branchNode;
    m_argument = argument;

    Assert.isTrue( argument != null );
  }

  public Node getBranchingNode( )
  {
    return m_branchNode;
  }

  public int getIzug( )
  {
    return m_izug;
  }

  public int getIabg( )
  {
    return m_iabg;
  }

  public int getIueb( )
  {
    return m_iueb;
  }

  public int getIvzwg( )
  {
    return m_ivzwg;
  }

  public int getIzuf( )
  {
    return m_izuf;
  }

  public String getArgument( )
  {
    return m_argument;
  }
}