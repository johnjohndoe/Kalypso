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

import org.kalypso.model.wspm.core.gml.WspmFixation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;

/**
 * @author Gernot Belger
 */
public class WspmResultFixationNode extends AbstractWspmResultNode implements IWspmResult
{
  private final WspmFixation m_fixation;

  private WspmResultLengthSection m_lengthSection;

  public WspmResultFixationNode( final IWspmResultNode parent, final WspmFixation fixation )
  {
    super( parent );

    m_fixation = fixation;
  }

  @Override
  protected IWspmResultNode[] createChildren( )
  {
    return new IWspmResultNode[0];
  }

  @Override
  public String getLabel( )
  {
    return m_fixation.getName();
  }

  @Override
  protected String getInternalName( )
  {
    // id of feature would be nicer
    return m_fixation.getName();
  }

  @Override
  public WspmResultLengthSection getLengthSection( )
  {
    synchronized( m_fixation )
    {
      if( m_lengthSection == null )
      {
        final IObservation<TupleResult> observation = m_fixation.toObservation();
        m_lengthSection = new WspmResultLengthSection( observation );
      }
    }

    return m_lengthSection;
  }

  @Override
  public Object getObject( )
  {
    return m_fixation;
  }

  @Override
  public TuhhCalculation getCalculation( )
  {
    throw new UnsupportedOperationException();
  }
}