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

import java.util.ArrayList;
import java.util.Collection;

import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Gernot Belger
 */
public class WspmResultProjectNode extends AbstractWspmResultNode
{
  private final TuhhWspmProject m_project;

  public WspmResultProjectNode( final TuhhWspmProject project )
  {
    super( null );

    m_project = project;
  }

  @Override
  protected IWspmResultNode[] createChildren( )
  {
    final Collection<IWspmResultNode> results = new ArrayList<>();

    final IFeatureBindingCollection<WspmWaterBody> waterBodies = m_project.getWaterBodies();
    for( final WspmWaterBody waterBody : waterBodies )
    {
      results.add( new WspmResultWaterNode( this, waterBody ) );
    }

    final IFeatureBindingCollection<TuhhCalculation> calculations = m_project.getCalculations();
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

  @Override
  public String getLabel( )
  {

    final String name = m_project.getName();
    if( name != null && name.length() > 0 )
      return name;

    return m_project.getId();
  }

  @Override
  protected String getInternalName( )
  {
    return m_project.getId();
  }

  @Override
  public Object getObject( )
  {
    return m_project;
  }
}