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

import org.kalypso.model.wspm.core.gml.WspmReach;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.pdb.wspm.CheckoutDataMapping;
import org.kalypso.model.wspm.pdb.wspm.CheckoutPdbData;
import org.kalypso.model.wspm.pdb.wspm.CheckoutPdbData.RemoveStrategy;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Gernot Belger
 */
public class CheckoutRemoveWorker
{
  private final CheckoutPdbData m_data;

  public CheckoutRemoveWorker( final CheckoutPdbData data )
  {
    m_data = data;
  }

  public void execute( )
  {
    final CheckoutDataMapping mapping = m_data.getMapping();

    final RemoveStrategy strategy = m_data.getRemoveStrategy();
    switch( strategy )
    {
      case keepAll:
        return;

      case keepWaterBodies:
        removeWaterBodyContents( mapping );
        return;

      case removeAll:
        removeWaterBodies( mapping );
        break;

    }
  }

  private void removeWaterBodyContents( final CheckoutDataMapping mapping )
  {
    final TuhhWspmProject project = mapping.getProject();
    clearCollection( mapping, project.getCalculations() );

    final IFeatureBindingCollection<WspmWaterBody> waterBodies = project.getWaterBodies();
    for( final WspmWaterBody waterBody : waterBodies )
    {
      final IFeatureBindingCollection<WspmReach> reaches = waterBody.getReaches();
      clearCollection( mapping, reaches );
      clearCollection( mapping, waterBody.getProfiles() );
      clearCollection( mapping, waterBody.getWspFixations() );
      clearCollection( mapping, waterBody.getRunoffEvents() );
    }
  }

  private void removeWaterBodies( final CheckoutDataMapping mapping )
  {
    final TuhhWspmProject project = mapping.getProject();
    clearCollection( mapping, project.getWaterBodies() );
  }

  private void clearCollection( final CheckoutDataMapping mapping, final IFeatureBindingCollection< ? extends Feature> list )
  {
    final Feature[] features = list.toArray( new Feature[list.size()] );

    for( final Feature element : features )
    {
      mapping.featureRemoved( element );
      // REMARK: not using #clear on IFeatureBindingCollection, it seems not to be correctly implemented
      list.remove( element );
    }
  }
}