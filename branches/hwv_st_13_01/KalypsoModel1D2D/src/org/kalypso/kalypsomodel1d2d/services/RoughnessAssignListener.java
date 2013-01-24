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
package org.kalypso.kalypsomodel1d2d.services;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.contribs.eclipse.core.runtime.jobs.MutexRule;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessLayer;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Dejan Antanaskovic
 * @author Gernot Belger
 */
public class RoughnessAssignListener implements ModellEventListener
{
  private static ISchedulingRule m_mutexRule = new MutexRule();

  private final ITerrainModel m_terrainModel;

  private final IFEDiscretisationModel1d2d m_discModel;

  private RoughnessAssignService m_job;

  public RoughnessAssignListener( final IFEDiscretisationModel1d2d discModel, final ITerrainModel terrainModel )
  {
    m_discModel = discModel;
    m_terrainModel = terrainModel;

    startJob( null );
  }

  /**
   * Starts the roughness (re)assignment job
   *
   * @param envelope
   *          The area where roughness recalculation is needed; if <code>null</code>, whole model will be recalculated
   */
  private void startJob( final GM_Envelope envelope )
  {
    // if( m_job != null )
    // m_job.cancel();

    m_job = new RoughnessAssignService( Messages.getString( "org.kalypso.kalypsomodel1d2d.services.RoughnessAssignListener.0" ), m_terrainModel, m_discModel ); //$NON-NLS-1$

    m_job.setWorkarea( envelope );

    // m_job.setSystem( true );
    m_job.setUser( false );
    m_job.setPriority( Job.SHORT );
    m_job.setRule( m_mutexRule );
    m_job.schedule( 500 );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  @Override
  public void onModellChange( final ModellEvent modellEvent )
  {
    /* If the event comes from the RoughnessAssignSerice, just ignore it */
    if( modellEvent instanceof RoughnessAssignServiceModellEvent )
      return;

    /* In every other case: refresh the roughnes assignment */
    if( modellEvent instanceof FeatureStructureChangeModellEvent )
    {
      final FeatureStructureChangeModellEvent event = (FeatureStructureChangeModellEvent) modellEvent;
      final Feature[] changedFeatures = event.getChangedFeatures();
      final GM_Envelope envelope = isRoughnessAssignNeeded( changedFeatures );
      if( envelope != null )
        startJob( envelope );
    }
    else if( modellEvent instanceof FeaturesChangedModellEvent )
    {
      final FeaturesChangedModellEvent event = (FeaturesChangedModellEvent) modellEvent;
      final Feature[] changedFeatures = event.getFeatures();
      final GM_Envelope envelope = isRoughnessAssignNeeded( changedFeatures );
      if( envelope != null )
        startJob( envelope );
    }
    else
      startJob( null );
  }

  public void dispose( )
  {
    if( m_job != null )
      m_job.cancel();
  }

  private GM_Envelope isRoughnessAssignNeeded( final Feature[] features )
  {
    if( features == null || features.length == 0 )
      return null;

    for( final Feature element : features )
    {
      if( element == null )
        continue;
      final QName qname = element.getFeatureType().getQName();
      if( qname.equals( IPolyElement.QNAME ) || qname.equals( IRoughnessPolygon.QNAME ) || qname.equals( IRoughnessLayer.QNAME ) )
        return FeatureHelper.getEnvelope( features );

      // Also handle change of node, it's position may have changed, so the geometry of it's adjacent elements may have
      // changed as well
      if( qname.equals( IFE1D2DNode.QNAME ) )
      {
        final GM_Envelope envelope = FeatureHelper.getEnvelope( features );
        // WE do buffer the envelope a bit here, as it may be only one point. A too big envelope only results in too
        // many
        // elements to be reassigned, which is not a problem.
        // REMARK: the size of the buffer depends on the coordinate system...; lets hope no one uses lat/lon here...
        // TODO: instead of using the location, we could directly determine the relevant elements...
        return envelope.getBuffer( 1.0 );
      }
    }

    return null;
  }
}
