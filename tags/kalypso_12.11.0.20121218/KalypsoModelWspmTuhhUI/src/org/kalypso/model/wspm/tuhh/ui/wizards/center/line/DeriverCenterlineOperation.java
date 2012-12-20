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
package org.kalypso.model.wspm.tuhh.ui.wizards.center.line;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.core.KalypsoModelWspmCorePlugin;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmProfileComparator;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.util.river.line.RiverLineBuilder;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.LineString;

/**
 * @author Gernot Belger
 */
public class DeriverCenterlineOperation implements ICoreRunnableWithProgress
{
  private final DeriveCenterlineData m_data;

  public DeriverCenterlineOperation( final DeriveCenterlineData data )
  {
    m_data = data;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final WspmWaterBody[] selectedWaterBodies = m_data.getSelectedWaterBodies();

    final Collection<FeatureChange> changes = new ArrayList<>( selectedWaterBodies.length );
    final IStatusCollector log = new StatusCollector( KalypsoModelWspmCorePlugin.getID() );

    /* Create center line for all water bodies */
    for( final WspmWaterBody waterBody : selectedWaterBodies )
    {
      try
      {
        final GM_Curve riverLine = createCenterLine( waterBody );
        changes.add( new FeatureChange( waterBody, WspmWaterBody.PROPERTY_CENTER_LINE, riverLine ) );
      }
      catch( final CoreException e )
      {
        log.add( e.getStatus() );
      }
    }

    /* Change workspace */
    try
    {
      final FeatureChange[] allChanges = changes.toArray( new FeatureChange[changes.size()] );
      final CommandableWorkspace workspace = m_data.getWorkspace();
      workspace.postCommand( new ChangeFeaturesCommand( workspace, allChanges ) );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return new Status( IStatus.ERROR, KalypsoModelWspmUIPlugin.ID, "Failed to change workspace", e ); //$NON-NLS-1$
    }

    return log.asMultiStatus( Messages.getString( "DeriverCenterlineOperation_0" ) ); //$NON-NLS-1$
  }

  private GM_Curve createCenterLine( final WspmWaterBody waterBody ) throws CoreException
  {
    try
    {
      final IFeatureBindingCollection<IProfileFeature> profileList = waterBody.getProfiles();

      /* Get and sort profiles in direction of flow */
      final IProfileFeature[] profiles = profileList.toArray( new IProfileFeature[profileList.size()] );
      Arrays.sort( profiles, new WspmProfileComparator( waterBody.isDirectionUpstreams() ) );

      /* Build center line from low points */
      final RiverLineBuilder builder = new RiverLineBuilder( profiles );
      final LineString centerline = builder.execute();

      return (GM_Curve) JTSAdapter.wrap( centerline, KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelWspmCorePlugin.getID(), Messages.getString( "DeriverCenterlineOperation_1" ), e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
  }
}