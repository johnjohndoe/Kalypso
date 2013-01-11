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
package org.kalypso.model.wspm.tuhh.ui.imports.ctripple;

import java.math.BigDecimal;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.ctripple.CodedTrippleProfile;
import org.kalypso.model.wspm.tuhh.core.ctripple.CodedTrippleProfileHorizon;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_Curve;

/**
 * @author Holger Albert
 */
public class CodedTrippleWorker extends AbstractCodedTrippleWorker
{
  private final CommandableWorkspace m_workspace;

  private final TuhhWspmProject m_targetProject;

  public CodedTrippleWorker( final CommandableWorkspace workspace, final TuhhWspmProject targetProject )
  {
    m_workspace = workspace;
    m_targetProject = targetProject;
  }

  @Override
  public void updateClassifications( final CodedTrippleImportData data ) throws Exception
  {
    final CodedTrippleClassificationUpdater classificationUpdater = new CodedTrippleClassificationUpdater( m_targetProject, data );
    classificationUpdater.updateClassification();
  }

  @Override
  public IProfileFeature createNewProfile( final CodedTrippleImportData data, final CodedTrippleProfile profile ) throws CoreException
  {
    final BigDecimal station = profile.getStation();

    try
    {
      final CodedTrippleProfileHorizon baseHorizon = profile.getBaseHorizon();
      if( baseHorizon == null )
        throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getID(), String.format( Messages.getString( "CodedTrippleWorker.0" ), station.doubleValue() ) ) ); //$NON-NLS-1$

      final String name = getName( baseHorizon );
      final String description = getDescription( profile.getMapper(), baseHorizon );

      final String riverId = "-1"; //$NON-NLS-1$
      final String riverName = Messages.getString( "CodedTrippleWorker.2" ); //$NON-NLS-1$
      final GM_Curve riverGeometry = null;

      final IProfileFeature profileFeature = createNewProfile( riverId, data.isDirectionUpstreams() );
      profileFeature.setName( name );
      profileFeature.setDescription( description );
      profileFeature.setSrsName( data.getCoordinateSystem() );
      profileFeature.setBigStation( station );

      final WspmWaterBody water = profileFeature.getWater();
      water.setName( riverName );
      water.setCenterLine( riverGeometry );

      final CodedTrippleProfilePointCreator pointCreator = new CodedTrippleProfilePointCreator( profile.getMapper(), baseHorizon, profileFeature );
      pointCreator.createProfilePoints();

      final CodedTrippleProfileObjectCreator objectCreator = new CodedTrippleProfileObjectCreator( profile.getMapper(), profile, profileFeature );
      objectCreator.createProfileObjects();

      return profileFeature;
    }
    catch( final Exception ex )
    {
      final String message = String.format( Messages.getString( "CodedTrippleWorker.1" ), station.doubleValue() ); //$NON-NLS-1$
      final Status status = new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getID(), message, ex );
      throw new CoreException( status );
    }
  }

  @Override
  public void createMarkers( final IProfileFeature profileFeature )
  {
    /* HINT: The profile points must be created already. */
    // TODO
  }

  @Override
  public void fireChangeEvents( )
  {
    final IFeatureBindingCollection<WspmWaterBody> waterBodies = m_targetProject.getWaterBodies();
    final WspmWaterBody[] wbs = waterBodies.toArray( new WspmWaterBody[] {} );

    m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, m_targetProject, wbs, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

    try
    {
      m_workspace.postCommand( new EmptyCommand( "", false ) ); //$NON-NLS-1$
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  private IProfileFeature createNewProfile( final String riverId, final boolean isDirectionUpstreams ) throws GMLSchemaException
  {
    final WspmWaterBody water = m_targetProject.createOrGetWaterBodyByRefNr( riverId, isDirectionUpstreams );

    final IProfileFeature newProfile = water.createNewProfile();
    newProfile.setProfileType( IWspmTuhhConstants.PROFIL_TYPE_PASCHE );

    return newProfile;
  }
}