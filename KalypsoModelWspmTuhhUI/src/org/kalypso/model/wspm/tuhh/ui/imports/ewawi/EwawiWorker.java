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
package org.kalypso.model.wspm.tuhh.ui.imports.ewawi;

import java.io.File;
import java.math.BigDecimal;
import java.net.URI;

import org.apache.commons.io.FilenameUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.ewawi.data.EwawiPro;
import org.kalypso.model.wspm.ewawi.data.EwawiSta;
import org.kalypso.model.wspm.ewawi.utils.EwawiException;
import org.kalypso.model.wspm.ewawi.utils.GewShape;
import org.kalypso.model.wspm.ewawi.utils.profiles.EwawiProfile;
import org.kalypso.model.wspm.ewawi.utils.profiles.EwawiProfilePart;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree_impl.gml.binding.commons.Image;

/**
 * @author Holger Albert
 */
public class EwawiWorker extends AbstractEwawiWorker
{
  private final CommandableWorkspace m_workspace;

  private final TuhhWspmProject m_targetProject;

  public EwawiWorker( final CommandableWorkspace workspace, final TuhhWspmProject targetProject )
  {
    m_workspace = workspace;
    m_targetProject = targetProject;
  }

  @Override
  public void updateClassifications( ) throws Exception
  {
    final EwawiClassificationUpdater classificationUpdater = new EwawiClassificationUpdater( m_targetProject );
    classificationUpdater.updateClassification();
  }

  @Override
  public IProfileFeature createNewProfile( final EwawiImportData data, final GewShape gewShape, final EwawiSta staIndex, final EwawiProfile ewawiProfile ) throws CoreException
  {
    final BigDecimal station = ewawiProfile.getStation();

    try
    {
      final EwawiProfilePart basePart = ewawiProfile.getBasePart();
      if( basePart == null )
        throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getID(), String.format( Messages.getString( "EwawiWorker.0" ), station.doubleValue() ) ) ); //$NON-NLS-1$

      final String name = getName( staIndex, basePart );
      final String description = getDescription( staIndex, basePart );
      final String[] photos = basePart.getPhotos( staIndex );

      final String riverId = getRiverId( basePart );
      final String riverName = getRiverName( data, gewShape, staIndex, basePart );
      final GM_Curve riverGeometry = getRiverGeometry( gewShape, staIndex, basePart );

      final IProfileFeature profileFeature = createNewProfile( riverId, data.isDirectionUpstreams() );
      profileFeature.setName( name );
      profileFeature.setDescription( description );
      profileFeature.setSrsName( data.getCoordinateSystem() );
      profileFeature.setBigStation( station );

      final WspmWaterBody water = profileFeature.getWater();
      water.setName( riverName );
      water.setCenterLine( riverGeometry );

      for( final String foto : photos )
      {
        final File fotoDirectory = data.getFotoDirectory();
        final File fotoFile = new File( fotoDirectory, foto );
        final URI fotoUrl = fotoFile.toURI();

        final Image image = profileFeature.addImage( fotoUrl );
        image.setName( FilenameUtils.removeExtension( foto ) );
      }

      final EwawiProfilePointCreator pointCreator = new EwawiProfilePointCreator( staIndex, basePart, profileFeature );
      pointCreator.createProfilePoints();

      final EwawiProfileObjectCreator objectCreator = new EwawiProfileObjectCreator( staIndex, ewawiProfile, profileFeature );
      objectCreator.createProfileObjects();

      return profileFeature;
    }
    catch( final DBaseException | EwawiException | GMLSchemaException e )
    {
      final String message = String.format( Messages.getString( "EwawiWorker.1" ), station.doubleValue() ); //$NON-NLS-1$
      final Status status = new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getID(), message, e );
      throw new CoreException( status );
    }
  }

  @Override
  public void createMarkers( final IProfileFeature profileFeature )
  {
    /* HINT: The profile points must be created already. */
    final EwawiProfilePointMarkerCreator markerCreator = new EwawiProfilePointMarkerCreator( profileFeature );
    markerCreator.createProfilePointMarker();
  }

  @Override
  public void updateWaterLevelFixation( final EwawiSta staIndex, final EwawiPro proIndex ) throws EwawiException
  {
    final EwawiWaterLevelFixationUpdater waterLevelFixationUpdater = new EwawiWaterLevelFixationUpdater( m_targetProject, staIndex, proIndex );
    waterLevelFixationUpdater.updateWaterLevelFixation();
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