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
package org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport.ewawi;

import java.io.File;
import java.math.BigDecimal;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FilenameUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetwork;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetworkCollection;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.ewawi.data.EwawiPro;
import org.kalypso.model.wspm.ewawi.data.EwawiSta;
import org.kalypso.model.wspm.ewawi.utils.EwawiException;
import org.kalypso.model.wspm.ewawi.utils.GewShape;
import org.kalypso.model.wspm.ewawi.utils.profiles.EwawiProfile;
import org.kalypso.model.wspm.ewawi.utils.profiles.EwawiProfilePart;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.imports.ewawi.AbstractEwawiWorker;
import org.kalypso.model.wspm.tuhh.ui.imports.ewawi.EwawiImportData;
import org.kalypso.model.wspm.tuhh.ui.imports.ewawi.EwawiProfileObjectCreator;
import org.kalypso.model.wspm.tuhh.ui.imports.ewawi.EwawiProfilePointCreator;
import org.kalypso.model.wspm.tuhh.ui.imports.ewawi.EwawiProfilePointMarkerCreator;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.gml.binding.commons.Image;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Holger Albert
 */
public class ImportEwawiWorker extends AbstractEwawiWorker
{
  private final IRiverProfileNetworkCollection m_profNetworkColl;

  private final List<IRiverProfileNetwork> m_addedNetworks;

  public ImportEwawiWorker( final IRiverProfileNetworkCollection profNetworkColl )
  {
    m_profNetworkColl = profNetworkColl;
    m_addedNetworks = new ArrayList<>();
  }

  @Override
  public void updateClassifications( )
  {
    /* Nothing to do. */
  }

  @Override
  public IProfileFeature createNewProfile( final EwawiImportData data, final GewShape gewShape, final EwawiSta staIndex, final EwawiProfile ewawiProfile ) throws CoreException
  {
    final BigDecimal station = ewawiProfile.getStation();

    try
    {
      final EwawiProfilePart basePart = ewawiProfile.getBasePart();
      if( basePart == null )
        throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getID(), String.format( Messages.getString( "ImportEwawiWorker.0" ), station.doubleValue() ) ) ); //$NON-NLS-1$

      final String name = getName( staIndex, basePart );
      final String description = getDescription( staIndex, basePart );
      final String[] photos = basePart.getPhotos( staIndex );

      final String riverId = getRiverId( basePart );
      final String riverName = getRiverName( data, gewShape, staIndex, basePart );

      final IRiverProfileNetwork network = createOrGetNetwork( data, riverId, riverName );
      final IProfileFeature profileFeature = createNewProfile( network );
      profileFeature.setName( name );
      profileFeature.setDescription( description );
      profileFeature.setSrsName( data.getCoordinateSystem() );
      profileFeature.setBigStation( station );

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
      final String message = String.format( Messages.getString( "ImportEwawiWorker.1" ), station.doubleValue() ); //$NON-NLS-1$
      final Status status = new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, message, e );
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
  public void updateWaterLevelFixation( final EwawiSta staIndex, final EwawiPro proIndex )
  {
    /* Nothing to do. */
  }

  @Override
  public void fireChangeEvents( )
  {
    /* Nothing to do. */
  }

  private IRiverProfileNetwork createOrGetNetwork( final EwawiImportData data, final String riverId, final String riverName )
  {
    final String networkName = String.format( "%s (%s)", riverName, riverId ); //$NON-NLS-1$

    final IFeatureBindingCollection<IRiverProfileNetwork> networks = m_profNetworkColl.getRiverProfileNetworks();
    for( final IRiverProfileNetwork network : networks )
    {
      if( networkName.equals( network.getName() ) )
        return network;
    }

    final IRiverProfileNetwork network = networks.addNew( IRiverProfileNetwork.QNAME );
    network.setName( networkName );
    network.setDescription( data.getProFile().getFile().getName() );
    m_addedNetworks.add( network );

    return network;
  }

  private IProfileFeature createNewProfile( final IRiverProfileNetwork network ) throws GMLSchemaException
  {
    final IProfileFeature newProfile = (IProfileFeature)FeatureHelper.addFeature( network, IRiverProfileNetwork.QNAME_PROP_RIVER_PROFILE, IProfileFeature.FEATURE_PROFILE );
    newProfile.setProfileType( IWspmTuhhConstants.PROFIL_TYPE_PASCHE );

    return newProfile;
  }

  public IRiverProfileNetworkCollection getProfNetworkColl( )
  {
    return m_profNetworkColl;
  }

  public IRiverProfileNetwork[] getNetworks( )
  {
    return m_addedNetworks.toArray( new IRiverProfileNetwork[] {} );
  }
}