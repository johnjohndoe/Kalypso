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
import java.util.List;

import org.apache.commons.io.FilenameUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetwork;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetworkCollection;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.ewawi.data.EwawiPro;
import org.kalypso.model.wspm.ewawi.data.EwawiSta;
import org.kalypso.model.wspm.ewawi.utils.EwawiException;
import org.kalypso.model.wspm.ewawi.utils.GewShape;
import org.kalypso.model.wspm.ewawi.utils.profiles.EwawiProfile;
import org.kalypso.model.wspm.ewawi.utils.profiles.EwawiProfilePart;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.imports.ewawi.AbstractEwawiWorker;
import org.kalypso.model.wspm.tuhh.ui.imports.ewawi.EwawiImportData;
import org.kalypso.model.wspm.tuhh.ui.imports.ewawi.EwawiProfilePointCreator;
import org.kalypso.model.wspm.tuhh.ui.imports.ewawi.EwawiProfilePointMarkerCreator;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree_impl.gml.binding.commons.Image;

/**
 * @author Holger Albert
 */
public class ImportEwawiWorker extends AbstractEwawiWorker
{
  private final IRiverProfileNetworkCollection m_profNetworkColl;

  private final List<Feature> m_terrainModelAdds;

  public ImportEwawiWorker( final IRiverProfileNetworkCollection profNetworkColl, final List<Feature> terrainModelAdds )
  {
    m_profNetworkColl = profNetworkColl;
    m_terrainModelAdds = terrainModelAdds;
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
        throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getID(), String.format( "No base profile found at station %.4f", station.doubleValue() ) ) );

      final String name = getName( staIndex, basePart );
      final String description = getDescription( staIndex, basePart );
      final String[] photos = basePart.getPhotos( staIndex );

      final String riverId = getRiverId( basePart );
      final String riverName = getRiverName( data, gewShape, basePart );
      final GM_Curve riverGeometry = getRiverGeometry( data, gewShape, basePart );

      // TODO Create the profile...
      final IProfileFeature profileFeature = null;
      profileFeature.setName( name );
      profileFeature.setDescription( description );
      profileFeature.setSrsName( data.getCoordinateSystem() );
      profileFeature.setBigStation( station );

      // TODO Update the profile network with the river values...

      for( final String foto : photos )
      {
        final File proFile = data.getProFile().getFile();
        final File proParent = proFile.getParentFile();
        final File fotoFile = new File( proParent, foto );
        final URI fotoUrl = fotoFile.toURI();

        final Image image = profileFeature.addImage( fotoUrl );
        image.setName( FilenameUtils.removeExtension( foto ) );
      }

      final EwawiProfilePointCreator pointCreator = new EwawiProfilePointCreator( staIndex, basePart, profileFeature );
      pointCreator.createProfilePoints();

      return profileFeature;
    }
    catch( final DBaseException | EwawiException e )
    {
      final String message = String.format( "Unable to create profile at %.4f", station.doubleValue() );
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
  public void updateWaterLevelFixation( final EwawiSta staIndex, final EwawiPro proIndex )
  {
    /* Nothing to do. */
  }

  @Override
  public void fireChangeEvents( )
  {
    final IFeatureBindingCollection<IRiverProfileNetwork> networks = m_profNetworkColl.getRiverProfileNetworks();
    final IRiverProfileNetwork[] nws = networks.toArray( new IRiverProfileNetwork[] {} );
    final GMLWorkspace workspace = m_profNetworkColl.getWorkspace();
    workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, m_profNetworkColl, nws, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
  }

  public IRiverProfileNetworkCollection getProfNetworkColl( )
  {
    return m_profNetworkColl;
  }

  public List<Feature> getTerrainModelAdds( )
  {
    return m_terrainModelAdds;
  }

  public IRiverProfileNetwork getNetwork( )
  {
    // TODO
    return null;
  }
}