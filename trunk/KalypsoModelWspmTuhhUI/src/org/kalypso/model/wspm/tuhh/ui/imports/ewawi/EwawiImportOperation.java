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
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.ewawi.data.EwawiPlus;
import org.kalypso.model.wspm.ewawi.data.EwawiPro;
import org.kalypso.model.wspm.ewawi.data.EwawiProLine;
import org.kalypso.model.wspm.ewawi.data.EwawiSta;
import org.kalypso.model.wspm.ewawi.data.EwawiStaLine;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiPunktart;
import org.kalypso.model.wspm.ewawi.utils.EwawiException;
import org.kalypso.model.wspm.ewawi.utils.GewShape;
import org.kalypso.model.wspm.ewawi.utils.profiles.EwawiProfile;
import org.kalypso.model.wspm.ewawi.utils.profiles.EwawiProfilePart;
import org.kalypso.model.wspm.ewawi.utils.profiles.EwawiProfilePoint;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.observation.result.IComponent;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.geometry.SHPPoint;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree_impl.gml.binding.commons.Image;

/**
 * @author Holger Albert
 */
public class EwawiImportOperation implements ICoreRunnableWithProgress
{
  private final CommandableWorkspace m_workspace;

  private final TuhhWspmProject m_targetProject;

  private final EwawiImportData m_data;

  public EwawiImportOperation( final CommandableWorkspace workspace, final TuhhWspmProject targetProject, final EwawiImportData data )
  {
    m_workspace = workspace;
    m_targetProject = targetProject;
    m_data = data;
  }

  @Override
  public IStatus execute( IProgressMonitor monitor )
  {
    /* Monitor. */
    if( monitor == null )
      monitor = new NullProgressMonitor();

    try
    {
      /* Monitor. */
      monitor.beginTask( "Importing EWAWI+ profiles", 1000 );
      monitor.subTask( "Reading river shape..." );

      /* The river shape. */
      GewShape gewShape = null;

      /* Read the river shape. */
      final File shpFile = m_data.getRiverShapeData().getShpFile().getFile();
      if( shpFile != null )
      {
        gewShape = new GewShape( shpFile );
        gewShape.init();
      }

      /* Monitor. */
      monitor.worked( 200 );
      monitor.subTask( "Creating profiles..." );

      /* Get the ewawi data object. */
      final EwawiPlus ewawiData = m_data.getEwawiData();

      /* Get the ewawi sta und pro objects. */
      final EwawiSta staIndex = ewawiData.getStaIndex();
      final EwawiPro proIndex = ewawiData.getProIndex();

      /* Get the ewawi profiles. */
      final EwawiProfile[] profiles = proIndex.getProfiles();
      for( final EwawiProfile profile : profiles )
      {
        /* Create the wspm profile. */
        createNewProfile( gewShape, staIndex, profile );

        /* Monitor. */
        monitor.worked( 700 / profiles.length );
      }

      /* Monitor. */
      monitor.subTask( "Fireing events..." );

      /* Fire change events. */
      fireChangeEvents();

      /* Monitor. */
      monitor.worked( 100 );

      return new Status( IStatus.OK, KalypsoModelWspmTuhhUIPlugin.getID(), "Profiles successfully created." );
    }
    catch( final Exception ex )
    {
      return new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getID(), ex.getLocalizedMessage(), ex );
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }

  private IProfileFeature createNewProfile( final GewShape gewShape, final EwawiSta staIndex, final EwawiProfile ewawiProfile ) throws CoreException
  {
    final BigDecimal station = ewawiProfile.getStation();

    try
    {
      final EwawiProfilePart basePart = ewawiProfile.getBasePart();
      if( basePart == null )
        throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getID(), String.format( "No base profile found at station %.4f", station.doubleValue() ) ) );

      final String riverId = String.format( "%d", basePart.getGewKennzahl() );
      final String riverName = getRiverName( gewShape, basePart );
      final String[] photos = basePart.getPhotos( staIndex );

      final IProfileFeature profileFeature = m_targetProject.createNewProfile( riverName, m_data.isDirectionUpstreams() );
      profileFeature.getWater().setRefNr( riverId );
      profileFeature.setSrsName( m_data.getCoordinateSystem() );

      for( final String foto : photos )
      {
        final File proFile = m_data.getProFile().getFile();
        final File proParent = proFile.getParentFile();
        final File fotoFile = new File( proParent, foto );
        final URI fotoUrl = fotoFile.toURI();

        final Image image = profileFeature.addImage( fotoUrl );
        image.setName( FilenameUtils.removeExtension( foto ) );
      }

      final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( IWspmTuhhConstants.PROFIL_TYPE_PASCHE );
      final IComponent hochwertComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT );
      final IComponent rechtswertComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
      final IComponent hoeheComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
      final IComponent breiteComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );

      final IProfil profil = profileFeature.getProfil();
      profil.addPointProperty( hochwertComponent );
      profil.addPointProperty( rechtswertComponent );
      profil.addPointProperty( hoeheComponent );
      profil.addPointProperty( breiteComponent );

      final EwawiProLine[] proLines = basePart.getProLines();
      for( final EwawiProLine proLine : proLines )
      {
        final EwawiProfilePoint profilePoint = createProfilePoint( staIndex, proLine );
        final SHPPoint shape = profilePoint.getShape();

        final double hochwert = shape.getY();
        final double rechtswert = shape.getX();
        final double hoehe = profilePoint.getHoehe().doubleValue();
        final double breite = profilePoint.getBreite().doubleValue();

        final IProfileRecord record = profil.createProfilPoint();
        record.setValue( record.indexOfProperty( hochwertComponent ), hochwert );
        record.setValue( record.indexOfProperty( rechtswertComponent ), rechtswert );
        record.setValue( record.indexOfProperty( hoeheComponent ), hoehe );
        record.setValue( record.indexOfProperty( breiteComponent ), breite );

        // TODO Set the values...

        profil.addPoint( record );
      }

      return profileFeature;
    }
    catch( final GMLSchemaException | DBaseException | EwawiException e )
    {
      final String message = String.format( "Unable to create profile at %.4f", station.doubleValue() );
      final Status status = new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getID(), message, e );
      throw new CoreException( status );
    }
  }

  private String getRiverName( final GewShape gewShape, final EwawiProfilePart basePart ) throws DBaseException
  {
    if( gewShape == null )
      return "Undefiniert";

    final Long gewKennzahl = basePart.getGewKennzahl();
    if( gewKennzahl == null )
      return "Undefiniert";

    final String name = gewShape.getName( gewKennzahl );
    if( name == null )
      return "Undefiniert";

    return name;
  }

  private EwawiProfilePoint createProfilePoint( final EwawiSta staIndex, final EwawiProLine proLine ) throws EwawiException
  {
    final EwawiStaLine leftFixPoint = staIndex.findFixPoint( proLine.getObjectArt(), EwawiPunktart._1, proLine.getGewKennzahl(), proLine.getStation() );
    final EwawiStaLine rightFixPoint = staIndex.findFixPoint( proLine.getObjectArt(), EwawiPunktart._2, proLine.getGewKennzahl(), proLine.getStation() );
    if( leftFixPoint == null || rightFixPoint == null )
      throw new EwawiException( "Einer der Festpunkte wurde nicht gefunden." );

    return new EwawiProfilePoint( leftFixPoint, rightFixPoint, proLine );
  }

  private void fireChangeEvents( )
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
}