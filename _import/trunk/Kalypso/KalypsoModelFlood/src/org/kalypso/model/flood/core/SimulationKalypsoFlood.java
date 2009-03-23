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
package org.kalypso.model.flood.core;

import java.io.File;
import java.math.BigDecimal;
import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.grid.CountGeoGridWalker;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.grid.PolygonGeoGridArea;
import org.kalypso.grid.VolumeGeoGridWalker;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.model.flood.binding.IFloodPolygon;
import org.kalypso.model.flood.binding.IFloodVolumePolygon;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.model.flood.binding.ITinReference;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.simspec.Modeldata;
import org.kalypso.simulation.core.util.SimulationUtilitites;
import org.kalypso.transformation.GeoTransformer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;

/**
 * @author Dejan Antanaskovic
 * 
 */
public class SimulationKalypsoFlood implements ISimulationSpecKalypsoFlood, ISimulation
{
  private static final double VOLUME_EPS = 1.0;

  private IFloodModel m_model = null;

  private ISimulationMonitor m_monitor;

  private void monitorAdd( final int chunk )
  {
    m_monitor.setProgress( m_monitor.getProgress() + chunk );
  }

  /**
   * @see org.kalypso.simulation.core.ISimulation#getSpezifikation()
   */
  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( "Specification_FloodCalculation.xml" );
  }

  /**
   * @see org.kalypso.simulation.core.ISimulation#run(java.io.File, org.kalypso.simulation.core.ISimulationDataProvider,
   *      org.kalypso.simulation.core.ISimulationResultEater, org.kalypso.simulation.core.ISimulationMonitor)
   */
  @Override
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    m_monitor = monitor;
    try
    {
      final GMLWorkspace modelWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( MODELSPEC_KALYPSOFLOOD.FLOOD_MODEL.name() ), null );
      m_model = (IFloodModel) modelWorkspace.getRootFeature().getAdapter( IFloodModel.class );

      final File eventsTmpDir = new File( tmpdir, MODELSPEC_KALYPSOFLOOD.EVENTS_BASE_FOLDER.getValue() );
      final IFeatureWrapperCollection<IRunoffEvent> events = m_model.getEvents();
      final int progressChunk = (95 - monitor.getProgress()) / (2 * events.size());
      for( final IRunoffEvent event : events )
      {
        if( event.isMarkedForProcessing() )
        {
          monitor.setMessage( "Berechne Flieﬂtiefen: " + event.getName() );
          processVolumes( event, progressChunk );
          final File eventFolder = new File( eventsTmpDir, event.getDataPath().toPortableString() );
          eventFolder.mkdirs();
          processEvent( eventFolder, event, progressChunk );
        }
        else
        {
          monitorAdd( progressChunk * 2 );
        }
      }
      final File tmpModel = File.createTempFile( "tmpFloodModel", ".gml", tmpdir );
      GmlSerializer.serializeWorkspace( tmpModel, modelWorkspace, "UTF-8" );
      resultEater.addResult( MODELSPEC_KALYPSOFLOOD.FLOOD_MODEL.name(), tmpModel );
      resultEater.addResult( MODELSPEC_KALYPSOFLOOD.EVENTS_BASE_FOLDER.name(), eventsTmpDir );
    }
    catch( final Exception e )
    {
      throw new SimulationException( e.getLocalizedMessage(), e );
    }
  }

  private void processVolumes( final IRunoffEvent event, final int progressChunk ) throws Exception
  {
    final ICoverageCollection terrainModel = m_model.getTerrainModel();
    final IFeatureWrapperCollection<IFloodPolygon> polygons = m_model.getPolygons();

    if( polygons.isEmpty() )
      return;
    
    /* Filter Volume Polygon */
    final List<IFloodVolumePolygon> volumePolygons = new ArrayList<IFloodVolumePolygon>( polygons.size() );
    for( final IFloodPolygon floodPolygon : polygons )
    {
      if( floodPolygon instanceof IFloodVolumePolygon && floodPolygon.getEvents().contains( event ) )
      {
        volumePolygons.add( (IFloodVolumePolygon) floodPolygon );
      }
    }

    // final SubMonitor progress = SubMonitor.convert( event, volumePolygons.size() );
    final int singleVolumeProgressChunk = progressChunk / polygons.size();
    for( final IFloodVolumePolygon floodVolumePolygon : volumePolygons )
    {
      processVolume( floodVolumePolygon, terrainModel );
      monitorAdd( singleVolumeProgressChunk );
    }
  }

  private void processVolume( final IFloodVolumePolygon volumePolygon, final ICoverageCollection terrainModel ) throws Exception
  {
    final BigDecimal volumeValue = volumePolygon.getVolume();
    if( volumeValue == null )
      return;

    final double volume = volumeValue.doubleValue();
    if( Double.isNaN( volume ) )
      return;

    final GM_Object volumeGmObject = volumePolygon.getArea();

    // Min/Max-WSP
    double minWsp = Double.MAX_VALUE;
    double maxWsp = -Double.MAX_VALUE;
    final CountGeoGridWalker countWalker = new CountGeoGridWalker( true );
    for( final ICoverage coverage : terrainModel )
    {
      final IGeoGrid geoGrid = GeoGridUtilities.toGrid( coverage );

      final String sourceCRS = geoGrid.getSourceCRS();
      final GeoTransformer transformer = new GeoTransformer( sourceCRS );
      final Geometry volumeGeometry = JTSAdapter.export( transformer.transform( volumeGmObject ) );

      final double cellSize = GeoGridUtilities.calcCellArea( geoGrid.getOffsetX(), geoGrid.getOffsetY() );
      final BigDecimal maxTerrain = geoGrid.getMax();

      /* Minimal waterlevel is the lowest point */
      // REMARK: could even be made higher, but than performance if bad, if result is near the minimum
      minWsp = Math.min( minWsp, geoGrid.getMin().doubleValue() );

      /* The maximum waterlevel per grid, is calculated by assuming that all cells have the maxmimum terrain value */
      geoGrid.getWalkingStrategy().walk( geoGrid, countWalker, new PolygonGeoGridArea( geoGrid, volumeGeometry ), new NullProgressMonitor() );
      final int numberOfCellsCoveringThePolgon = countWalker.getCount();

      final double coveredArea = numberOfCellsCoveringThePolgon * cellSize;
      final double heightFromZero = volume / coveredArea;
      final double maxGridWsp = heightFromZero + maxTerrain.doubleValue();

      maxWsp = Math.max( maxWsp, maxGridWsp );
      maxWsp += maxWsp / 2; // in order to improve performance, if waterlevel is near the maximum
    }

    final double wsp = searchWsp( volume, minWsp, maxWsp, terrainModel, volumeGmObject );
    volumePolygon.setWaterlevel( new BigDecimal( wsp ) );
  }

  private double searchWsp( final double targetVolume, final double minWsp, final double maxWsp, final ICoverageCollection terrainModel, final GM_Object volumeGmObject ) throws Exception
  {
    // Binary search within min/max; we start in the middle
    final double currentWsp = (maxWsp + minWsp) / 2;

    // System.out.println( "Current WSP: " + currentWsp );
    final double currentVolume = calcWsp( volumeGmObject, terrainModel, currentWsp );
    // System.out.println( "Current Volume: " + currentVolume );
    // System.out.println( "" );

    if( Math.abs( currentVolume - targetVolume ) < VOLUME_EPS )
      return currentWsp;

    if( currentVolume < targetVolume )
      return searchWsp( targetVolume, currentWsp, maxWsp, terrainModel, volumeGmObject );
    else
      return searchWsp( targetVolume, minWsp, currentWsp, terrainModel, volumeGmObject );
  }

  /**
   * Calculates the volume for a specific wsp value
   */
  private double calcWsp( final GM_Object volumeGmObject, final ICoverageCollection terrainModel, final double currentWsp ) throws Exception
  {
    final VolumeGeoGridWalker volumeWalker = new VolumeGeoGridWalker( currentWsp, false );

    double volume = 0.0;
    for( final ICoverage coverage : terrainModel )
    {
      final IGeoGrid grid = GeoGridUtilities.toGrid( coverage );

      final String sourceCRS = grid.getSourceCRS();
      final GeoTransformer transformer = new GeoTransformer( sourceCRS );
      final Geometry volumeGeometry = JTSAdapter.export( transformer.transform( volumeGmObject ) );

      grid.getWalkingStrategy().walk( grid, volumeWalker, new PolygonGeoGridArea( grid, volumeGeometry ), new NullProgressMonitor() );

      volume += volumeWalker.getVolume();
    }

    return volume;
  }

  private void processEvent( final File eventFolder, final IRunoffEvent event, final int progressChunk ) throws Exception
  {
    final ICoverageCollection terrainModel = m_model.getTerrainModel();
    // TODO: shouldn't we filter by the event?
    final IFeatureWrapperCollection<IFloodPolygon> polygons = m_model.getPolygons();

    /* check for existing result coverages */
    // TODO: existing result should be removed! IMPORTANT: also remove underlying grid-files
    // all this should be done before the calculation is started.
    final ICoverageCollection resultCoverages = event.createResultCoverages();
    
    /*
     * FIXME: in the interactive mode (workflow task handler, org.kalypso.model.flood.handlers.ProcessFloodModelHandler),
     * we are asking user to delete or to keep existing results; so, we cannot just throw an exception here!
     * What to do?
     */
    if( resultCoverages.size() != 0 )
    {
      // FIXME @dejan multiple processing of process chain leads to this exception // hotfix: clear list
      resultCoverages.clear(); // hotfix!!!
      // throw new IllegalStateException( "Event enth‰lt noch Ergebnisse: " + event.getName() );
    }
      

    // final IFolder eventFolder = eventsFolder.getFolder( event.getDataPath().toPortableString() );

    final int monitorChunk = progressChunk / terrainModel.size();
    for( final ICoverage terrainCoverage : terrainModel )
    {
      final IGeoGrid terrainGrid = GeoGridUtilities.toGrid( terrainCoverage );
      final IFeatureWrapperCollection<ITinReference> tins = event.getTins();

      final IGeoGrid diffGrid = new FloodDiffGrid( terrainGrid, tins, polygons, event );

      /* set destination: => event folder/results */
      // generate unique name for grid file
      final File resultsFolder = new File( eventFolder, "results" );
      resultsFolder.mkdir();
      final String uniqueFileName = FileUtilities.createNewUniqueFileName( "grid", ".ascbin", resultsFolder );

      final File outputCoverageFile = new File( resultsFolder, uniqueFileName );
      final String fileName = ISimulationSpecKalypsoFlood.CONST_COVERAGE_FILE_RELATIVE_PATH_PREFIX + event.getDataPath() + "/results/" + outputCoverageFile.getName();

      final ICoverage coverage = GeoGridUtilities.addCoverage( resultCoverages, diffGrid, outputCoverageFile, fileName, "image/bin", new NullProgressMonitor() );
      coverage.setName( "Flieﬂtiefen - " + terrainCoverage.getName() );

      final String desc = String.format( "erzeugt am: %1$te.%1$tm.%1$tY - %s", new Date(), terrainCoverage.getName() );
      coverage.setDescription( desc );

      terrainGrid.dispose();
      monitorAdd( monitorChunk );
    }
  }

  // TODO: does not work like that, as we need a feature wrapper collection later to query the list
  // private IFloodPolygon[] getPolygons( final IRunoffEvent event )
  // {
  // final IFeatureWrapperCollection<IFloodPolygon> polygons = m_model.getPolygons();
  //
  // final List<IFloodPolygon> filteredPolygons = new ArrayList<IFloodPolygon>();
  //
  // for( final IFloodPolygon floodPolygon : polygons )
  // {
  // if( floodPolygon.getEvents().contains( event ) )
  // filteredPolygons.add( floodPolygon );
  // }
  //
  // return filteredPolygons.toArray( new IFloodPolygon[filteredPolygons.size()] );
  // }

  public static final Modeldata getModeldata( )
  {
    final List<MODELSPEC_KALYPSOFLOOD> inputs = new ArrayList<MODELSPEC_KALYPSOFLOOD>();
    inputs.add( MODELSPEC_KALYPSOFLOOD.FLOOD_MODEL );
    inputs.add( MODELSPEC_KALYPSOFLOOD.GRID_FOLDER );
    inputs.add( MODELSPEC_KALYPSOFLOOD.EVENTS_BASE_FOLDER );

    final List<MODELSPEC_KALYPSOFLOOD> outputs = new ArrayList<MODELSPEC_KALYPSOFLOOD>();
    outputs.add( MODELSPEC_KALYPSOFLOOD.FLOOD_MODEL );
    outputs.add( MODELSPEC_KALYPSOFLOOD.EVENTS_BASE_FOLDER );

    return SimulationUtilitites.createModelData( FLOODSIMULATION_TYPEID, getMap( inputs ), true, getMap( outputs ), true );
  }

  private static final Map<String, String> getMap( final List<MODELSPEC_KALYPSOFLOOD> keys )
  {
    final Map<String, String> map = new HashMap<String, String>();
    for( final MODELSPEC_KALYPSOFLOOD key : keys )
    {
      map.put( key.name(), key.getValue() );
    }
    return map;
  }

}
