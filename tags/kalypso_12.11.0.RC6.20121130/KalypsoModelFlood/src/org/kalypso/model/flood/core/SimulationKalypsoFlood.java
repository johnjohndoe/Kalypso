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
import java.io.IOException;
import java.math.BigDecimal;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.grid.BinaryGeoGridReader;
import org.kalypso.grid.CountGeoGridWalker;
import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.grid.IGeoWalkingStrategy;
import org.kalypso.grid.OptimizedGeoGridWalkingStrategy;
import org.kalypso.grid.RectifiedGridCoverageGeoGrid;
import org.kalypso.grid.VolumeGeoGridWalker;
import org.kalypso.grid.areas.PolygonGeoGridArea;
import org.kalypso.grid.parallel.SequentialBinaryGeoGridReader;
import org.kalypso.model.flood.KalypsoModelFloodPlugin;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.model.flood.binding.IFloodPolygon;
import org.kalypso.model.flood.binding.IFloodVolumePolygon;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.model.flood.binding.ITinReference;
import org.kalypso.model.flood.i18n.Messages;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.SimulationMonitorAdaptor;
import org.kalypsodeegree.graphics.transformation.GeoTransformUtils;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;

/**
 * @author Dejan Antanaskovic
 */
public class SimulationKalypsoFlood implements ISimulation
{
  public static String TYPEID = "KalypsoFloodSimulation"; //$NON-NLS-1$

  public static String CONST_COVERAGE_FILE_RELATIVE_PATH_PREFIX = "../events/"; //$NON-NLS-1$

  public static final String INPUT_FLOOD_MODEL = "FLOOD_MODEL"; //$NON-NLS-1$

  public static final String INPUT_GRID_FOLDER = "GRID_FOLDER"; //$NON-NLS-1$

  public static final String INPUT_EVENTS_BASE_FOLDER = "EVENTS_BASE_FOLDER"; //$NON-NLS-1$

  public static final String OUTPUT_FLOOD_MODEL = "FLOOD_MODEL"; //$NON-NLS-1$

  public static final String OUTPUT_EVENTS_BASE_FOLDER = "EVENTS_BASE_FOLDER"; //$NON-NLS-1$

  private static final String STR_EREIGNIS_xS = Messages.getString( "org.kalypso.model.flood.core.SimulationKalypsoFlood.7" ); //$NON-NLS-1$

  private static final String STR_EREIGNIS_xS_VOLUMENERMITTLUNG_xS = STR_EREIGNIS_xS + Messages.getString( "org.kalypso.model.flood.core.SimulationKalypsoFlood.8" ); //$NON-NLS-1$

  private static final String STR_EREIGNISE_xS_VOLUMENERMITTLUNG_xS_COVERAGE_xS = STR_EREIGNIS_xS_VOLUMENERMITTLUNG_xS + Messages.getString( "org.kalypso.model.flood.core.SimulationKalypsoFlood.9" ); //$NON-NLS-1$

  private static final String STR_EREIGNIS_xS_FLIESSTIEFENERMITTLUNG_xS = STR_EREIGNIS_xS + Messages.getString( "org.kalypso.model.flood.core.SimulationKalypsoFlood.0" ); //$NON-NLS-1$

  private static final double VOLUME_EPS = 1.0;

  private static final double WSP_EPS = 0.00001; // very small, as in order to reach

  /**
   * @see org.kalypso.simulation.core.ISimulation#getSpezifikation()
   */
  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( "Specification_FloodCalculation.xml" ); //$NON-NLS-1$
  }

  @Override
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    final URL gmlURL = (URL)inputProvider.getInputForID( INPUT_FLOOD_MODEL );

    final IProgressMonitor simulationMonitorAdaptor = new SimulationMonitorAdaptor( monitor );

    final File eventsTmpDir = new File( tmpdir, "events" ); //$NON-NLS-1$

    try
    {
      final GMLWorkspace workspace = runSimulation( gmlURL, eventsTmpDir, simulationMonitorAdaptor );
      final File tmpModel = File.createTempFile( "tmpFloodModel", ".gml", tmpdir ); //$NON-NLS-1$ //$NON-NLS-2$
      GmlSerializer.serializeWorkspace( tmpModel, workspace, "UTF-8" ); //$NON-NLS-1$
      resultEater.addResult( OUTPUT_FLOOD_MODEL, tmpModel );
      resultEater.addResult( OUTPUT_EVENTS_BASE_FOLDER, eventsTmpDir );
    }
    catch( final SimulationException e )
    {
      throw e;
    }
    catch( final Exception e )
    {
      throw new SimulationException( e.getLocalizedMessage(), e );
    }
  }

  private GMLWorkspace runSimulation( final URL gmlURL, final File eventsTmpDir, final IProgressMonitor monitor ) throws SimulationException
  {
    try
    {
      final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.model.flood.core.SimulationKalypsoFlood.15" ), 1000 ); //$NON-NLS-1$

      progress.subTask( Messages.getString( "org.kalypso.model.flood.core.SimulationKalypsoFlood.16" ) ); //$NON-NLS-1$
      final GMLWorkspace modelWorkspace = GmlSerializer.createGMLWorkspace( gmlURL, null );
      final IFloodModel model = (IFloodModel)modelWorkspace.getRootFeature().getAdapter( IFloodModel.class );
      ProgressUtilities.worked( monitor, 100 );

      /* Find events to be calculated */
      final IFeatureBindingCollection<IRunoffEvent> events = model.getEvents();
      final List<IRunoffEvent> markedEvents = new ArrayList<>();
      for( final IRunoffEvent event : events )
      {
        if( event.isMarkedForProcessing() )
          markedEvents.add( event );
      }

      if( markedEvents.size() == 0 )
        throw new CoreException( new Status( IStatus.WARNING, KalypsoModelFloodPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.model.flood.core.SimulationKalypsoFlood.17" ) ) ); //$NON-NLS-1$

      progress.setWorkRemaining( events.size() * 2 );
      for( final IRunoffEvent event : markedEvents )
      {
        progress.subTask( String.format( STR_EREIGNIS_xS, event.getName() ) );
        /* final IStatus processVolumes = */processVolumes( model, event, progress.newChild( 1 ) );
        // TODO: collect stats and log to file and/or present to user
        final File eventFolder = new File( eventsTmpDir, event.getDataPath().toPortableString() );
        eventFolder.mkdirs();

        processEvent( model, eventFolder, event, progress.newChild( 1 ) );
      }

      return modelWorkspace;
    }
    catch( final CoreException e )
    {
      throw new SimulationException( e.getStatus().getMessage(), e );
    }
    catch( final SimulationException e )
    {
      throw e;

    }
    catch( final Exception e )
    {
      throw new SimulationException( e.getLocalizedMessage(), e );
    }
  }

  private IStatus processVolumes( final IFloodModel model, final IRunoffEvent event, final IProgressMonitor monitor ) throws Exception
  {
    final ICoverageCollection terrainModel = model.getTerrainModel();
    final IFeatureBindingCollection<IFloodPolygon> polygons = model.getPolygons();

    /* Filter Volume Polygon */
    final List<IFloodVolumePolygon> volumePolygons = new ArrayList<>( polygons.size() );
    for( final IFloodPolygon floodPolygon : polygons )
    {
      if( floodPolygon instanceof IFloodVolumePolygon && floodPolygon.getEvents().contains( event ) )
        volumePolygons.add( (IFloodVolumePolygon)floodPolygon );
    }

    final Collection<IStatus> stati = new ArrayList<>();
    final SubMonitor progress = SubMonitor.convert( monitor, volumePolygons.size() );
    for( final IFloodVolumePolygon floodVolumePolygon : volumePolygons )
    {
      progress.subTask( String.format( STR_EREIGNIS_xS_VOLUMENERMITTLUNG_xS, event.getName(), floodVolumePolygon.getName() ) );
      final IStatus result = processVolume( event, floodVolumePolygon, terrainModel, progress.newChild( 1 ) );
      if( !result.isOK() )
      {
        KalypsoModelFloodPlugin.getDefault().getLog().log( result );
        stati.add( result );
      }
    }

    if( stati.size() == 0 )
      return Status.OK_STATUS;

    final IStatus[] children = stati.toArray( new IStatus[stati.size()] );
    return new MultiStatus( KalypsoModelFloodPlugin.PLUGIN_ID, -1, children, Messages.getString( "org.kalypso.model.flood.core.SimulationKalypsoFlood.1" ), null ); //$NON-NLS-1$
  }

  private IStatus processVolume( final IRunoffEvent event, final IFloodVolumePolygon volumePolygon, final ICoverageCollection terrainCoverages, final IProgressMonitor monitor ) throws SimulationException, GeoGridException, GM_Exception
  {
    final IFeatureBindingCollection<ICoverage> terrainCoveragesList = terrainCoverages.getCoverages();
    final SubMonitor progress = SubMonitor.convert( monitor, terrainCoveragesList.size() * 2 );

    final BigDecimal volumeValue = volumePolygon.getVolume();
    final String volumeName = volumePolygon.getName();

    final String noValueMsg = String.format( Messages.getString( "org.kalypso.model.flood.core.SimulationKalypsoFlood.2" ), volumeName ); //$NON-NLS-1$
    final IStatus noValueStatus = new Status( IStatus.WARNING, KalypsoModelFloodPlugin.PLUGIN_ID, noValueMsg );

    if( volumeValue == null )
      return noValueStatus;

    final double volume = volumeValue.doubleValue();
    if( Double.isNaN( volume ) )
      return noValueStatus;

    final GM_Object volumeGmObject = volumePolygon.getArea();

    // Min/Max-WSP
    double minWsp = Double.POSITIVE_INFINITY;
    double maxWsp = Double.NEGATIVE_INFINITY;
    double maxVol = Double.NaN;
    final CountGeoGridWalker countWalker = new CountGeoGridWalker( true );
    for( final ICoverage coverage : terrainCoveragesList )
    {
      progress.subTask( String.format( STR_EREIGNISE_xS_VOLUMENERMITTLUNG_xS_COVERAGE_xS, event.getName(), volumeName, coverage.getName() ) );

      IGeoGrid geoGrid = GeoGridUtilities.toGrid( coverage );

      // try optimized binary grid reader
      if( geoGrid instanceof RectifiedGridCoverageGeoGrid )
      {
        try
        {
          geoGrid = new BinaryGeoGridReader( geoGrid, ((RectifiedGridCoverageGeoGrid)geoGrid).getGridURL() );
        }
        catch( final IOException e )
        {
          e.printStackTrace();
        }
      }

      final String sourceCRS = geoGrid.getSourceCRS();
      final Geometry volumeGeometry = JTSAdapter.export( GeoTransformUtils.transformQuiet( volumeGmObject, sourceCRS ) );

      final double cellSize = GeoGridUtilities.calcCellArea( geoGrid.getOffsetX(), geoGrid.getOffsetY() );
      final BigDecimal maxTerrain = geoGrid.getMax();
      final BigDecimal minTerrain = geoGrid.getMax();
      /* Minimal waterlevel is the lowest point */
      // REMARK: could even be made higher, but than performance if bad, if result is near the minimum
      minWsp = Math.min( minWsp, geoGrid.getMin().doubleValue() );

      /* The maximum waterlevel per grid, is calculated by assuming that all cells have the maxmimum terrain value */
      final IGeoWalkingStrategy strategy = new OptimizedGeoGridWalkingStrategy();
      strategy.walk( geoGrid, countWalker, new PolygonGeoGridArea( geoGrid, volumeGeometry ), progress.newChild( 1, SubMonitor.SUPPRESS_ALL_LABELS ) );
      final int numberOfCellsCoveringThePolgon = countWalker.getCount();
      if( numberOfCellsCoveringThePolgon > 0 )
      {
        final double coveredArea = numberOfCellsCoveringThePolgon * cellSize;
        final double heightFromZero = volume / coveredArea;
        final double maxGridWsp = heightFromZero + maxTerrain.doubleValue();

        maxWsp = Math.max( maxWsp, maxGridWsp );
        maxWsp += maxWsp / 2; // in order to improve performance, if waterlevel is near the maximum

        maxVol += coveredArea * (maxWsp - minTerrain.doubleValue());
      }
    }

    if( Double.isNaN( minWsp ) || Double.isInfinite( minWsp ) )
      throw new SimulationException( Messages.getString( "org.kalypso.model.flood.core.SimulationKalypsoFlood.18" ) ); //$NON-NLS-1$
    if( Double.isNaN( maxWsp ) || Double.isInfinite( maxWsp ) )
      throw new SimulationException( Messages.getString( "org.kalypso.model.flood.core.SimulationKalypsoFlood.19" ) ); //$NON-NLS-1$

    progress.setWorkRemaining( 100 );

    // Calculate maxVol if unable to estimate it
    // if( Double.isNaN( maxVol ) )
    // maxVol = calcVolume( volumeGmObject, terrainCoverages, maxWsp, progress.newChild( 10 ) );

    final VolumeResult result = searchWsp( volume, minWsp, 0.0, maxWsp, maxVol, terrainCoverages, volumeGmObject, progress.newChild( 100 ) );
    final double wsp = result.m_wsp;
    if( result.m_status.matches( IStatus.ERROR ) )
    {
      volumePolygon.setWaterlevel( null );
      throw new SimulationException( result.m_status.getMessage() );
    }

    volumePolygon.setWaterlevel( new BigDecimal( wsp ) );

    return result.m_status;
  }

  private VolumeResult searchWsp( final double targetVolume, final double minWsp, final double minVol, final double maxWsp, final double maxVol, final ICoverageCollection terrainModel, final GM_Object volumeGmObject, final IProgressMonitor monitor ) throws SimulationException
  {
    // Binary search within min/max; we start in the middle
    final double currentWsp = (maxWsp + minWsp) / 2;

    // HM, check if we could not use Newton iteration to reduce number of iterations.
    // double currentWsp = Double.NaN;
    // try
    // {
    // final LinearEquation linearEquation = new LinearEquation( minWsp, minVol, maxWsp, maxVol );
    // currentWsp = linearEquation.computeX( targetVolume );
    // }
    // catch( SameXValuesException e )
    // {
    // e.printStackTrace();
    // return Double.NaN;
    // }

    final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.model.flood.core.SimulationKalypsoFlood.21" ), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$

    if( Double.isNaN( currentWsp ) || Double.isInfinite( currentWsp ) )
      return new VolumeResult( new Status( IStatus.ERROR, KalypsoModelFloodPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.model.flood.core.SimulationKalypsoFlood.20" ) ), Double.NaN ); //$NON-NLS-1$

    if( Math.abs( currentWsp - minWsp ) < WSP_EPS )
    {
      final double volumeDif = Math.abs( minVol - targetVolume );
      final String msg = String.format( Messages.getString( "org.kalypso.model.flood.core.SimulationKalypsoFlood.3" ), volumeDif ); //$NON-NLS-1$
      return new VolumeResult( new Status( IStatus.WARNING, KalypsoModelFloodPlugin.PLUGIN_ID, msg ), minWsp );
    }

    if( Math.abs( currentWsp - maxWsp ) < WSP_EPS )
    {
      final double volumeDif = Math.abs( maxVol - targetVolume );
      final String msg = String.format( Messages.getString( "org.kalypso.model.flood.core.SimulationKalypsoFlood.4" ), volumeDif ); //$NON-NLS-1$
      return new VolumeResult( new Status( IStatus.WARNING, KalypsoModelFloodPlugin.PLUGIN_ID, msg ), maxWsp );
    }

    // TODO: better condition to avoid endless-loop. Either count loops or stop, if currentWsp is too near to min/max

    // System.out.println( "Current WSP: " + currentWsp );
    final double currentVolume = calcVolume( volumeGmObject, terrainModel, currentWsp, progress.newChild( 10 ) );
    // System.out.println( "Current Volume: " + currentVolume );
    // System.out.println( "" );

    final double targetDiff = Math.abs( currentVolume - targetVolume );
    progress.subTask( Messages.getString( "org.kalypso.model.flood.core.SimulationKalypsoFlood.22", currentWsp, targetDiff ) ); //$NON-NLS-1$

    final String msg = String.format( "min: (%.3f/%.1f) max: (%.3f/%.1f) current: (%.3f/%.1f)", minWsp, minVol, maxWsp, maxVol, currentWsp, currentVolume ); //$NON-NLS-1$
    System.out.println( msg );

    if( targetDiff < VOLUME_EPS )
      return new VolumeResult( Status.OK_STATUS, maxWsp );

    // Depending on sign, search in upper or lower half
    if( currentVolume < targetVolume )
      return searchWsp( targetVolume, currentWsp, currentVolume, maxWsp, maxVol, terrainModel, volumeGmObject, progress.newChild( 100 ) );
    else
      return searchWsp( targetVolume, minWsp, minVol, currentWsp, currentVolume, terrainModel, volumeGmObject, progress.newChild( 100 ) );
  }

  /**
   * Calculates the volume for a specific wsp value
   */
  private double calcVolume( final GM_Object volumeGmObject, final ICoverageCollection terrainCollection, final double currentWsp, final IProgressMonitor monitor ) throws SimulationException
  {
    final IFeatureBindingCollection<ICoverage> terrainCoverages = terrainCollection.getCoverages();
    final SubMonitor progress = SubMonitor.convert( monitor, terrainCoverages.size() );

    try
    {
      final VolumeGeoGridWalker volumeWalker = new VolumeGeoGridWalker( currentWsp, false );

      double volume = 0.0;
      for( final ICoverage coverage : terrainCoverages )
      {
        IGeoGrid geoGrid = GeoGridUtilities.toGrid( coverage );

        // try optimized binary grid reader
        if( geoGrid instanceof RectifiedGridCoverageGeoGrid )
        {
          try
          {
            geoGrid = new BinaryGeoGridReader( geoGrid, ((RectifiedGridCoverageGeoGrid)geoGrid).getGridURL() );
          }
          catch( final IOException e )
          {
            e.printStackTrace();
          }
        }

        final String sourceCRS = geoGrid.getSourceCRS();
        final Geometry volumeGeometry = JTSAdapter.export( GeoTransformUtils.transformQuiet( volumeGmObject, sourceCRS ) );
        final IGeoWalkingStrategy strategy = new OptimizedGeoGridWalkingStrategy();
        strategy.walk( geoGrid, volumeWalker, new PolygonGeoGridArea( geoGrid, volumeGeometry ), progress.newChild( 1, SubMonitor.SUPPRESS_ALL_LABELS ) );

        volume += volumeWalker.getVolume();
      }

      return volume;
    }
    catch( final GeoGridException e )
    {
      throw new SimulationException( Messages.getString( "org.kalypso.model.flood.core.SimulationKalypsoFlood.23", e.getLocalizedMessage() ), e ); //$NON-NLS-1$
    }
    catch( final GM_Exception e )
    {
      throw new SimulationException( Messages.getString( "org.kalypso.model.flood.core.SimulationKalypsoFlood.24", e.getLocalizedMessage() ), e ); //$NON-NLS-1$
    }
  }

  private void processEvent( final IFloodModel model, final File eventFolder, final IRunoffEvent event, final IProgressMonitor monitor ) throws Exception
  {
    final ICoverageCollection terrainModel = model.getTerrainModel();
    final IFeatureBindingCollection<ICoverage> terrainCoverages = terrainModel.getCoverages();
    final SubMonitor progress = SubMonitor.convert( monitor, terrainCoverages.size() );

    // TODO: shouldn't we filter by the event?
    final IFeatureBindingCollection<IFloodPolygon> polygons = model.getPolygons();

    /* check for existing result coverages */
    // TODO: existing result should be removed! IMPORTANT: also remove underlying grid-files
    // all this should be done before the calculation is started.
    final ICoverageCollection resultCoverages = event.createResultCoverages();

    /*
     * FIXME: in the interactive mode (workflow task handler,
     * org.kalypso.model.flood.handlers.ProcessFloodModelHandler), we are asking user to delete or to keep existing
     * results; so, we cannot just throw an exception here! What to do?
     */
    final IFeatureBindingCollection<ICoverage> resultCoveragesList = resultCoverages.getCoverages();
    if( resultCoveragesList.size() != 0 )
    {
      // FIXME @dejan multiple processing of process chain leads to this exception // hotfix: clear list
      resultCoveragesList.clear(); // hotfix!!!
      // throw new IllegalStateException( "Event enth‰lt noch Ergebnisse: " + event.getName() );
    }

    for( int i = 0; i < terrainCoverages.size(); i++ )
    {
      final ICoverage terrainCoverage = terrainCoverages.get( i );
      progress.subTask( String.format( STR_EREIGNIS_xS_FLIESSTIEFENERMITTLUNG_xS, event.getName(), terrainCoverage.getName() ) );

      final RectifiedGridCoverageGeoGrid inputGrid = (RectifiedGridCoverageGeoGrid)GeoGridUtilities.toGrid( terrainCoverage );
      final IFeatureBindingCollection<ITinReference> tins = event.getTins();

      // create sequential grid reader
      final SequentialBinaryGeoGridReader inputGridReader = new FloodDiffGrid( inputGrid, inputGrid.getGridURL(), tins, polygons, event );

      /* set destination: => event folder/results */
      // generate unique name for grid file
      final File resultsFolder = new File( eventFolder, "results" ); //$NON-NLS-1$
      resultsFolder.mkdir();
      final File outputCoverageFile = new File( resultsFolder, String.format( "HQ%d_%02d.bin", event.getReturnPeriod(), i ) ); //$NON-NLS-1$

      final String fileName = CONST_COVERAGE_FILE_RELATIVE_PATH_PREFIX + event.getDataPath() + "/results/" + outputCoverageFile.getName();//$NON-NLS-1$

      final ICoverage coverage = GeoGridUtilities.addCoverage( resultCoverages, inputGridReader, 2, outputCoverageFile, fileName, "image/bin", progress.newChild( 1 ) ); //$NON-NLS-1$

      coverage.setName( Messages.getString( "org.kalypso.model.flood.core.SimulationKalypsoFlood.10", terrainCoverage.getName() ) ); //$NON-NLS-1$

      final String desc = Messages.getString( "org.kalypso.model.flood.core.SimulationKalypsoFlood.11", new Date(), terrainCoverage.getName() ); //$NON-NLS-1$
      coverage.setDescription( desc );

      inputGridReader.close();
      inputGrid.close();
    }
  }

  private static class VolumeResult
  {
    public final IStatus m_status;

    public final double m_wsp;

    public VolumeResult( final IStatus status, final double wsp )
    {
      m_status = status;
      m_wsp = wsp;
    }
  }
}