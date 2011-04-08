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
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.grid.CountGeoGridWalker;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.grid.RectifiedGridCoverageGeoGrid;
import org.kalypso.grid.SequentialBinaryGeoGridReader;
import org.kalypso.grid.VolumeGeoGridWalker;
import org.kalypso.grid.areas.PolygonGeoGridArea;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.model.flood.binding.IFloodPolygon;
import org.kalypso.model.flood.binding.IFloodVolumePolygon;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.model.flood.binding.ITinReference;
import org.kalypso.model.flood.i18n.Messages;
import org.kalypso.transformation.transformer.GeoTransformerFactory;
import org.kalypso.transformation.transformer.IGeoTransformer;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;

/**
 * @author Gernot Belger
 * @author Thomas Jung
 * 
 */
public class FloodModelProcess
{
  private static final double VOLUME_EPS = 1.0;

  private final IFloodModel m_model;

  private final IRunoffEvent[] m_events;

  public FloodModelProcess( final IFloodModel model, final IRunoffEvent[] events )
  {
    m_model = model;
    m_events = events;
  }

  public IStatus process( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.model.flood.core.FloodModelProcess.0" ), m_events.length * 2 ); //$NON-NLS-1$

    for( final IRunoffEvent event : m_events )
    {
      try
      {
        progress.subTask( event.getName() );
        processVolumes( event, progress.newChild( 1, SubMonitor.SUPPRESS_NONE ) );
        processEvent( event, progress.newChild( 1, SubMonitor.SUPPRESS_NONE ) );
      }
      catch( final CoreException e )
      {
        throw e;
      }
      catch( final Exception e )
      {
        throw new InvocationTargetException( e );
      }
    }

    return Status.OK_STATUS;
  }

  private void processVolumes( final IRunoffEvent event, final IProgressMonitor monitor ) throws Exception
  {
    final ICoverageCollection terrainModel = m_model.getTerrainModel();
    final IFeatureWrapperCollection<IFloodPolygon> polygons = m_model.getPolygons();

    /* Filter Volume Polygon */
    final List<IFloodVolumePolygon> volumePolygons = new ArrayList<IFloodVolumePolygon>( polygons.size() );
    for( final IFloodPolygon floodPolygon : polygons )
    {
      if( floodPolygon instanceof IFloodVolumePolygon && floodPolygon.getEvents().contains( event ) )
      {
        volumePolygons.add( (IFloodVolumePolygon) floodPolygon );
      }
    }

    final SubMonitor progress = SubMonitor.convert( monitor, volumePolygons.size() );
    for( final IFloodVolumePolygon floodVolumePolygon : volumePolygons )
    {
      processVolume( floodVolumePolygon, terrainModel, progress.newChild( 1 ) );
    }
  }

  private void processVolume( final IFloodVolumePolygon volumePolygon, final ICoverageCollection terrainModel, final IProgressMonitor monitor ) throws Exception
  {
    final BigDecimal volumeValue = volumePolygon.getVolume();
    if( volumeValue == null )
    {
      return;
    }

    final double volume = volumeValue.doubleValue();
    if( Double.isNaN( volume ) )
    {
      return;
    }

    final GM_Object volumeGmObject = volumePolygon.getArea();

    // Min/Max-WSP
    double minWsp = Double.MAX_VALUE;
    double maxWsp = -Double.MAX_VALUE;
    final CountGeoGridWalker countWalker = new CountGeoGridWalker( true );
    for( final ICoverage coverage : terrainModel.getCoverages() )
    {
      final IGeoGrid geoGrid = GeoGridUtilities.toGrid( coverage );

      final String sourceCRS = geoGrid.getSourceCRS();
      final IGeoTransformer transformer = GeoTransformerFactory.getGeoTransformer( sourceCRS );
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

    ProgressUtilities.done( monitor );
  }

  private double searchWsp( final double targetVolume, final double minWsp, final double maxWsp, final ICoverageCollection terrainModel, final GM_Object volumeGmObject ) throws Exception
  {
    // Binary search within min/max; we start in the middle
    final double currentWsp = (maxWsp + minWsp) / 2;

    final double currentVolume = calcWsp( volumeGmObject, terrainModel, currentWsp );

    if( Math.abs( currentVolume - targetVolume ) < VOLUME_EPS )
    {
      return currentWsp;
    }

    if( currentVolume < targetVolume )
    {
      return searchWsp( targetVolume, currentWsp, maxWsp, terrainModel, volumeGmObject );
    }
    else
    {
      return searchWsp( targetVolume, minWsp, currentWsp, terrainModel, volumeGmObject );
    }
  }

  /**
   * Calculates the volume for a specific wsp value
   */
  private double calcWsp( final GM_Object volumeGmObject, final ICoverageCollection terrainModel, final double currentWsp ) throws Exception
  {
    final VolumeGeoGridWalker volumeWalker = new VolumeGeoGridWalker( currentWsp, false );

    double volume = 0.0;
    for( final ICoverage coverage : terrainModel.getCoverages() )
    {
      final IGeoGrid grid = GeoGridUtilities.toGrid( coverage );

      final String sourceCRS = grid.getSourceCRS();
      final IGeoTransformer transformer = GeoTransformerFactory.getGeoTransformer( sourceCRS );
      final Geometry volumeGeometry = JTSAdapter.export( transformer.transform( volumeGmObject ) );

      grid.getWalkingStrategy().walk( grid, volumeWalker, new PolygonGeoGridArea( grid, volumeGeometry ), new NullProgressMonitor() );

      volume += volumeWalker.getVolume();
    }

    return volume;
  }

  private void processEvent( final IRunoffEvent event, final IProgressMonitor monitor ) throws Exception
  {
    final ICoverageCollection terrainModel = m_model.getTerrainModel();
    IFeatureBindingCollection<ICoverage> terrainCoverages = terrainModel.getCoverages();
    final SubMonitor progress = SubMonitor.convert( monitor, terrainCoverages.size() * 100 );
    // TODO: shouldn't we filter by the event?
    final IFeatureWrapperCollection<IFloodPolygon> polygons = m_model.getPolygons();

    /* check for existing result coverages */
    final ICoverageCollection resultCoverages = event.getResultCoverages();
    if( resultCoverages.getCoverages().size() != 0 )
      throw new IllegalStateException( Messages.getString( "org.kalypso.model.flood.core.FloodModelProcess.1" ) + event.getName() ); //$NON-NLS-1$

    final IFolder scenarioFolder = KalypsoAFGUIFrameworkPlugin.getDefault().getActiveWorkContext().getCurrentCase().getFolder();
    final IFolder eventsFolder = scenarioFolder.getFolder( "events" ); //$NON-NLS-1$
    final IFolder eventFolder = eventsFolder.getFolder( event.getDataPath().toPortableString() );

    for( final ICoverage terrainCoverage : terrainCoverages )
    {
      progress.subTask( terrainCoverage.getName() );

      // final IGeoGrid terrainGrid = GeoGridUtilities.toGrid( terrainCoverage );
      final RectifiedGridCoverageGeoGrid inputGrid = (RectifiedGridCoverageGeoGrid) GeoGridUtilities.toGrid( terrainCoverage );
      final IFeatureWrapperCollection<ITinReference> tins = event.getTins();

      // final IGeoGrid diffGrid = new FloodDiffGrid( terrainGrid, tins, polygons, event );
      final SequentialBinaryGeoGridReader inputGridReader = new FloodDiffGrid( inputGrid, inputGrid.getGridURL(), tins, polygons, event );

      /* set destination: => event folder/results */
      // generate unique name for grid file
      final File gridsPath = new File( eventFolder.toString(), "results" ); //$NON-NLS-1$
      final String uniqueFileName = FileUtilities.createNewUniqueFileName( "grid", ".ascbin", gridsPath ); //$NON-NLS-1$ //$NON-NLS-2$

      // get results folder
      final IFolder destFolder = eventFolder.getFolder( "results" ); //$NON-NLS-1$
      if( destFolder.exists() == false )
      {
        destFolder.create( false, true, new SubProgressMonitor( monitor, 5 ) );
      }

      // get file
      final IFile destFile = destFolder.getFile( uniqueFileName );
      final File file = destFile.getLocation().toFile();

      final String fileName = "../events/" + event.getDataPath() + "/results/" + file.getName(); //$NON-NLS-1$ //$NON-NLS-2$
      //      final ICoverage coverage = GeoGridUtilities.addCoverage( resultCoverages, diffGrid, file, fileName, "image/bin", progress.newChild( 95 ) ); //$NON-NLS-1$
      final ICoverage coverage = GeoGridUtilities.addCoverage( resultCoverages, inputGridReader, 2, file, fileName, "image/bin", progress.newChild( 95 ) ); //$NON-NLS-1$

      coverage.setName( Messages.getString( "org.kalypso.model.flood.core.FloodModelProcess.10", terrainCoverage.getName() ) ); //$NON-NLS-1$

      final String desc = Messages.getString( "org.kalypso.model.flood.core.FloodModelProcess.11", new Date(), terrainCoverage.getName() ); //$NON-NLS-1$
      coverage.setDescription( desc );

      // terrainGrid.dispose();
      inputGridReader.close();
      inputGridReader.dispose();
    }

    /* update resource folder */
    scenarioFolder.refreshLocal( IResource.DEPTH_INFINITE, new NullProgressMonitor() );
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
}
