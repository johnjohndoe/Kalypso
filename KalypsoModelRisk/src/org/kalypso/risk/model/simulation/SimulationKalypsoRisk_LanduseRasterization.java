/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.risk.model.simulation;

import java.io.File;
import java.net.URL;
import java.util.List;

import org.deegree.crs.transformations.CRSTransformation;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.grid.AbstractDelegatingGeoGrid;
import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IVectorDataModel;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.transformation.CachedTransformationFactory;
import org.kalypso.transformation.TransformUtilities;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author Dejan Antanaskovic
 * 
 */
public class SimulationKalypsoRisk_LanduseRasterization implements ISimulationSpecKalypsoRisk, ISimulation
{
  /**
   * @see org.kalypso.simulation.core.ISimulation#getSpezifikation()
   */
  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( "Specification_LanduseRasterization.xml" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.simulation.core.ISimulation#run(java.io.File, org.kalypso.simulation.core.ISimulationDataProvider,
   *      org.kalypso.simulation.core.ISimulationResultEater, org.kalypso.simulation.core.ISimulationMonitor)
   */
  @Override
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    try
    {
      final GMLWorkspace rasterModelWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( MODELSPEC_KALYPSORISK.RASTER_MODEL.name() ), null );
      final IRasterDataModel rasterModel = (IRasterDataModel) rasterModelWorkspace.getRootFeature().getAdapter( IRasterDataModel.class );

      final GMLWorkspace vectorModelWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( MODELSPEC_KALYPSORISK.VECTOR_MODEL.name() ), null );
      final IVectorDataModel vectorModel = (IVectorDataModel) vectorModelWorkspace.getRootFeature().getAdapter( IVectorDataModel.class );

      final File outputRasterTmpDir = new File( tmpdir, "outputRaster" ); //$NON-NLS-1$
      outputRasterTmpDir.mkdir();
      doLanduseRasterization( outputRasterTmpDir, rasterModel, vectorModel, monitor, 1.0 );
      final File tmpRasterModel = File.createTempFile( IRasterDataModel.MODEL_NAME, ".gml", tmpdir ); //$NON-NLS-1$
      GmlSerializer.serializeWorkspace( tmpRasterModel, rasterModelWorkspace, "UTF-8" ); //$NON-NLS-1$
      resultEater.addResult( MODELSPEC_KALYPSORISK.RASTER_MODEL.name(), tmpRasterModel );
      resultEater.addResult( MODELSPEC_KALYPSORISK.OUTPUT_RASTER_FOLDER.name(), outputRasterTmpDir );
    }
    catch( final Exception e )
    {
      throw new SimulationException( e.getLocalizedMessage() );
    }
  }

  /**
   * creates the land use raster files. The grid cells get the ordinal number of the the land use class.
   * 
   */
  private void doLanduseRasterization( final File tmpdir, final IRasterDataModel rasterModel, final IVectorDataModel vectorModel, final ISimulationMonitor monitor, final double chainProcessWeight ) throws SimulationException
  {
    monitor.setMessage( Messages.getString( "org.kalypso.risk.model.simulation.SimulationKalypsoRisk_LanduseRasterization.0" ) ); //$NON-NLS-1$
    final IFeatureWrapperCollection<IAnnualCoverageCollection> waterDepthCoverageCollection = rasterModel.getWaterlevelCoverageCollection();
    if( waterDepthCoverageCollection.size() == 0 )
      throw new SimulationException( "Keine Fliesstiefen Rasterdaten vorhanden. Bitte importieren Sie zuerst die Fliesstiefen." ); //$NON-NLS-1$
    final IAnnualCoverageCollection maxCoveragesCollection = RiskModelHelper.getMaxReturnPeriodCollection( waterDepthCoverageCollection );
    final Integer maxReturnPeriod = maxCoveragesCollection.getReturnPeriod();
    if( maxReturnPeriod == Integer.MIN_VALUE )
      throw new SimulationException( Messages.getString( "org.kalypso.risk.model.simulation.SimulationKalypsoRisk_LanduseRasterization.1" ) ); //$NON-NLS-1$

    final ICoverageCollection inputCoverages = maxCoveragesCollection;
    final ICoverageCollection outputCoverages = rasterModel.getLanduseCoverage();

    // remove existing (invalid) coverage entries from the model
    outputCoverages.clear();

    // TODO: delete old landuse coverage files!
    // hmm... how to initiate that with WPS?

    final IFeatureWrapperCollection<ILandusePolygon> polygonCollection = vectorModel.getLandusePolygonCollection();

    final double monitorIncrement = (100.0 - monitor.getProgress()) * chainProcessWeight / inputCoverages.size();
    final int monitorInitial = monitor.getProgress();
    try
    {
      for( int i = 0; i < inputCoverages.size(); i++ )
      {
        final ICoverage inputCoverage = inputCoverages.get( i );
        monitor.setMessage( Messages.getString( "org.kalypso.risk.model.simulation.RiskModelHelper.14" ) + (i + 1) + "/" + inputCoverages.size() + "]..." ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

        final IGeoGrid inputGrid = GeoGridUtilities.toGrid( inputCoverage );
        final int sizeY = inputGrid.getSizeY();

        /* This grid should have the cs of the input grid. */
        final IGeoGrid outputGrid = new AbstractDelegatingGeoGrid( inputGrid )
        {
          /**
           * @see org.kalypso.grid.AbstractDelegatingGeoGrid#getValue(int, int) gets the ordinal number of the landuse
           *      class
           */
          @Override
          public double getValue( int x, int y ) throws GeoGridException
          {
            if( y % 20 == 0 )
              monitor.setProgress( monitorInitial + ((int) (monitorIncrement * y / sizeY)) );
            try
            {
              final Double value = super.getValue( x, y );
              if( value.equals( Double.NaN ) )
                return Double.NaN;
              else
              {
                /* This coordinate has the cs of the input grid! */
                final Coordinate coordinate = GeoGridUtilities.toCoordinate( inputGrid, x, y, null );

                if( polygonCollection.size() == 0 )
                  return Double.NaN;

                final ILandusePolygon landusePolygon = polygonCollection.get( 0 );
                final String coordinateSystem = landusePolygon.getGeometry().getCoordinateSystem();
                final GM_Position positionAt = JTSAdapter.wrap( coordinate );

                /* Transform query position into the cs of the polygons. */
                final CRSTransformation transformation = CachedTransformationFactory.getInstance().createFromCoordinateSystems( inputGrid.getSourceCRS(), coordinateSystem );
                final GM_Position position = TransformUtilities.transform( positionAt, transformation );

                /* This list has some unknown cs. */
                final List<ILandusePolygon> list = polygonCollection.query( position );
                if( list == null || list.size() == 0 )
                  return Double.NaN;
                else
                  for( final ILandusePolygon polygon : list )
                  {
                    if( polygon.contains( position ) )
                    {
                      final Integer ordinalNumber = polygon.getLanduseClassOrdinalNumber();
                      return ordinalNumber != null ? ordinalNumber : Double.NaN;
                    }
                  }
                return Double.NaN;
              }
            }
            catch( Exception ex )
            {
              throw new GeoGridException( Messages.getString( "org.kalypso.risk.model.simulation.RiskModelHelper.10" ), ex ); //$NON-NLS-1$
            }
          }
        };
        final String outputCoverageFileName = inputCoverage.getGmlID() + "_" + i + ".bin"; //$NON-NLS-1$ //$NON-NLS-2$
        final String outputCoverageFileRelativePath = CONST_COVERAGE_FILE_RELATIVE_PATH_PREFIX + outputCoverageFileName;
        final File outputCoverageFile = new File( tmpdir.getAbsolutePath(), outputCoverageFileName );
        GeoGridUtilities.addCoverage( outputCoverages, outputGrid, outputCoverageFile, outputCoverageFileRelativePath, "image/bin", new NullProgressMonitor() ); //$NON-NLS-1$
        inputGrid.dispose();
      }
    }
    catch( final Exception e )
    {
      throw new SimulationException( e.getLocalizedMessage() );
    }
  }
}
