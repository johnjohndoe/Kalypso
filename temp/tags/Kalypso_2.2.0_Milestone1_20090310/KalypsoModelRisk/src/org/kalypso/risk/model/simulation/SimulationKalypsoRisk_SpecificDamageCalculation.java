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
import java.util.Date;
import java.util.List;

import org.deegree.crs.transformations.CRSTransformation;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.grid.AbstractDelegatingGeoGrid;
import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.risk.Messages;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.ILanduseClass;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
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
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author Dejan Antanaskovic
 * 
 */
public class SimulationKalypsoRisk_SpecificDamageCalculation implements ISimulationSpecKalypsoRisk, ISimulation
{
  /**
   * @see org.kalypso.simulation.core.ISimulation#getSpezifikation()
   */
  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( "Specification_SpecificDamageCalculation.xml" );
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
      final GMLWorkspace controlModelWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( MODELSPEC_KALYPSORISK.CONTROL_MODEL.name() ), null );
      final IRasterizationControlModel controlModel = (IRasterizationControlModel) controlModelWorkspace.getRootFeature().getAdapter( IRasterizationControlModel.class );

      final GMLWorkspace rasterModelWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( MODELSPEC_KALYPSORISK.RASTER_MODEL.name() ), null );
      final IRasterDataModel rasterModel = (IRasterDataModel) rasterModelWorkspace.getRootFeature().getAdapter( IRasterDataModel.class );

      final GMLWorkspace vectorModelWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( MODELSPEC_KALYPSORISK.VECTOR_MODEL.name() ), null );
      final IVectorDataModel vectorModel = (IVectorDataModel) vectorModelWorkspace.getRootFeature().getAdapter( IVectorDataModel.class );

      final File outputRasterTmpDir = new File( tmpdir, "outputRaster" );
      outputRasterTmpDir.mkdir();
      doDamagePotentialCalculation( outputRasterTmpDir, controlModel, rasterModel, vectorModel, monitor, 1.0 );
      final File tmpRasterModel = File.createTempFile( IRasterDataModel.MODEL_NAME, ".gml", tmpdir );
      GmlSerializer.serializeWorkspace( tmpRasterModel, rasterModelWorkspace, "UTF-8" );
      resultEater.addResult( MODELSPEC_KALYPSORISK.RASTER_MODEL.name(), tmpRasterModel );
      resultEater.addResult( MODELSPEC_KALYPSORISK.OUTPUT_RASTER_FOLDER.name(), outputRasterTmpDir );
    }
    catch( final Exception e )
    {
      throw new SimulationException( e.getLocalizedMessage() );
    }
  }

  /**
   * Creates the specific damage coverage collection. <br>
   * The damage value for each grid cell is taken from the underlying polygon.
   */
  private void doDamagePotentialCalculation( final File tmpdir, final IRasterizationControlModel controlModel, final IRasterDataModel rasterModel, final IVectorDataModel vectorModel, final ISimulationMonitor monitor, final double chainProcessWeight ) throws SimulationException
  {
    final IFeatureWrapperCollection<IAnnualCoverageCollection> specificDamageCoverageCollection = rasterModel.getSpecificDamageCoverageCollection();
    final IFeatureWrapperCollection<ILandusePolygon> polygonCollection = vectorModel.getLandusePolygonCollection();
    final List<ILanduseClass> landuseClassesList = controlModel.getLanduseClassesList();

    if( rasterModel.getWaterlevelCoverageCollection().size() == 0 )
      throw new SimulationException( Messages.getString( org.kalypso.risk.Messages.getString( "RiskCalcSpecificDamageRunnable.0" ) ) ); //$NON-NLS-1$

    /*
     * As the default value is 1, this is cannot happen any more
     * 
     * for( final IAnnualCoverageCollection collection : rasterModel.getWaterlevelCoverageCollection() ) { final Integer
     * returnPeriod = collection.getReturnPeriod(); if( returnPeriod == null || returnPeriod <= 0 ) throw new
     * SimulationException( Messages.getString( "DamagePotentialCalculationHandler.18" ) ); //$NON-NLS-1$ }
     */

    monitor.setMessage( Messages.getString( "DamagePotentialCalculationHandler.9" ) ); //$NON-NLS-1$
    try
    {
      /* clear existing data */
      specificDamageCoverageCollection.clear();
      for( final ILanduseClass landuseClass : landuseClassesList )
        landuseClass.clearStatisticEntries();

      /* loop over all waterdepths */
      for( final IAnnualCoverageCollection srcAnnualCoverages : rasterModel.getWaterlevelCoverageCollection() )
      {
        monitor.setMessage( Messages.getString( "DamagePotentialCalculationHandler.10" ) + srcAnnualCoverages.getReturnPeriod() ); //$NON-NLS-1$

        /* create annual damage coverage collection */
        final IAnnualCoverageCollection destCoverageCollection = specificDamageCoverageCollection.addNew( IAnnualCoverageCollection.QNAME );

        final int returnPeriod = srcAnnualCoverages.getReturnPeriod();

        for( int i = 0; i < srcAnnualCoverages.size(); i++ )
        {
          final ICoverage inputCoverage = srcAnnualCoverages.get( i );

          final IGeoGrid inputGrid = GeoGridUtilities.toGrid( inputCoverage );
          final double cellSize = Math.abs( inputGrid.getOffsetX().x - inputGrid.getOffsetY().x ) * Math.abs( inputGrid.getOffsetX().y - inputGrid.getOffsetY().y );

          final IGeoGrid outputGrid = new AbstractDelegatingGeoGrid( inputGrid )
          {
            /**
             * @see org.kalypso.grid.AbstractDelegatingGeoGrid#getValue(int, int) gets the damage value for each grid
             *      cell from the underlying polygon.
             */
            @Override
            public double getValue( int x, int y ) throws GeoGridException
            {
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
                  {
                    for( final ILandusePolygon polygon : list )
                    {
                      if( polygon.contains( position ) )
                      {
                        final Integer landuseClassOrdinalNumber = polygon.getLanduseClassOrdinalNumber();
                        final double damageValue = polygon.getDamageValue( value );

                        if( Double.isNaN( damageValue ) )
                          return Double.NaN;

                        if( damageValue < 0.0 )
                          return Double.NaN;

                        /* set statistic for landuse class */
                        RiskModelHelper.fillStatistics( returnPeriod, landuseClassesList, polygon, damageValue, landuseClassOrdinalNumber, cellSize );
                        return damageValue;
                      }
                    }
                  }
                  return Double.NaN;
                }
              }
              catch( Exception ex )
              {
                throw new GeoGridException( org.kalypso.risk.Messages.getString( "RiskModelHelper.0" ), ex ); //$NON-NLS-1$
              }
            }
          };

          /* add the new coverage to the collection */
          final String outputCoverageFileName = "specificDamage_HQ" + srcAnnualCoverages.getReturnPeriod() + "_" + i + ".bin"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
          final String outputCoverageFileRelativePath = CONST_COVERAGE_FILE_RELATIVE_PATH_PREFIX + outputCoverageFileName;
          final File outputCoverageFile = new File( tmpdir.getAbsolutePath(), outputCoverageFileName );
          final ICoverage newCoverage = GeoGridUtilities.addCoverage( destCoverageCollection, outputGrid, outputCoverageFile, outputCoverageFileRelativePath, "image/bin", new NullProgressMonitor() ); //$NON-NLS-1$

          for( final ILanduseClass landuseClass : landuseClassesList )
          {
            landuseClass.updateStatistic( returnPeriod );
          }
          newCoverage.setName( Messages.getString( org.kalypso.risk.Messages.getString( "RiskModelHelper.6" ) ) + srcAnnualCoverages.getReturnPeriod() + " [" + i + "]" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
          newCoverage.setDescription( Messages.getString( org.kalypso.risk.Messages.getString( "RiskModelHelper.9" ) ) + new Date().toString() ); //$NON-NLS-1$
          inputGrid.dispose();
        }
        /* set the return period of the specific damage grid */
        destCoverageCollection.setReturnPeriod( srcAnnualCoverages.getReturnPeriod() );
      }
    }
    catch( final Exception e )
    {
      throw new SimulationException( Messages.getString( "RiskCalcSpecificDamageRunnable.1" ) + ": " + e.getLocalizedMessage() ); //$NON-NLS-1$
    }
  }
}
