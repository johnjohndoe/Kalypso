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
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.deegree.crs.transformations.CRSTransformation;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.gml.ui.map.CoverageManagementHelper;
import org.kalypso.grid.AbstractDelegatingGeoGrid;
import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.risk.i18n.Messages;
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
import org.kalypso.simulation.core.SimulationMonitorAdaptor;
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
    return getClass().getResource( "Specification_SpecificDamageCalculation.xml" ); //$NON-NLS-1$
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
      final IProgressMonitor simulationMonitorAdaptor = new SimulationMonitorAdaptor( monitor );
      simulationMonitorAdaptor.beginTask( Messages.getString( "org.kalypso.risk.model.simulation.SimulationKalypsoRisk_SpecificDamageCalculation.1" ), 100 ); //$NON-NLS-1$
      final GMLWorkspace controlModelWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( MODELSPEC_KALYPSORISK.CONTROL_MODEL.name() ), null );
      final IRasterizationControlModel controlModel = (IRasterizationControlModel) controlModelWorkspace.getRootFeature().getAdapter( IRasterizationControlModel.class );

      final GMLWorkspace rasterModelWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( MODELSPEC_KALYPSORISK.RASTER_MODEL.name() ), null );
      final IRasterDataModel rasterModel = (IRasterDataModel) rasterModelWorkspace.getRootFeature().getAdapter( IRasterDataModel.class );

      final GMLWorkspace vectorModelWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( MODELSPEC_KALYPSORISK.VECTOR_MODEL.name() ), null );
      final IVectorDataModel vectorModel = (IVectorDataModel) vectorModelWorkspace.getRootFeature().getAdapter( IVectorDataModel.class );

      final File outputRasterTmpDir = new File( tmpdir, "outputRaster" ); //$NON-NLS-1$
      outputRasterTmpDir.mkdir();
      doDamagePotentialCalculation( outputRasterTmpDir, controlModel, rasterModel, vectorModel, simulationMonitorAdaptor );
      final File tmpRasterModel = File.createTempFile( IRasterDataModel.MODEL_NAME, ".gml", tmpdir ); //$NON-NLS-1$
      GmlSerializer.serializeWorkspace( tmpRasterModel, rasterModelWorkspace, "UTF-8" ); //$NON-NLS-1$
      resultEater.addResult( MODELSPEC_KALYPSORISK.RASTER_MODEL.name(), tmpRasterModel );
      resultEater.addResult( MODELSPEC_KALYPSORISK.OUTPUT_RASTER_FOLDER.name(), outputRasterTmpDir );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new SimulationException( e.getLocalizedMessage() );
    }
  }

  /**
   * Creates the specific damage coverage collection. <br>
   * The damage value for each grid cell is taken from the underlying polygon.
   */
  private void doDamagePotentialCalculation( final File tmpdir, final IRasterizationControlModel controlModel, final IRasterDataModel rasterModel, final IVectorDataModel vectorModel, final IProgressMonitor monitor ) throws SimulationException
  {
    final IFeatureWrapperCollection<IAnnualCoverageCollection> specificDamageCoverageCollection = rasterModel.getSpecificDamageCoverageCollection();
    final IFeatureWrapperCollection<ILandusePolygon> polygonCollection = vectorModel.getLandusePolygonCollection();
    final List<ILanduseClass> landuseClassesList = controlModel.getLanduseClassesList();

    if( rasterModel.getWaterlevelCoverageCollection().size() == 0 )
      throw new SimulationException( Messages.getString( "org.kalypso.risk.model.simulation.SimulationKalypsoRisk_SpecificDamageCalculation.0" ) ); //$NON-NLS-1$

    /*
     * As the default value is 1, this is cannot happen any more
     * 
     * for( final IAnnualCoverageCollection collection : rasterModel.getWaterlevelCoverageCollection() ) { final Integer
     * returnPeriod = collection.getReturnPeriod(); if( returnPeriod == null || returnPeriod <= 0 ) throw new
     * SimulationException( Messages.getString( "DamagePotentialCalculationHandler.18" ) ); //$NON-NLS-1$ }
     */
    try
    {
      /* clear existing data */
      for( int i = 0; i < specificDamageCoverageCollection.size(); i++ )
      {
        final IAnnualCoverageCollection annualCoverageCollection = specificDamageCoverageCollection.get( i );
        for( int k = 0; k < annualCoverageCollection.size(); k++ )
          CoverageManagementHelper.deleteGridFile( annualCoverageCollection.get( k ) );
      }
      specificDamageCoverageCollection.clear();

      // Put classes into list by, with index ordinal-number for faster access later
      final List<ILanduseClass> landuseClasses = new ArrayList<ILanduseClass>();
      for( ILanduseClass landuseClass : landuseClassesList )
      {
        landuseClass.clearStatisticEntries();

        int ordinalNumber = landuseClass.getOrdinalNumber();
        if(  landuseClasses.size() > ordinalNumber && landuseClasses.get( ordinalNumber ) != null )
          System.out.println( String.format( "WARNING: two landuse classes with same oridnal number: %s", ordinalNumber ) );
        
        while( landuseClasses.size() < ordinalNumber + 1 )
          landuseClasses.add( null );
        
        landuseClasses.set( ordinalNumber, landuseClass );
      }

      /* loop over all waterdepths */
      for( final IAnnualCoverageCollection srcAnnualCoverages : rasterModel.getWaterlevelCoverageCollection() )
      {
        int srcAnnualCoverageSize = srcAnnualCoverages.size();
        final int perCoverageTicks = 100 / srcAnnualCoverageSize;
        String taskName = String.format( Messages.getString( "org.kalypso.risk.model.simulation.DamagePotentialCalculationHandler.10" ), srcAnnualCoverages.getReturnPeriod() );
        final SubMonitor subMonitor = SubMonitor.convert( monitor, taskName, 100 );

        /* create annual damage coverage collection */
        final IAnnualCoverageCollection destCoverageCollection = specificDamageCoverageCollection.addNew( IAnnualCoverageCollection.QNAME );

        final int returnPeriod = srcAnnualCoverages.getReturnPeriod();

        for( int i = 0; i < srcAnnualCoverageSize; i++ )
        {
          final ICoverage inputCoverage = srcAnnualCoverages.get( i );
          if( srcAnnualCoverageSize > 1 )
            subMonitor.subTask( String.format( "%s (%s)", taskName, inputCoverage.getName() ) );
          final IGeoGrid inputGrid = GeoGridUtilities.toGrid( inputCoverage );
          final double cellSize = Math.abs( inputGrid.getOffsetX().x - inputGrid.getOffsetY().x ) * Math.abs( inputGrid.getOffsetX().y - inputGrid.getOffsetY().y );

          final IGeoGrid outputGrid = new AbstractDelegatingGeoGrid( inputGrid )
          {
            /**
             * @see org.kalypso.grid.AbstractDelegatingGeoGrid#getValue(int, int) gets the damage value for each grid
             *      cell from the underlying polygon.
             */
            @Override
            public double getValue( final int x, final int y ) throws GeoGridException
            {
              try
              {
                final Double value = super.getValue( x, y );
                if( value.equals( Double.NaN ) )
                  return Double.NaN;

                // possible that waterdepth input grid contains water depth less than zero!
                else if( value.doubleValue() <= 0.0 )
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

                        if( damageValue <= 0.0 )
                          return Double.NaN;

                        /* set statistic for landuse class */
                        ILanduseClass landuseClass = landuseClasses.get( landuseClassOrdinalNumber );
                        if( landuseClass == null )
                          System.out.println( String.format( "Unknown landuse class: %s", landuseClassOrdinalNumber ) );
                        else
                          RiskModelHelper.fillStatistics( returnPeriod, landuseClass, damageValue, cellSize );
                        return damageValue;
                      }
                    }
                  }
                  return Double.NaN;
                }
              }
              catch( final Exception ex )
              {
                throw new GeoGridException( Messages.getString( "org.kalypso.risk.model.simulation.RiskModelHelper.0" ), ex ); //$NON-NLS-1$
              }
            }
          };

          /* add the new coverage to the collection */
          final String outputCoverageFileName = "specificDamage_HQ" + srcAnnualCoverages.getReturnPeriod() + "_" + i + ".bin"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
          final String outputCoverageFileRelativePath = CONST_COVERAGE_FILE_RELATIVE_PATH_PREFIX + outputCoverageFileName;
          final File outputCoverageFile = new File( tmpdir.getAbsolutePath(), outputCoverageFileName );
          final ICoverage newCoverage = GeoGridUtilities.addCoverage( destCoverageCollection, outputGrid, outputCoverageFile, outputCoverageFileRelativePath, "image/bin", subMonitor.newChild( perCoverageTicks, SubMonitor.SUPPRESS_ALL_LABELS ) ); //$NON-NLS-1$

          for( final ILanduseClass landuseClass : landuseClassesList )
          {
            landuseClass.updateStatistic( returnPeriod );
          }
          newCoverage.setName( String.format( Messages.getString( "org.kalypso.risk.model.simulation.SimulationKalypsoRisk_SpecificDamageCalculation.2" ), srcAnnualCoverages.getReturnPeriod(), i ) ); //$NON-NLS-1$
          newCoverage.setDescription( String.format( Messages.getString( "org.kalypso.risk.model.simulation.SimulationKalypsoRisk_SpecificDamageCalculation.3" ), new Date().toString() ) ); //$NON-NLS-1$
          inputGrid.dispose();
        }
        /* set the return period of the specific damage grid */
        destCoverageCollection.setReturnPeriod( srcAnnualCoverages.getReturnPeriod() );
      }
    }
    catch( final Exception e )
    {   
      e.printStackTrace();
      throw new SimulationException( Messages.getString( "org.kalypso.risk.model.simulation.RiskCalcSpecificDamageRunnable.1" ) + ": " + e.getLocalizedMessage() ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }
}
