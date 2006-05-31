/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.convert.namodel;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.URL;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import org.apache.commons.io.IOUtils;
import org.kalypso.convert.namodel.job.NaModelParameterAnalyseSimulation;
import org.kalypso.convert.namodel.optimize.CalcDataProviderDecorater;
import org.kalypso.convert.namodel.optimize.NAOptimizingJob;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.optimize.IOptimizingJob;
import org.kalypso.optimize.OptimizerCalJob;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.opengis.cs.CS_CoordinateSystem;

import com.vividsolutions.jts.geom.Geometry;

/**
 * @author doemming
 */
public class NaModelCalcJob implements ISimulation
{

  private ISimulation m_calcJob = null;

  private static final String MEASURE_SEAL_FEATURE = "MeasureSealing";

  private static final String MEASURE_PROP_SEAL_GEOM = "areaOfInterest";

  private static final String MEASURE_PROP_SEAL_FACTOR = "grz";

  private static final String HYDRO_PROP_GEOM = "Ort";

  private static final String CATCHMENT_PROP_GEOM = "Ort";

  private static final String HYDRO_PROP_SEAL_FACTOR = "m_vers";

  private static final String MEASURE_RETENSION_BASIN_FEATURE = "RetensionBasin";

  private static final String MEASURE_PROP_RETENSION_GEOM = "position";

  private static final String DEFAULT_ENCONDING = "UTF-8";

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.job.ICalcDataProvider, org.kalypso.services.calculation.job.ICalcResultEater,
   *      org.kalypso.services.calculation.job.ICalcMonitor)
   */
  public void run( final File tmpdir, final ISimulationDataProvider dataProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    try
    {
      final Logger logger = Logger.getAnonymousLogger();

      // TODO make seperate calcjob for next function !
      // final ISimulationDataProvider innerDataProvider = getDataProviderFromMeasure( logger, dataProvider, tmpdir );

      // testen ob calcjob optimization hat
      // final URL schemaURL = getClass().getResource( "schema/nacontrol.xsd" );
      // final GMLWorkspace controlWorkspace = GmlSerializer.createGMLWorkspace( (URL) innerDataProvider.getInputForID(
      // NaModelConstants.IN_CONTROL_ID ) );
      final GMLWorkspace controlWorkspace = GmlSerializer.createGMLWorkspace( (URL) dataProvider.getInputForID( NaModelConstants.IN_CONTROL_ID ) );
      final Feature rootFeature = controlWorkspace.getRootFeature();
      final boolean optimize = FeatureHelper.booleanIsTrue( rootFeature, "automaticCallibration", false );

      if( dataProvider.hasID( NaModelConstants.IN_ANALYSE_MODELL_XSD_ID ) )
        m_calcJob = new NaModelParameterAnalyseSimulation( logger );
      else if( optimize )
      {
        final IOptimizingJob optimizeJob;
        // optimizeJob = new NAOptimizingJob( tmpdir, innerDataProvider, monitor );
        optimizeJob = new NAOptimizingJob( tmpdir, dataProvider, monitor );
        m_calcJob = new OptimizerCalJob( logger, optimizeJob );
      }
      else
        m_calcJob = new NaModelInnerCalcJob();

      if( m_calcJob != null )
        // m_calcJob.run( tmpdir, innerDataProvider, resultEater, monitor );
        m_calcJob.run( tmpdir, dataProvider, resultEater, monitor );
    }
    catch( Exception e )
    {
      throw new SimulationException( "could not instantiate NAOptimizingJob", e );
    }
  }

  /**
   * @param originalDataProvider
   * @param tmpDir
   *          dir for temporary files
   * @return modified dataprovider including measures or same dataprovider if no measures are used <br>
   *         TODO move this business to a measureclacjob !
   */
  private ISimulationDataProvider getDataProviderFromMeasure( final Logger logger, final ISimulationDataProvider originalDataProvider, final File tmpDir )
  {
    final CalcDataProviderDecorater result = new CalcDataProviderDecorater( originalDataProvider );
    if( !(originalDataProvider.hasID( NaModelConstants.IN_MEASURE_ID ) && originalDataProvider.hasID( NaModelConstants.IN_HYDROTOP_ID ) && originalDataProvider.hasID( NaModelConstants.IN_MODELL_ID )) )
    {
      final StringBuffer buffer = new StringBuffer();
      if( !originalDataProvider.hasID( NaModelConstants.IN_MEASURE_ID ) )
        buffer.append( "optional dataset 'measures' is not provided, " );
      if( !originalDataProvider.hasID( NaModelConstants.IN_HYDROTOP_ID ) )
        buffer.append( "dataset 'hydrotope' is not provided, " );
      if( !originalDataProvider.hasID( NaModelConstants.IN_MODELL_ID ) )
        buffer.append( "dataset 'model' is not provided, " );
      buffer.append( "so no measures will be used..." );
      logger.info( buffer.toString() );
      return result;
    }
    try
    {
      logger.info( "start using dataset 'measure' for updating the model ..." );
      /** Get available Measuers */
      final URL measureURL = (URL) originalDataProvider.getInputForID( NaModelConstants.IN_MEASURE_ID );
      final GMLWorkspace measureWorkspace = GmlSerializer.createGMLWorkspace( measureURL );
      /** Insert implemented Measure */
      insertStorageChannelMeasure( measureWorkspace, originalDataProvider, result, logger, tmpDir );
      insertSealingChangeMeasure( measureWorkspace, originalDataProvider, result, logger, tmpDir );
    }
    catch( Exception e )
    {
      logger.info( "could not read measure from dataprovider, will continue no meauser used,\n reason:" + e.getLocalizedMessage() );
      e.printStackTrace();
      return result;
    }
    return result;
  }

  /**
   * @param measureWorkspace
   * @param originalDataProvider
   * @param result
   * @param logger
   * @param tmpDir
   * @throws Exception
   */
  private void insertSealingChangeMeasure( GMLWorkspace measureWorkspace, ISimulationDataProvider originalDataProvider, CalcDataProviderDecorater result, Logger logger, File tmpDir ) throws SimulationException, IOException, Exception
  {
    final URL hydrotopURL = (URL) originalDataProvider.getInputForID( NaModelConstants.IN_HYDROTOP_ID );
    // Versiegelungsgrad Measure
    final IFeatureType sealingFT = measureWorkspace.getFeatureType( MEASURE_SEAL_FEATURE );
    final Feature[] sealingFEs = measureWorkspace.getFeatures( sealingFT );
    if( sealingFEs.length == 0 )
    {
      logger.info( "measure " + MEASURE_SEAL_FEATURE + " is empty, continue normal simulation without this measure" );
      return;
    }
    final GMLWorkspace hydroWorkspace = GmlSerializer.createGMLWorkspace( hydrotopURL );
    // final IFeatureType hydroFT = hydroWorkspace.getFeatureType( null );
    final FeatureList hydroList = (FeatureList) hydroWorkspace.getFeatureFromPath( "HydrotopCollectionMember/HydrotopMember" );

    // for geometry operations hydroworkspace and measureworkspace must use the same coordinatessystem,
    // so let measure transform to the one hydo uses. (less work than other way)
    if( hydroList.size() > 0 )
    {
      final Feature feature = (Feature) hydroList.get( 0 );
      final GM_Object geom = feature.getGeometryProperties()[0];
      final CS_CoordinateSystem targetCS = geom.getCoordinateSystem();
      final TransformVisitor visitor = new TransformVisitor( targetCS );
      measureWorkspace.accept( visitor, "/", FeatureVisitor.DEPTH_INFINITE );
    }
    for( int i = 0; i < sealingFEs.length; i++ )
    {
      int c_success = 0;
      int c_error = 0;
      final Feature sealFE = sealingFEs[i];
      double sealMeasure = FeatureHelper.getAsDouble( sealFE, MEASURE_PROP_SEAL_FACTOR, 1.0d );
      final GM_Object measureGEOM = (GM_Object) sealFE.getProperty( MEASURE_PROP_SEAL_GEOM );
      final Geometry jtsMeasureGEOM = JTSAdapter.export( measureGEOM );
      // final double areaMeasure = jtsMeasureGEOM.getArea();
      final GM_Envelope selENV = sealFE.getEnvelope();
      final List hydrosInENV = hydroList.query( selENV, null );
      for( Iterator iter = hydrosInENV.iterator(); iter.hasNext(); )
      {
        final Feature hydroFE = (Feature) iter.next();
        final double sealHydro = FeatureHelper.getAsDouble( hydroFE, HYDRO_PROP_SEAL_FACTOR, 1.0d );

        final GM_Object hydroGEOM = (GM_Object) hydroFE.getProperty( HYDRO_PROP_GEOM );
        final Geometry jtsHydroGEOM = JTSAdapter.export( hydroGEOM );
        Geometry intersection = null;
        try
        {
          intersection = jtsMeasureGEOM.intersection( jtsHydroGEOM );
          c_success++;
        }
        catch( Exception e )
        {
          c_error++;
        }
        if( intersection == null || intersection.isEmpty() )
          continue;
        final double areaIntersection = intersection.getArea();
        final double areaHydro = jtsHydroGEOM.getArea();
        // TODO check for numerical best order
        // remark: it does not matter, if in next loop the same hydrotop again is affected
        final double newSealFactor = ((areaHydro - areaIntersection) * sealHydro + areaIntersection * sealMeasure) / areaHydro;
        hydroFE.setProperty( HYDRO_PROP_SEAL_FACTOR, new Double( newSealFactor ) );
      }
      logger.info( "Fehler Hydrotop Measure: " + c_error + "/" + (c_success + c_error) + "\n" );
    }
    final File hydroTopFile = File.createTempFile( "measured_hydrotops", ".gml", tmpDir );
    final Writer writerHydroTop = new BufferedWriter( new OutputStreamWriter( new FileOutputStream( hydroTopFile ), DEFAULT_ENCONDING ) );
    try
    {
      GmlSerializer.serializeWorkspace( writerHydroTop, hydroWorkspace, DEFAULT_ENCONDING );
    }
    finally
    {
      IOUtils.closeQuietly( writerHydroTop );
    }
    final URL newHydroURL = hydroTopFile.toURL();
    // now overwrite the old hydros
    result.addURL( NaModelConstants.IN_HYDROTOP_ID, newHydroURL );
  }

  /**
   * @param measureWorkspace
   * @param originalDataProvider
   * @param result
   * @param tmpDir
   * @throws Exception
   */
  private void insertStorageChannelMeasure( GMLWorkspace measureWorkspace, ISimulationDataProvider originalDataProvider, CalcDataProviderDecorater result, Logger logger, File tmpDir ) throws SimulationException, IOException, Exception
  {
    final URL measureURL = (URL) originalDataProvider.getInputForID( NaModelConstants.IN_MEASURE_ID );
    final URL modelURL = (URL) originalDataProvider.getInputForID( NaModelConstants.IN_MODELL_ID );
    // *Get available Measuers*/
    final IFeatureType measureRhbFT = measureWorkspace.getFeatureType( MEASURE_RETENSION_BASIN_FEATURE );
    final Feature[] measureRhbFEs = measureWorkspace.getFeatures( measureRhbFT );
    if( measureRhbFEs.length == 0 )
    {
      logger.info( "measure " + MEASURE_RETENSION_BASIN_FEATURE + " is empty, continue normal simulation without this measure" );
      return;
    }
    final GMLWorkspace modelworkspace = GmlSerializer.createGMLWorkspace( modelURL );
    final FeatureList rbList = (FeatureList) modelworkspace.getFeatureFromPath( "ChannelCollectionMember/channelMember[StorageChannel]" );
    final FeatureList catchementList = (FeatureList) modelworkspace.getFeatureFromPath( "CatchmentCollectionMember/catchmentMember" );
    // for geometry operations modelWorkspace and measureworkspace must use the same coordinatessystem,
    // so let measure transform to the one hydo uses. (less work than other way)
    if( rbList.size() > 0 )
    {
      final Feature feature = (Feature) rbList.get( 0 );
      final GM_Object geom = feature.getGeometryProperties()[0];
      final CS_CoordinateSystem targetCS = geom.getCoordinateSystem();
      final TransformVisitor visitor = new TransformVisitor( targetCS );
      measureWorkspace.accept( visitor, "/", FeatureVisitor.DEPTH_INFINITE );
    }
    for( int i = 0; i < measureRhbFEs.length; i++ )
    {
      int c_success = 0;
      int c_error = 0;

      final Feature measureRhbFE = measureRhbFEs[i];
      final GM_Object measureRhbGEOM = (GM_Object) measureRhbFE.getProperty( MEASURE_PROP_RETENSION_GEOM );
      GM_Envelope rbENV = measureRhbGEOM.getEnvelope();
      // TODO was passiert wenn das RHB nicht eindeutig in einem catchment liegt (hier wird angenommen das es
      // komplet
      // im Chatchment ist)
      List catchmentInENV = catchementList.query( rbENV, null );
      for( Iterator iter = catchmentInENV.iterator(); iter.hasNext(); )
      {
        Feature catchment = (Feature) iter.next();
        GM_Object catchmentGEOM = (GM_Object) catchment.getProperty( CATCHMENT_PROP_GEOM );
        if( catchmentGEOM.contains( measureRhbGEOM ) )
        {

          IFeatureType storageChannelFT = modelworkspace.getFeatureType( NaModelHelper.STORAGE_CHANNEL_ELEMENT_NAME );
          int error = NaModelHelper.addRHBinCatchment( new CommandableWorkspace( modelworkspace ), catchment, storageChannelFT, measureRhbFE );
          if( error < 0 )
            c_error++;
          else
            c_success++;
        }
      }
      logger.info( "Fehler Rückhaltebecken Measure: " + c_error + "/" + (c_success + c_error) + "\n" );
    }
    final File modelFile = File.createTempFile( "measured_model", ".gml", tmpDir );
    final Writer writerModel = new BufferedWriter( new OutputStreamWriter( new FileOutputStream( modelFile ), DEFAULT_ENCONDING ) );
    try
    {
      GmlSerializer.serializeWorkspace( writerModel, modelworkspace, DEFAULT_ENCONDING );
    }
    finally
    {
      IOUtils.closeQuietly( writerModel );
    }
    final URL newModelURL = modelFile.toURL();
    // now overwrite the model file
    result.addURL( NaModelConstants.IN_MODELL_ID, newModelURL );

  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getSpezifikation()
   */
  public URL getSpezifikation( )
  {
    return getClass().getResource( "resources/nacalcjob_spec.xml" );
  }

  public boolean isSucceeded( )
  {
    if( m_calcJob instanceof NaModelInnerCalcJob )
      return ((NaModelInnerCalcJob) m_calcJob).isSucceeded();
    if( m_calcJob instanceof NAOptimizingJob )
      return ((NAOptimizingJob) m_calcJob).isSucceeded();
    if( m_calcJob instanceof NaModelParameterAnalyseSimulation )
      return ((NaModelParameterAnalyseSimulation) m_calcJob).isSucceeded();

    return false;
  }
}