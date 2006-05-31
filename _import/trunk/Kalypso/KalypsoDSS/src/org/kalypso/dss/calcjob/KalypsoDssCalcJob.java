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
package org.kalypso.dss.calcjob;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.java.xml.XMLHelper;
import org.kalypso.convert.namodel.NaModelCalcJob;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.convert.namodel.optimize.CalcDataProviderDecorater;
import org.kalypso.dss.MeasuresConstants;
import org.kalypso.dss.utils.MeasuresHelper;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
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
import org.kalypsodeegree.model.sort.JMSpatialIndex;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.opengis.cs.CS_CoordinateSystem;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;

/**
 * @author kuepfer
 */
public class KalypsoDssCalcJob implements ISimulation
{

  // public static final JAXBContext JC_SPEC = JaxbUtilities.createQuiet(
  // org.kalypso.simulation.core.simspec.ObjectFactory.class );

  private HashSet<NaModelCalcJob> m_naCalcJobs = new HashSet<NaModelCalcJob>();

  /**
   * @see org.kalypso.simulation.core.ISimulation#run(java.io.File, org.kalypso.simulation.core.ISimulationDataProvider,
   *      org.kalypso.simulation.core.ISimulationResultEater, org.kalypso.simulation.core.ISimulationMonitor)
   */
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    final Logger logger = Logger.getAnonymousLogger();
    final URL calcCases = getClass().getResource( "../resources/hq1_bis_hq100.zip" );
    ArrayList<CalcDataProviderDecorater> dataProvieder = null;
    try
    {
      // unzip all available calc cases hq1 to hq100
      ZipUtilities.unzip( calcCases.openStream(), tmpdir );
      logger.info( " ...calc cases successfully unziped" );
      FilenameFilter filter = new FilenameFilter()
      {

        public boolean accept( File dir, String name )
        {
          File file = new File( dir, name );
          if( file.isDirectory() )
            return true;
          return false;
        }
      };
      String[] directories = tmpdir.list( filter );
      final String tmpdirAsString = tmpdir.toString();
      dataProvieder = new ArrayList<CalcDataProviderDecorater>();
      for( int i = 0; i < directories.length; i++ )
      {
        final String dirName = directories[i];
        final File baseDir = new File( tmpdirAsString + "\\" + dirName );
        final NaSimulationDataProvieder naSimulationDataProvieder = new NaSimulationDataProvieder( baseDir );
        dataProvieder.add( new CalcDataProviderDecorater( naSimulationDataProvieder ) );
      }
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }

    Iterator<CalcDataProviderDecorater> iterator = dataProvieder.iterator();
    while( iterator.hasNext() )
    {
      final CalcDataProviderDecorater rrmInputProvider = iterator.next();
      mergeMeasures( inputProvider, rrmInputProvider, logger, tmpdir );
      final NaModelCalcJob calcJob = new NaModelCalcJob();
      m_naCalcJobs.add( calcJob );
      final File calcDirUrl = (File) rrmInputProvider.getInputForID( NaSimulationDataProvieder.CALC_DIR );
      calcJob.run( calcDirUrl, rrmInputProvider, resultEater, monitor );

    }

  }

  private void mergeMeasures( final ISimulationDataProvider dssInputProvider, CalcDataProviderDecorater rrmInputProvider, Logger logger, File tmpDir ) throws SimulationException
  {
    final URL measuresRhbURL = (URL) dssInputProvider.getInputForID( MeasuresConstants.IN_MEASURE_RHB_ID );
    final URL measuresSealingURL = (URL) dssInputProvider.getInputForID( MeasuresConstants.IN_MEASURE_SEALING_ID );
    final URL measuresMrsURL = (URL) dssInputProvider.getInputForID( MeasuresConstants.IN_MEASURE_MRS_ID );

    try
    {
      if( measuresRhbURL != null )
        insertStorageChannelMeasure( measuresRhbURL, rrmInputProvider, logger );
      if( measuresSealingURL != null )
        insertSealingChangeMeasure( measuresSealingURL, rrmInputProvider, logger );
      if( measuresMrsURL != null )
        insertSwaleTrenchMeasure( measuresMrsURL, rrmInputProvider, logger );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      logger.info( "Problems while merging measures with modell data - " + e.getMessage() );
    }

  }

  /**
   * @see org.kalypso.simulation.core.ISimulation#getSpezifikation()
   */
  public URL getSpezifikation( )
  {
    return null;
  }

  /**
   * This method merges the sealing measure GML-file with the original hydrotop file. The corrFactor in the hydrotop is
   * adjusted to account for the measure.
   * 
   * @param sealingURL
   *          URL of the sealingMeasure.gml
   * @param result
   *          dataProvider
   * @param logger
   *          logger
   * @throws Exception
   */
  private void insertSealingChangeMeasure( final URL sealingURL, final CalcDataProviderDecorater result, final Logger logger ) throws SimulationException, IOException, Exception
  {
    GMLWorkspace sealingWS = GmlSerializer.createGMLWorkspace( sealingURL );

    final URL hydrotopURL = (URL) result.getInputForID( NaModelConstants.IN_HYDROTOP_ID );
    final URL parameterURL = (URL) result.getInputForID( NaModelConstants.IN_PARAMETER_ID );
    // Versiegelungsgrad Measure
    final IGMLSchema sealingSchema = sealingWS.getGMLSchema();
    final IFeatureType sealingFT = sealingSchema.getFeatureType( new QName( MeasuresConstants.NS_MEASURES_SEALING, MeasuresConstants.MEASURE_SEALING_FE ) );
    final Feature[] sealingFEs = sealingWS.getFeatures( sealingFT );
    if( sealingFEs.length == 0 )
    {
      logger.info( "measure " + MeasuresConstants.MEASURE_SEALING_FE + " is empty, continue normal simulation without this measure" );
      return;
    }
    final GMLWorkspace hydroWorkspace = GmlSerializer.createGMLWorkspace( hydrotopURL );
    final GMLWorkspace paraWorkspace = GmlSerializer.createGMLWorkspace( parameterURL );
    final FeatureList hydroList = (FeatureList) hydroWorkspace.getFeatureFromPath( NaModelConstants.HYDRO_MEMBER );

    // for geometry operations hydroworkspace and measureworkspace must use the same coordinatessystem,
    // so let measure transform to the one hydo uses. (less work than other way)
    if( hydroList.size() > 0 )
    {
      final Feature feature = (Feature) hydroList.get( 0 );
      final GM_Object geom = feature.getGeometryProperties()[0];
      final CS_CoordinateSystem targetCS = geom.getCoordinateSystem();
      final TransformVisitor visitor = new TransformVisitor( targetCS );
      sealingWS.accept( visitor, "/", FeatureVisitor.DEPTH_INFINITE );
    }
    for( int i = 0; i < sealingFEs.length; i++ )
    {
      int c_success = 0;
      int c_error = 0;
      final Feature sealFE = sealingFEs[i];
      double sealMeasure = FeatureHelper.getAsDouble( sealFE, MeasuresConstants.SEALING_MEASURE_SEALINGFACTOR_PROP, 1.0d );

      final GM_Object measureGEOM = (GM_Object) sealFE.getProperty( new QName( MeasuresConstants.NS_MEASURES_SEALING, MeasuresConstants.SEALING_MEASURE_GEOMETRY_PROP ) );
      final Geometry jtsMeasureGEOM = JTSAdapter.export( measureGEOM );
      // final double areaMeasure = jtsMeasureGEOM.getArea();
      final GM_Envelope selENV = sealFE.getEnvelope();
      final List<JMSpatialIndex> hydrosInENV = hydroList.query( selENV, null );
      for( Iterator iter = hydrosInENV.iterator(); iter.hasNext(); )
      {
        final Feature hydroFE = (Feature) iter.next();
        final double sealHydro = getSealingForHydrotop( hydroFE, paraWorkspace );

        final GM_Object hydroGEOM = (GM_Object) hydroFE.getProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_GEOM ) );
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
        final double corrSealingHydroOld = FeatureHelper.getAsDouble( hydroFE, NaModelConstants.HYDRO_PROP_SEAL_CORR_FACTOR, 1.0d );
        final double areaIntersection = intersection.getArea();
        final double areaHydro = jtsHydroGEOM.getArea();
        // TODO check for numerical best order, and account for corrFactor from hydrotop element
        // remark: it does not matter, if in next loop the same hydrotop again is affected

        /**
         * The corrSealingHydro1 is the fixed correction factor from the calibrated model. If the measure dose not
         * affect the whole hydrotop then a new overall sealing factor as a mean value has to be calculated
         */
        final double sealingAreaIntersection = areaIntersection * sealMeasure * corrSealingHydroOld;
        final double sealingAreaOld = (areaHydro - areaIntersection) * sealHydro * corrSealingHydroOld;
        final double newSealingFactor = (sealingAreaIntersection + sealingAreaOld) / areaHydro;
        /**
         * Now to account for the change of the old and new sealing factor we have to calculate the new correction
         * factor.
         */
        final double corrSealingHydroNew = newSealingFactor / (sealHydro * corrSealingHydroOld);

        /** it must also be weighted according the area that is affected by the change */
        hydroFE.setProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_SEAL_CORR_FACTOR ), new Double( corrSealingHydroNew ) );
      }
      logger.info( "Fehler Hydrotop Measure: " + c_error + "/" + (c_success + c_error) + "\n" );
    }
    final File calcDir = (File) result.getInputForID( NaSimulationDataProvieder.CALC_DIR );
    final File hydroTopFile = File.createTempFile( "measured_hydrotops", ".gml", calcDir );
    final Writer writerHydroTop = new BufferedWriter( new OutputStreamWriter( new FileOutputStream( hydroTopFile ), MeasuresConstants.DEFAULT_ENCONDING ) );
    try
    {
      GmlSerializer.serializeWorkspace( writerHydroTop, hydroWorkspace, MeasuresConstants.DEFAULT_ENCONDING );
    }
    finally
    {
      IOUtils.closeQuietly( writerHydroTop );
    }
    final URL newHydroURL = hydroTopFile.toURL();
    // now overwrite the old hydros
    result.addURL( NaModelConstants.IN_HYDROTOP_ID, newHydroURL );
  }

  public boolean isSucceeded( )
  {
    Iterator<NaModelCalcJob> name = m_naCalcJobs.iterator();
    while( name.hasNext() )
    {
      final NaModelCalcJob job = name.next();
      if( !job.isSucceeded() )
        return false;
    }
    return true;
  }

  /**
   * Get's the sealing factor for a hydrotop from the paramter.gml file
   * 
   * @param hydroFE
   *          the hydrotop as a Feature
   * @param paramWorkspace
   *          workspace containing the parameter-gml
   */
  private double getSealingForHydrotop( final Feature hydroFE, final GMLWorkspace paramWorkspace )
  {
    final Object property = hydroFE.getProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_LANDUSE_NAME ) );
    final FeatureList features = (FeatureList) paramWorkspace.getFeatureFromPath( NaModelConstants.PARA_PROP_LANDUSE_MEMBER );
    Feature landuseFE = null;
    Iterator it = features.iterator();
    while( it.hasNext() )
    {
      landuseFE = (Feature) it.next();
      final Object paramProperty = landuseFE.getProperty( new QName( XMLHelper.GMLSCHEMA_NS, NaModelConstants.GML_PROP_NAME ) );
      if( paramProperty.equals( property ) )
        break;
    }
    final IFeatureType featureType = landuseFE.getFeatureType();
    final IPropertyType linkProp = featureType.getProperty( new QName( NaModelConstants.NS_NAPARAMETER, NaModelConstants.PARA_LANDUSE_PROP_SEALING_LINK ) );
    final Feature sealingFE = paramWorkspace.resolveLink( landuseFE, (IRelationType) linkProp );
    return FeatureHelper.getAsDouble( sealingFE, NaModelConstants.PARA_LANDUSE_PROP_SEALING, 1.0d );
  }

  /**
   * @param measureWorkspace
   * @param originalDataProvider
   * @param result
   * @param tmpDir
   * @throws Exception
   */

  private void insertStorageChannelMeasure( final URL rhbURL, final CalcDataProviderDecorater result, final Logger logger ) throws SimulationException, IOException, Exception
  {
    final GMLWorkspace measureRhbWorkspace = GmlSerializer.createGMLWorkspace( rhbURL );
    final URL modelURL = (URL) result.getInputForID( NaModelConstants.IN_MODELL_ID );
    // *Get available Measuers*/
    final FeatureList measureRhbFEs = (FeatureList) measureRhbWorkspace.getFeatureFromPath( MeasuresConstants.MEASURE_RHB_FE );
    if( measureRhbFEs.size() == 0 )
    {
      logger.info( "measure " + MeasuresConstants.MEASURE_RHB_FE + " is empty, continue normal simulation without this measure" );
      return;
    }
    final GMLWorkspace modelworkspace = GmlSerializer.createGMLWorkspace( modelURL );
    final FeatureList rbList = (FeatureList) modelworkspace.getFeatureFromPath( "ChannelCollectionMember/channelMember[StorageChannel]" );
    final FeatureList catchementList = (FeatureList) modelworkspace.getFeatureFromPath( "CatchmentCollectionMember/catchmentMember" );
    // for geometry operations modelWorkspace and rhbMeasureWorkspace must use the same coordinatessystem,
    // so lets transform the measures workspace to the one rrm model uses. (less work than other way)
    if( rbList.size() > 0 )
    {
      final Feature feature = (Feature) rbList.get( 0 );
      final GM_Object geom = feature.getGeometryProperties()[0];
      final CS_CoordinateSystem targetCS = geom.getCoordinateSystem();
      final TransformVisitor visitor = new TransformVisitor( targetCS );
      measureRhbWorkspace.accept( visitor, "/", FeatureVisitor.DEPTH_INFINITE );
    }
    for( Iterator iterMeasureRhb = measureRhbFEs.iterator(); iterMeasureRhb.hasNext(); )
    {
      final Feature measureRhbFE = (Feature) iterMeasureRhb.next();
      int c_success = 0;
      int c_error = 0;

      final GM_Object measureRhbGEOM = (GM_Object) measureRhbFE.getProperty( new QName( MeasuresConstants.NS_MEASURES_RHB, MeasuresConstants.MRS_MEASURE_GEOMETRY_PROP ) );
      final GM_Envelope rbENV = measureRhbGEOM.getEnvelope();
      // TODO was passiert wenn das RHB nicht eindeutig in einem catchment liegt (hier wird angenommen das es
      // komplet
      // im Chatchment ist)
      final List<JMSpatialIndex> catchmentInENV = catchementList.query( rbENV, null );
      for( Iterator iter = catchmentInENV.iterator(); iter.hasNext(); )
      {
        Feature catchment = (Feature) iter.next();
        GM_Object catchmentGEOM = (GM_Object) catchment.getProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MODEL_CATCHMENT_GEOM_PROP ) );
        if( catchmentGEOM.contains( measureRhbGEOM ) )
        {
          boolean succseeded = MeasuresHelper.addRHBinCatchment( modelworkspace, catchment, rbList, measureRhbFE );
          if( !succseeded )
            c_error++;
          else
            c_success++;
        }
      }
      logger.info( "Fehler Rückhaltebecken Measure: " + c_error + "/" + (c_success + c_error) + "\n" );
    }
    final File calcDir = (File) result.getInputForID( NaSimulationDataProvieder.CALC_DIR );
    final File modelFile = File.createTempFile( "measured_model", ".gml", calcDir );
    final Writer writerModel = new BufferedWriter( new OutputStreamWriter( new FileOutputStream( modelFile ), MeasuresConstants.DEFAULT_ENCONDING ) );
    try
    {
      GmlSerializer.serializeWorkspace( writerModel, modelworkspace, MeasuresConstants.DEFAULT_ENCONDING );
    }
    finally
    {
      IOUtils.closeQuietly( writerModel );
    }
    final URL newModelURL = modelFile.toURL();
    // now overwrite the model file
    result.addURL( NaModelConstants.IN_MODELL_ID, newModelURL );

  }

  private void insertSwaleTrenchMeasure( URL measuresMrsURL, CalcDataProviderDecorater rrmInputProvider, Logger logger ) throws Exception
  {
    final GMLWorkspace mrsMeasureWorkspace = GmlSerializer.createGMLWorkspace( measuresMrsURL );
    final FeatureList mrsList = (FeatureList) mrsMeasureWorkspace.getFeatureFromPath( MeasuresConstants.MEASURE_MRS_FE );
    final URL modelURL = (URL) rrmInputProvider.getInputForID( NaModelConstants.IN_MODELL_ID );
    final GMLWorkspace modelWorkspace = GmlSerializer.createGMLWorkspace( modelURL );
    final FeatureList catchementList = (FeatureList) modelWorkspace.getFeatureFromPath( "CatchmentCollectionMember/catchmentMember" );
    for( Iterator iterMrsList = mrsList.iterator(); iterMrsList.hasNext(); )
    {
      final Feature mrsMeasure = (Feature) iterMrsList.next();
      final GM_Envelope envMeasure = mrsMeasure.getEnvelope();
      final GM_Object mrsGEOM = (GM_Object) mrsMeasure.getProperty( new QName( MeasuresConstants.NS_MEASURES_MRS, MeasuresConstants.MRS_MEASURE_GEOMETRY_PROP ) );
      final List<JMSpatialIndex> list = catchementList.query( envMeasure, null );
      for( Iterator iter = list.iterator(); iter.hasNext(); )
      {
        final Feature catchmentInEnv = (Feature) iter.next();
        final GM_Object catchmentGEOM = (GM_Object) catchmentInEnv.getProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MODEL_CATCHMENT_GEOM_PROP ) );
        final Geometry catchmentJTS = JTSAdapter.export( catchmentGEOM );
        final Geometry mrsJTS = JTSAdapter.export( mrsGEOM );
        final Geometry intersection = catchmentJTS.intersection( mrsJTS );
        if( intersection != null && intersection instanceof LineString )
        {
          double mrsLength = intersection.getLength();
          final IGMLSchema schema = modelWorkspace.getGMLSchema();
          final IFeatureType mrsFt = schema.getFeatureType( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_FT_PROP ) );
          final Feature parentFeMrs = (Feature) modelWorkspace.getFeatureFromPath( NaModelConstants.MRS_COLLECTION_PROP );

        }
      }
    }

  }
}
