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
import java.util.HashMap;
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
import org.kalypso.dss.utils.MeasuresConstants;
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
import com.vividsolutions.jts.geom.Point;

/**
 * @author kuepfer
 */
public class KalypsoDssCalcJob implements ISimulation
{

  // public static final JAXBContext JC_SPEC = JaxbUtilities.createQuiet(
  // org.kalypso.simulation.core.simspec.ObjectFactory.class );

  private HashSet<NaModelCalcJob> m_naCalcJobs = new HashSet<NaModelCalcJob>();

  ISimulationResultEater m_naJobResultEater = new ISimulationResultEater()
  {

    public void addResult( String id, File file ) throws SimulationException
    {
      if( id.equals( NaModelConstants.OUTPUT_DIR_NAME ) )
      {
        // bin jetzt im ergbinss folder
        // hole nur die ergebinse die ich am client weiter geben will
        // File res = new File("");
        // resultEater.addResult( res)
        System.out.println();
      }

    }

  };

  /** assures to return only directories for a foo.list() call */
  private FilenameFilter m_filter = new FilenameFilter()
  {

    public boolean accept( File dir, String name )
    {
      File file = new File( dir, name );
      if( file.isDirectory() )
        return true;
      return false;
    }
  };

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

      String[] directories = tmpdir.list( m_filter );
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
      mergeMeasures( inputProvider, rrmInputProvider, logger );
      final NaModelCalcJob calcJob = new NaModelCalcJob();
      m_naCalcJobs.add( calcJob );
      final File calcDirUrl = (File) rrmInputProvider.getInputForID( NaSimulationDataProvieder.CALC_DIR );
      calcJob.run( calcDirUrl, rrmInputProvider, m_naJobResultEater, monitor );

    }

  }

  private void mergeMeasures( final ISimulationDataProvider dssInputProvider, CalcDataProviderDecorater rrmInputProvider, Logger logger ) throws SimulationException
  {
    boolean writeNewModelFile = false;
    boolean writeNewHydrotopFile = false;
    // get the urls for measures and model files
    final URL measuresRhbURL = (URL) dssInputProvider.getInputForID( MeasuresConstants.IN_MEASURE_RHB_ID );
    final URL measuresSealingURL = (URL) dssInputProvider.getInputForID( MeasuresConstants.IN_MEASURE_SEALING_ID );
    final URL measuresMrsURL = (URL) dssInputProvider.getInputForID( MeasuresConstants.IN_MEASURE_MRS_ID );
    final URL designAreaURL = (URL) dssInputProvider.getInputForID( MeasuresConstants.IN_DESIGN_AREA_ID );
    final URL modelURL = (URL) rrmInputProvider.getInputForID( NaModelConstants.IN_MODELL_ID );
    final URL hydrotopURL = (URL) rrmInputProvider.getInputForID( NaModelConstants.IN_HYDROTOP_ID );
    final File calcDir = (File) rrmInputProvider.getInputForID( NaSimulationDataProvieder.CALC_DIR );

    GMLWorkspace modelWorkspace = null;
    GMLWorkspace hydrotopWorkspace = null;
    try
    {
      if( measuresRhbURL != null )
      {
        modelWorkspace = GmlSerializer.createGMLWorkspace( modelURL );
        insertStorageChannelMeasure( measuresRhbURL, modelWorkspace, logger );
        writeNewModelFile = true;
      }
      if( measuresSealingURL != null )
      {
        hydrotopWorkspace = GmlSerializer.createGMLWorkspace( hydrotopURL );
        insertSealingChangeMeasure( measuresSealingURL, hydrotopWorkspace, rrmInputProvider, logger );
        writeNewHydrotopFile = true;
      }
      if( measuresMrsURL != null )
      {
        insertSwaleTrenchMeasure( measuresMrsURL, modelWorkspace, hydrotopWorkspace, designAreaURL, logger );
        writeNewHydrotopFile = true;
        writeNewModelFile = true;
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
      logger.info( "Problems while merging measures with modell data - " + e.getMessage() );
      throw new SimulationException( e.getMessage(), e );
    }

    Writer writerHydroTop = null;
    Writer writerModel = null;
    try
    {
      try
      {
        // 1. write the changed data to a new rrm and hydrotop file
        // 2. replacing in the bean the old URL to the new files
        final File modelFile = File.createTempFile( "measured_model", ".gml", calcDir );
        if( writeNewModelFile )
        {
          writerModel = new BufferedWriter( new OutputStreamWriter( new FileOutputStream( modelFile ), MeasuresConstants.DEFAULT_ENCONDING ) );
          GmlSerializer.serializeWorkspace( writerModel, modelWorkspace, MeasuresConstants.DEFAULT_ENCONDING );
          rrmInputProvider.addURL( NaModelConstants.IN_MODELL_ID, modelFile.toURL() );
        }
        if( writeNewHydrotopFile )
        {
          final File hydroTopFile = File.createTempFile( "measured_hydrotops", ".gml", calcDir );
          writerHydroTop = new BufferedWriter( new OutputStreamWriter( new FileOutputStream( hydroTopFile ), MeasuresConstants.DEFAULT_ENCONDING ) );
          GmlSerializer.serializeWorkspace( writerHydroTop, hydrotopWorkspace, MeasuresConstants.DEFAULT_ENCONDING );
          rrmInputProvider.addURL( NaModelConstants.IN_HYDROTOP_ID, hydroTopFile.toURL() );
        }
      }
      finally
      {
        IOUtils.closeQuietly( writerHydroTop );
        IOUtils.closeQuietly( writerModel );
      }

    }
    catch( Exception e )
    {
      logger.warning( "There have been problems serializing the measured model and hydrotop files -- The model is calcuate without any measures" );
      rrmInputProvider.addURL( NaModelConstants.IN_MODELL_ID, modelURL );
      rrmInputProvider.addURL( NaModelConstants.IN_HYDROTOP_ID, hydrotopURL );
    }
  }

  /**
   * @see org.kalypso.simulation.core.ISimulation#getSpezifikation()
   */
  public URL getSpezifikation( )
  {
    return getClass().getResource( "resources/dsscalcjob_spec.xml" );
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
  private void insertSealingChangeMeasure( final URL sealingURL, final GMLWorkspace hydrotopWorkspace, final CalcDataProviderDecorater result, final Logger logger ) throws SimulationException, IOException, Exception
  {
    GMLWorkspace sealingWS = GmlSerializer.createGMLWorkspace( sealingURL );

    final URL parameterURL = (URL) result.getInputForID( NaModelConstants.IN_PARAMETER_ID );
    // Versiegelungsgrad Measure
    final IGMLSchema sealingSchema = sealingWS.getGMLSchema();
    final IFeatureType sealingFT = sealingSchema.getFeatureType( new QName( MeasuresConstants.NS_MEASURES_SEALING, MeasuresConstants.SEALING_MEASURE_FT ) );
    final Feature[] sealingFEs = sealingWS.getFeatures( sealingFT );
    if( sealingFEs.length == 0 )
    {
      logger.info( "measure " + MeasuresConstants.SEALING_MEASURE_FT + " is empty, continue normal simulation without this measure" );
      return;
    }
    final GMLWorkspace paraWorkspace = GmlSerializer.createGMLWorkspace( parameterURL );
    final FeatureList hydroList = (FeatureList) hydrotopWorkspace.getFeatureFromPath( NaModelConstants.HYDRO_MEMBER );

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
    int c_success = 0;
    int c_error = 0;
    for( int i = 0; i < sealingFEs.length; i++ )
    {
      final Feature sealFE = sealingFEs[i];
      double sealMeasure = FeatureHelper.getAsDouble( sealFE, new QName( MeasuresConstants.NS_MEASURES_SEALING, MeasuresConstants.SEALING_MEASURE_SEALINGFACTOR_PROP ), 1.0d );

      final GM_Object measureGEOM = (GM_Object) sealFE.getProperty( new QName( MeasuresConstants.NS_MEASURES_SEALING, MeasuresConstants.SEALING_MEASURE_GEOMETRY_PROP ) );
      final Geometry jtsMeasureGEOM = JTSAdapter.export( measureGEOM );
      // final double areaMeasure = jtsMeasureGEOM.getArea();
      final GM_Envelope selENV = sealFE.getEnvelope();
      final List<JMSpatialIndex> hydrosInENV = hydroList.query( selENV, null );
      for( Iterator iter = hydrosInENV.iterator(); iter.hasNext(); )
      {
        final Feature hydroFE = (Feature) iter.next();

        final GM_Object hydroGEOM = (GM_Object) hydroFE.getProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_GEOM ) );
        final Geometry jtsHydroGEOM = JTSAdapter.export( hydroGEOM );
        Geometry intersection = null;
        try
        {
          intersection = jtsMeasureGEOM.intersection( jtsHydroGEOM );
          if( intersection.isEmpty() )
            continue;
          c_success++;
        }
        catch( Exception e )
        {
          c_error++;
        }
        final double corrSealingHydroOld = FeatureHelper.getAsDouble( hydroFE, new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_SEAL_CORR_FACTOR ), 1.0d );
        final double areaIntersection = intersection.getArea();
        final double areaHydro = jtsHydroGEOM.getArea();
        // TODO check for numerical best order, and account for corrFactor from hydrotop element
        // remark: it does not matter, if in next loop the same hydrotop again is affected

        /**
         * The corrSealingHydroOld is the fixed correction factor from the calibrated model. If the measure dose not
         * affect the whole hydrotop then a new overall sealing factor as a mean value has to be calculated. <br>
         * To account for the change of the old and new sealing factor we have to calculate the new correction factor,
         * because we can not change the original sealing factor in the parameter.gml.
         */
        final double originalSealingFactor = getSealingForHydrotop( hydroFE, paraWorkspace ); // TODO abfangen wenn
        // sealHydro = 0
        // sonst infinity
        final double sealingAreaIntersection = areaIntersection * sealMeasure * corrSealingHydroOld;
        final double sealingAreaOld = (areaHydro - areaIntersection) * originalSealingFactor * corrSealingHydroOld;
        final double virtualSealingFactor = (sealingAreaIntersection + sealingAreaOld) / areaHydro;
        double corrSealingHydroNew = virtualSealingFactor / (originalSealingFactor * corrSealingHydroOld);
        if( Double.isInfinite( corrSealingHydroNew ) )
        {
          corrSealingHydroNew = 1.0d;
        }

        hydroFE.setProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_SEAL_CORR_FACTOR ), new Double( corrSealingHydroNew ) );
      }
      logger.info( "Fehler Entsiegelungs-Measure: " + c_error + "/" + (c_success + c_error) + "\n" );
    }

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
      final Object paramProperty = landuseFE.getProperty( new QName( XMLHelper.GMLSCHEMA_NS, NaModelConstants.GML_FEATURE_NAME_PROP ) );
      if( paramProperty.equals( property ) )
        break;
    }
    final IFeatureType featureType = landuseFE.getFeatureType();
    final IPropertyType linkProp = featureType.getProperty( new QName( NaModelConstants.NS_NAPARAMETER, NaModelConstants.PARA_LANDUSE_PROP_SEALING_LINK ) );
    final Feature sealingFE = paramWorkspace.resolveLink( landuseFE, (IRelationType) linkProp );
    return FeatureHelper.getAsDouble( sealingFE, new QName( NaModelConstants.NS_NAPARAMETER, NaModelConstants.PARA_LANDUSE_PROP_SEALING ), 1.0d );
  }

  /**
   * @param measureWorkspace
   * @param originalDataProvider
   * @param result
   * @param tmpDir
   * @throws Exception
   */

  private void insertStorageChannelMeasure( final URL rhbURL, final GMLWorkspace modelWorkspace, final Logger logger ) throws SimulationException, IOException, Exception
  {
    final GMLWorkspace measureRhbWorkspace = GmlSerializer.createGMLWorkspace( rhbURL );
    // *Get available Measuers*/
    final IGMLSchema rhbMeasureSchema = measureRhbWorkspace.getGMLSchema();
    final IFeatureType sealingFT = rhbMeasureSchema.getFeatureType( new QName( MeasuresConstants.NS_MEASURES_RHB, MeasuresConstants.RHB_MEASURE_FT ) );
    final Feature[] measureRhbFEs = measureRhbWorkspace.getFeatures( sealingFT );
    // final FeatureList measureRhbFEs = (FeatureList) measureRhbWorkspace.getFeatureFromPath( MeasuresConstants. );
    if( measureRhbFEs.length == 0 )
    {
      logger.info( "measure " + MeasuresConstants.RHB_MEASURE_FT + " is empty, continue normal simulation without this measure" );
      return;
    }
    final FeatureList storageChannelList = (FeatureList) modelWorkspace.getFeatureFromPath( "ChannelCollectionMember/channelMember[StorageChannel]" );
    final FeatureList catchementList = (FeatureList) modelWorkspace.getFeatureFromPath( "CatchmentCollectionMember/catchmentMember" );
    // for geometry operations modelWorkspace and rhbMeasureWorkspace must use the same coordinatessystem,
    // so lets transform the measures workspace to the one rrm model uses. (less work than other way)
    if( storageChannelList.size() > 0 )
    {
      final Feature feature = (Feature) storageChannelList.get( 0 );
      final GM_Object geom = feature.getGeometryProperties()[0];
      final CS_CoordinateSystem targetCS = geom.getCoordinateSystem();
      final TransformVisitor visitor = new TransformVisitor( targetCS );
      measureRhbWorkspace.accept( visitor, "/", FeatureVisitor.DEPTH_INFINITE );
    }
    for( int i = 0; i < measureRhbFEs.length; i++ )
    {
      final Feature measureRhbFE = measureRhbFEs[i];
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
        GM_Object catchmentGEOM = (GM_Object) catchment.getProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.CATCHMENT_GEOM_PROP ) );
        if( catchmentGEOM.contains( measureRhbGEOM ) )
        {
          boolean succseeded = MeasuresHelper.addRHBinCatchment( modelWorkspace, catchment, storageChannelList, measureRhbFE );
          if( !succseeded )
            c_error++;
          else
            c_success++;
        }
      }
      logger.info( "Fehler R¸ckhaltebecken Measure: " + c_error + "/" + (c_success + c_error) + "\n" );
    }

  }

  private void insertSwaleTrenchMeasure( final URL measuresMrsURL, GMLWorkspace naModelWorkspace, GMLWorkspace hydrotopWorkspace, final URL designAreaURL, final Logger logger ) throws Exception
  {
    try
    {
      final GMLWorkspace mrsMeasureWorkspace = GmlSerializer.createGMLWorkspace( measuresMrsURL );
      final IGMLSchema mrsMeasureSchema = mrsMeasureWorkspace.getGMLSchema();
      final IFeatureType mrsMeasureFT = mrsMeasureSchema.getFeatureType( new QName( MeasuresConstants.NS_MEASURES_MRS, MeasuresConstants.MRS_MEASURE_FT ) );
      final Feature[] mrsMeasureFEs = mrsMeasureWorkspace.getFeatures( mrsMeasureFT );
      if( mrsMeasureFEs.length == 0 )
      {
        logger.info( "measure " + MeasuresConstants.MRS_MEASURE_FT + " is empty, continue normal simulation without this measure" );
        return;
      }
      /** Create a swale and trench collection if it does not exist */
      final IGMLSchema naModelSchema = naModelWorkspace.getGMLSchema();
      final Feature naModelRoot = naModelWorkspace.getRootFeature();
      final IFeatureType naModelRootFt = naModelRoot.getFeatureType();
      Feature swaleTrenchCollection = (Feature) naModelWorkspace.getFeatureFromPath( NaModelConstants.MRS_COLLECTION_MEMBER_PROP );
      if( swaleTrenchCollection == null )
      {
        final IFeatureType swaleTrenchColFt = naModelSchema.getFeatureType( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_COLLECTION_FT ) );
        swaleTrenchCollection = naModelWorkspace.createFeature( naModelRoot, swaleTrenchColFt );
        final IRelationType linkPropertyCol = (IRelationType) naModelRootFt.getProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_COLLECTION_MEMBER_PROP ) );
        naModelWorkspace.addFeatureAsComposition( naModelRoot, linkPropertyCol, 0, swaleTrenchCollection );
      }
      // get the necessary featureTypes and proptertyTypes for the swale and trench element
      final IFeatureType swaleTrenchCollectionFT = swaleTrenchCollection.getFeatureType();
      final IRelationType linkPropertyStMemeber = (IRelationType) swaleTrenchCollectionFT.getProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_MEMBER_PROP ) );
      final IFeatureType mrsFt = naModelSchema.getFeatureType( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_FT ) );

      /** Start inserting bussines */
      final GMLWorkspace designAreaWorkspace = GmlSerializer.createGMLWorkspace( designAreaURL );
      final FeatureList catchementList = (FeatureList) naModelWorkspace.getFeatureFromPath( "CatchmentCollectionMember/catchmentMember" );
      if( catchementList.size() > 0 )
      {
        // assure that the measures and designArea geometries have the same coordinate system like the model data
        final Feature catchment = (Feature) catchementList.iterator().next();
        final GM_Object targetGeom = (GM_Object) catchment.getProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.CATCHMENT_GEOM_PROP ) );
        final CS_CoordinateSystem targetCS = targetGeom.getCoordinateSystem();
        final TransformVisitor visitor = new TransformVisitor( targetCS );
        mrsMeasureWorkspace.accept( visitor, "/", FeatureVisitor.DEPTH_INFINITE );
        designAreaWorkspace.accept( visitor, "/", FeatureVisitor.DEPTH_INFINITE );
      }
      int c_success = 0;
      int c_error = 0;
      for( int i = 0; i < mrsMeasureFEs.length; i++ )
      {
        final Feature mrsMeasure = mrsMeasureFEs[i];
        final Double percentage = FeatureHelper.getAsDouble( mrsMeasure, new QName( MeasuresConstants.NS_MEASURES_MRS, MeasuresConstants.MRS_MEASURE_PROP_PERCENTAGE ), 0d );
        final GM_Envelope envMeasure = mrsMeasure.getEnvelope();
        final GM_Object mrsGEOM = (GM_Object) mrsMeasure.getProperty( new QName( MeasuresConstants.NS_MEASURES_MRS, MeasuresConstants.MRS_MEASURE_GEOMETRY_PROP ) );
        final Geometry mrsJTS = JTSAdapter.export( mrsGEOM );
        final List<JMSpatialIndex> queriedCatchmentList = catchementList.query( envMeasure, null );
        for( Iterator iter = queriedCatchmentList.iterator(); iter.hasNext(); )
        {
          final Feature catchmentInEnv = (Feature) iter.next();
          final GM_Object catchmentGEOM = (GM_Object) catchmentInEnv.getProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.CATCHMENT_GEOM_PROP ) );
          try
          {
            final Geometry catchmentJTS = JTSAdapter.export( catchmentGEOM );
            final Geometry intersection = catchmentJTS.intersection( mrsJTS );
            if( !intersection.isEmpty() )
            {
              // splits the measure according to the catchment boundary into sub measures
              final GM_Object mrsSubMeasure = JTSAdapter.wrap( intersection );
              // get set properties from the measure element to transfer them to the newly create model element
              final Double diameter = FeatureHelper.getAsDouble( mrsMeasure, new QName( MeasuresConstants.NS_MEASURES_MRS, MeasuresConstants.MRS_MEASURE_PROP_DIAMETER ), 250 );
              final Double width = FeatureHelper.getAsDouble( mrsMeasure, new QName( MeasuresConstants.NS_MEASURES_MRS, MeasuresConstants.MRS_MEASURE_PROP_WIDTH ), 1.50 );
              final Feature swaleTrenchFE = naModelWorkspace.createFeature( swaleTrenchCollection, mrsFt );
              swaleTrenchFE.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_DIAMETER_PIPE_PROP ), diameter );
              swaleTrenchFE.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_WIDTH_PROP ), width );
              swaleTrenchFE.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_GEOM_PROP ), mrsSubMeasure );
              // this parameter is not used at the moment left in for
              swaleTrenchFE.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_KF_PIPE_PROP ), new Double( 0 ) );
              swaleTrenchFE.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_SLOPE_PROP ), new Double( MeasuresConstants.MRS_DEFAULT_SLOPE_PROP ) );
              swaleTrenchFE.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_ROUGHNESS_PIPE_PROP ), new Double( MeasuresConstants.MRS_DEFAULT_ROUGHNESS_PIPE_PROP ) );
              swaleTrenchFE.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_INFLOW_GW_PROP ), new Double( MeasuresConstants.MRS_DEFAULT_INFLOW_GW_PROP ) );
              swaleTrenchFE.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_MAX_PERK_PROP ), new Double( MeasuresConstants.MRS_DEFAULT_MAX_PERK_PROP ) );
              swaleTrenchFE.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_LANDUSE_TYPE_PROP ), NaModelConstants.DEFAULT_MRS_LANDUSE_PROP );
              swaleTrenchFE.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.MRS_SOIL_PROFIL_TYPE_PROP ), NaModelConstants.DEFAULT_MRS_SOIL_PROFIL_PROP );
              naModelWorkspace.addFeatureAsComposition( swaleTrenchCollection, linkPropertyStMemeber, 0, swaleTrenchFE );
              // add catchment that intersects with mrs-Element for later handling
              c_success++;
            }
          }
          catch( Exception e )
          {
            c_error++;
          }
        }
        logger.info( "Fehler Mulden-Rigolen Measure: " + c_error + "/" + (c_success + c_error) + "\n" );
        setHydroTypsForSwaleAndTrenchElement( designAreaWorkspace, percentage, (LineString) mrsJTS, hydrotopWorkspace );
      }

    }
    catch( Exception e )
    {
      logger.warning( "Was not able to merge swale and trench measures....skipped!" );
      throw new Exception( "Was not able to merge swale and trench measures....skipped!", e );
    }

  }

  private void setHydroTypsForSwaleAndTrenchElement( final GMLWorkspace designAreaWorkspace, final Double percentage, final LineString mrsJTS, final GMLWorkspace hydrotopWorkspace ) throws Exception
  {
    /** calculate puffer area for hydTyps */
    final FeatureList designAreaList = (FeatureList) designAreaWorkspace.getFeatureFromPath( MeasuresConstants.DESIGNAREA_MEMBER_PROP );
    // just get first area of planning (there should only be one element in the List)
    final Feature designAreaFe = (Feature) designAreaList.iterator().next();
    final GM_Object designAreaGEOM = (GM_Object) designAreaFe.getProperty( new QName( MeasuresConstants.NS_DESIGNAREA, MeasuresConstants.DESINGAREA_GEOM_PROP ) );
    final Geometry designAreaJTS = JTSAdapter.export( designAreaGEOM );
    int points = mrsJTS.getNumPoints();
    Point point1 = mrsJTS.getStartPoint();
    Point point2 = mrsJTS.getPointN( 2 );
    Point pointN = mrsJTS.getEndPoint();
    Point pointNminus1 = mrsJTS.getPointN( points - 1 );

    // calculate buffer width
    final double totalLength = mrsJTS.getLength();
    final double area = designAreaJTS.getArea();
    final double bufferWidth = area * percentage.doubleValue() / 100 / totalLength / 2;

    // cut off half circle at the end of the
    final Geometry bufferedArea = mrsJTS.buffer( bufferWidth );

    final GM_Object geomBufferdArea = JTSAdapter.wrap( bufferedArea );
    final FeatureList hydroList = (FeatureList) hydrotopWorkspace.getFeatureFromPath( NaModelConstants.HYDRO_MEMBER );
    final List queriedHydroList = hydroList.query( geomBufferdArea.getEnvelope(), null );
    for( Iterator iter = queriedHydroList.iterator(); iter.hasNext(); )
    {
      final Feature hydFe = (Feature) iter.next();
      final GM_Object hydGeom = (GM_Object) hydFe.getProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_GEOM ) );
      final Geometry hydGeomInEnv = JTSAdapter.export( hydGeom );
      if( hydGeomInEnv.intersects( bufferedArea ) )
        hydFe.setProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_HYDTYPE ), NaModelConstants.HYDRO_ENUM_HYDTYPE_SWALETRENCH );
    }
  }

  private double getBufferMask( Point p11, Point p12, Point p21, Point p22, double buffer )
  {
    double x11 = p11.getCoordinate().x;
    double x12 = p12.getCoordinate().x;
    double y11 = p11.getCoordinate().y;
    double y12 = p12.getCoordinate().y;
    double slope1 = y11 - y12 / x11 - x12;

    double x21 = p21.getCoordinate().x;
    double x22 = p22.getCoordinate().x;
    double y21 = p21.getCoordinate().y;
    double y22 = p22.getCoordinate().y;
    double slope2 = y21 - y22 / x21 - x22;

    return 1;
  }
}
