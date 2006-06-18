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
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import org.apache.commons.io.IOUtils;
import org.apache.commons.io.filefilter.IOFileFilter;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.commons.xml.NS;
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
import org.kalypsodeegree_impl.model.feature.visitors.SetPropertyFeatureVisitor;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.kalypsodeegree_impl.tools.GeometryUtilities;
import org.opengis.cs.CS_CoordinateSystem;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.TopologyException;

/**
 * @author kuepfer
 */
public class KalypsoDssCalcJob implements ISimulation
{

  private HashSet<NaModelCalcJob> m_naCalcJobs = new HashSet<NaModelCalcJob>();

  private File m_resultDirDSS = null;

  List<String> m_featruesWithResults = new ArrayList<String>();

  /** assures to return only directories for a file.list() call */
  // private FileFilter m_dirFilter = new FileFilter();
  ISimulationResultEater m_resultEater;

  /**
   * @see org.kalypso.simulation.core.ISimulation#run(java.io.File, org.kalypso.simulation.core.ISimulationDataProvider,
   *      org.kalypso.simulation.core.ISimulationResultEater, org.kalypso.simulation.core.ISimulationMonitor)
   */
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    final Logger logger = Logger.getAnonymousLogger();
    m_resultEater = resultEater;
    m_resultDirDSS = new File( tmpdir, "dssResults" );
    m_resultDirDSS.mkdirs();

    // create baseDir for hqJobs
    final File hqJobsBaseDir = new File( tmpdir, "hqJobs" );
    hqJobsBaseDir.mkdirs();

    // read control for optimize and requested HQs
    final URL controlURL = (URL) inputProvider.getInputForID( MeasuresConstants.IN_METADATA_ID );

    // TODO implement optimizing job
    boolean optimize = false;
    String choiseCalcCase = null;
    try
    {
      // read .calculation
      final GMLWorkspace control = GmlSerializer.createGMLWorkspace( controlURL );
      final Feature rootFeatureControl = control.getRootFeature();
      choiseCalcCase = (String) rootFeatureControl.getProperty( new QName( MeasuresConstants.NS_MESURESMETA, MeasuresConstants.METADATA_CALCCASE_PROP ) );
      optimize = FeatureHelper.booleanIsTrue( rootFeatureControl, new QName( MeasuresConstants.NS_MESURESMETA, MeasuresConstants.METADATA_OPTIMIZE_PROP ), false );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      // fallback to default calculate all HQs
      choiseCalcCase = MeasuresConstants.METADATA_CALCCASE_ENUM_ALL;
    }

    final URL calcCases = getClass().getResource( "../resources/hq1_bis_hq100.zip" );

    // list of dataproviders for each hq-case
    final ArrayList<CalcDataProviderDecorater> dataProvider = new ArrayList<CalcDataProviderDecorater>();

    // HQs to calculate
    try
    {
      // unzip all available calc cases hq1 to hq100
      monitor.setMessage( "Vorbereiten des Basismodels" );
      if( monitor.isCanceled() )
        return;
      // unpack all HQs
      ZipUtilities.unzip( calcCases.openStream(), hqJobsBaseDir );
      logger.info( " ...calc cases successfully unziped" );
      monitor.setMessage( "Model ist initialisiert.." );
      if( monitor.isCanceled() )
        return;

      //  
      final FileFilter m_dirFilter = new FileFilter();
      if( !choiseCalcCase.equals( MeasuresConstants.METADATA_CALCCASE_ENUM_ALL ) )
        m_dirFilter.add( new String[] { choiseCalcCase } );
      final String[] hqIdentifierToCalculate = hqJobsBaseDir.list( m_dirFilter );
      final boolean doMeasures;

      if( inputProvider.hasID( MeasuresConstants.IN_CALC_DO_MEASURE_SWITCH ) )
      {
        final URL calcSwitchURL = (URL) inputProvider.getInputForID( MeasuresConstants.IN_CALC_DO_MEASURE_SWITCH );
        final StringWriter writer = new StringWriter();
        InputStream inputStream = null;
        try
        {
          inputStream = calcSwitchURL.openStream();
          IOUtils.copy( inputStream, writer );
          String string = writer.toString();
          doMeasures = string.indexOf( "yes" ) >= 0;
        }
        finally
        {
          IOUtils.closeQuietly( inputStream );
        }
      }
      else
        doMeasures = false;

      final List<HTMLFragmentBean> htmlFragmentBeans = new ArrayList<HTMLFragmentBean>();
      final String tmpdirAsString = hqJobsBaseDir.toString();
      // 
      for( int i = 0; i < hqIdentifierToCalculate.length; i++ )
      {
        final String hqIdentifier = hqIdentifierToCalculate[i];
        final File baseDir = new File( tmpdirAsString + "\\" + hqIdentifier );
        final NaSimulationDataProvieder naSimulationDataProvider = new NaSimulationDataProvieder( baseDir );
        final CalcDataProviderDecorater rrmInputProvider = new CalcDataProviderDecorater( naSimulationDataProvider );

        monitor.setMessage( "Füge die Maßnahmen in das Model ein..." );
        // insert measures defined from the client application
        final List<Feature> resultNodes = mergeMeasures( inputProvider, rrmInputProvider, doMeasures, logger );
        final File dssResultDirRun = new File( m_resultDirDSS, hqIdentifier );

        dssResultDirRun.mkdirs();
        final ISimulationResultEater naJobResultEater = new ISimulationResultEater()
        {
          public void addResult( final String id, final File file )
          {
            if( id.equals( NaModelConstants.OUT_ZML ) )
            {
              final File rrmResultDir = file;
              // try
              // {

              for( final Feature resultNode : resultNodes )
              {

                try
                {
                  FlowsDSSResultGenerator.generateDssResultFor( dssResultDirRun, rrmResultDir, inputProvider, hqIdentifier, resultNode, doMeasures, htmlFragmentBeans );
                }
                catch( MalformedURLException e )
                {
                  e.printStackTrace();
                }
              }
              // final ResultFileFilter filter = new ResultFileFilter( m_featruesWithResults );
              // FileUtils.listFiles( file, filter, TrueFileFilter.INSTANCE );
              // File[] resultfiles = filter.getMatchingFiles();
              // for( int j = 0; j < resultfiles.length; j++ )
              // {
              // final File f = resultfiles[j];
              // FileUtils.copyDirectoryToDirectory( f, resultDirRun );
              // }
              // }
              // catch( IOException e )
              // {
              // e.printStackTrace();
              // }
            }
          }

        };

        monitor.setMessage( "Maßnahmen erfogreich in das Model eingefügt..." );
        final NaModelCalcJob calcJob = new NaModelCalcJob();
        m_naCalcJobs.add( calcJob );
        final File calcDirUrl = (File) rrmInputProvider.getInputForID( NaSimulationDataProvieder.CALC_DIR );
        monitor.setMessage( "Starte Berechnungdurchlauf " + i + " von " + dataProvider.size() );
        calcJob.run( calcDirUrl, rrmInputProvider, naJobResultEater, monitor );
        monitor.setMessage( "Berechnungslauf " + i + " beendet" );
      }
      final File analyseFile = new File( m_resultDirDSS, "analyse.html" );
      final File analyseHQFile = new File( m_resultDirDSS, "analyseHQ.html" );
      FlowsDSSResultGenerator.generateHTMLFormFragments( analyseFile, htmlFragmentBeans, true );
      FlowsDSSResultGenerator.generateHTMLFormFragments( analyseHQFile, htmlFragmentBeans, false );
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
    // final Iterator<CalcDataProviderDecorater> iterator = dataProvider.iterator();
    // int i = 1;
    // while( iterator.hasNext() )
    // {
    // final CalcDataProviderDecorater rrmInputProvider = iterator.next();
    // final File resultDirRun = new File( m_resultDirDSS, hqIdentifierToCalculate[i - 1] );
    // resultDirRun.mkdirs();
    // final ISimulationResultEater naJobResultEater = new ISimulationResultEater()
    // {
    //
    // public void addResult( String id, File file )
    // {
    // if( id.equals( NaModelConstants.OUT_ZML ) )
    // {
    // try
    // {
    // final Feature[] resultNodes = new Feature[0];
    // for( final Feature resultNode : resultNodes )
    // {
    // generateDssResultFor( file, resultNode );
    // }
    //
    // final ResultFileFilter filter = new ResultFileFilter( m_featruesWithResults );
    // FileUtils.listFiles( file, filter, TrueFileFilter.INSTANCE );
    // File[] resultfiles = filter.getMatchingFiles();
    // for( int j = 0; j < resultfiles.length; j++ )
    // {
    // final File f = resultfiles[j];
    // FileUtils.copyDirectoryToDirectory( f, resultDirRun );
    // }
    // }
    // catch( IOException e )
    // {
    // e.printStackTrace();
    // }
    // }
    // }
    //
    // };
    //
    // monitor.setMessage( "Füge die Maßnahmen in das Model ein..." );
    // // insert measures defined from the client application
    // mergeMeasures( inputProvider, rrmInputProvider, logger );
    // monitor.setMessage( "Maßnahmen erfogreich in das Model eingefügt..." );
    // final NaModelCalcJob calcJob = new NaModelCalcJob();
    // m_naCalcJobs.add( calcJob );
    // final File calcDirUrl = (File) rrmInputProvider.getInputForID( NaSimulationDataProvieder.CALC_DIR );
    // monitor.setMessage( "Starte Berechnungdurchlauf " + i + " von " + dataProvider.size() );
    // calcJob.run( calcDirUrl, rrmInputProvider, naJobResultEater, monitor );
    // monitor.setMessage( "Berechnungslauf " + i + " beendet" );
    // i++;
    // }
    resultEater.addResult( NaModelConstants.OUT_ZML, m_resultDirDSS );
  }

  private List<Feature> mergeMeasures( final ISimulationDataProvider dssInputProvider, CalcDataProviderDecorater rrmInputProvider, final boolean doMeasures, Logger logger ) throws SimulationException
  {
    final List<Feature> resultNodes = new ArrayList<Feature>();
    boolean writeNewHydrotopFile = false;
    boolean writeNewInitalValueFile = false;
    // get the urls for measures and model files

    final URL measuresRhbURL;
    if( dssInputProvider.hasID( MeasuresConstants.IN_MEASURE_RHB_ID ) )
      measuresRhbURL = (URL) dssInputProvider.getInputForID( MeasuresConstants.IN_MEASURE_RHB_ID );
    else
      measuresRhbURL = null;

    final URL measuresSealingURL;
    if( dssInputProvider.hasID( MeasuresConstants.IN_MEASURE_SEALING_ID ) )
      measuresSealingURL = (URL) dssInputProvider.getInputForID( MeasuresConstants.IN_MEASURE_SEALING_ID );
    else
      measuresSealingURL = null;

    final URL measuresMrsURL;
    if( dssInputProvider.hasID( MeasuresConstants.IN_MEASURE_MRS_ID ) )
      measuresMrsURL = (URL) dssInputProvider.getInputForID( MeasuresConstants.IN_MEASURE_MRS_ID );
    else
      measuresMrsURL = null;

    final URL designAreaURL;
    if( dssInputProvider.hasID( MeasuresConstants.IN_DESIGN_AREA_ID ) )
      designAreaURL = (URL) dssInputProvider.getInputForID( MeasuresConstants.IN_DESIGN_AREA_ID );
    else
      designAreaURL = null;

    final URL planningMeasureURL;
    if( dssInputProvider.hasID( MeasuresConstants.IN_MEASURE_PLANNING_ID ) )
      planningMeasureURL = (URL) dssInputProvider.getInputForID( MeasuresConstants.IN_MEASURE_PLANNING_ID );
    else
      planningMeasureURL = null;

    final URL modelURL = (URL) rrmInputProvider.getInputForID( NaModelConstants.IN_MODELL_ID );
    final URL hydrotopURL = (URL) rrmInputProvider.getInputForID( NaModelConstants.IN_HYDROTOP_ID );
    final URL initValueURL = (URL) rrmInputProvider.getInputForID( NaModelConstants.LZSIM_IN_ID );
    final File calcDir = (File) rrmInputProvider.getInputForID( NaSimulationDataProvieder.CALC_DIR );

    GMLWorkspace modelWorkspace = null;
    GMLWorkspace hydrotopWorkspace = null;
    GMLWorkspace initValuesWorkspace = null;
    try
    {
      modelWorkspace = GmlSerializer.createGMLWorkspace( modelURL );
      if( planningMeasureURL != null )
      {

        hydrotopWorkspace = GmlSerializer.createGMLWorkspace( hydrotopURL );
        initValuesWorkspace = GmlSerializer.createGMLWorkspace( initValueURL );
        // insert measure
        insertPlanningMeasure( planningMeasureURL, hydrotopWorkspace, initValuesWorkspace, rrmInputProvider, logger );
        writeNewHydrotopFile = true;
        writeNewInitalValueFile = true;
      }
      if( measuresRhbURL != null && doMeasures )
      {
        insertStorageChannelMeasure( measuresRhbURL, modelWorkspace, logger );
      }
      if( measuresSealingURL != null && doMeasures )
      {
        if( hydrotopWorkspace == null )
          hydrotopWorkspace = GmlSerializer.createGMLWorkspace( hydrotopURL );
        insertSealingChangeMeasure( measuresSealingURL, hydrotopWorkspace, rrmInputProvider, logger );
        writeNewHydrotopFile = true;
      }
      if( measuresMrsURL != null && doMeasures )
      {
        insertSwaleTrenchMeasure( measuresMrsURL, modelWorkspace, hydrotopWorkspace, designAreaURL, logger );
        writeNewHydrotopFile = true;
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
      logger.info( "Problems while merging measures with modell data - " + e.getLocalizedMessage() );
      throw new SimulationException( e.getMessage(), e );
    }
    try
    {
      // set all generate result properties to false
      setAllResultFlags( modelWorkspace, false );
      final Feature[] affectedChannels = getAffectedChannels( modelWorkspace, designAreaURL );
      for( final Feature channelFE : affectedChannels )
      {
        final IRelationType nodelinkRT = (IRelationType) channelFE.getFeatureType().getProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.LINK_CHANNEL_DOWNSTREAMNODE ) );
        final Feature nodeFE = modelWorkspace.resolveLink( channelFE, nodelinkRT );
        nodeFE.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.GENERATE_RESULT_PROP ), Boolean.TRUE );
        if( !resultNodes.contains( nodeFE ) )
          resultNodes.add( nodeFE );
      }

      // setFeaturesWithResults( modelWorkspace, affectedChannels );
      // the calcCase always generates results for all elements in the rrm
      // setAllResultFlags( modelWorkspace, true );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      // setAllResultFlags( modelWorkspace, true );
    }

    Writer writerHydroTop = null;
    Writer writerModel = null;
    Writer writerInitValue = null;
    try
    {
      try
      {
        /**
         * 1. write the changed data to a new rrm, hydrotop and inital value file <br>
         * 2. replacing in the bean the old URL to the new files
         */
        // model file
        final File modelFile = File.createTempFile( "measured_model", ".gml", calcDir );
        writerModel = new BufferedWriter( new OutputStreamWriter( new FileOutputStream( modelFile ), MeasuresConstants.DEFAULT_ENCONDING ) );
        GmlSerializer.serializeWorkspace( writerModel, modelWorkspace, MeasuresConstants.DEFAULT_ENCONDING );
        rrmInputProvider.addURL( NaModelConstants.IN_MODELL_ID, modelFile.toURL() );
        // inital value file
        if( writeNewInitalValueFile )
        {
          final File initValueFile = File.createTempFile( "measured_initValues", ".gml", calcDir );
          writerInitValue = new BufferedWriter( new OutputStreamWriter( new FileOutputStream( initValueFile ), MeasuresConstants.DEFAULT_ENCONDING ) );
          GmlSerializer.serializeWorkspace( writerInitValue, initValuesWorkspace, MeasuresConstants.DEFAULT_ENCONDING );
          rrmInputProvider.addURL( NaModelConstants.LZSIM_IN_ID, initValueFile.toURL() );
        }
        // hydrotop file
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
        IOUtils.closeQuietly( writerModel );
        IOUtils.closeQuietly( writerInitValue );
        IOUtils.closeQuietly( writerHydroTop );
      }

    }
    catch( Exception e )
    {
      logger.warning( "There have been problems serializing the measured model and hydrotop files -- The model is calcuate without any measures" );
      rrmInputProvider.addURL( NaModelConstants.IN_MODELL_ID, modelURL );
      rrmInputProvider.addURL( NaModelConstants.IN_HYDROTOP_ID, hydrotopURL );
    }
    return resultNodes;
  }

  private Feature[] getAffectedChannels( GMLWorkspace modelWorkspace, URL designAreaURL ) throws Exception
  {
    final GMLWorkspace designArea = GmlSerializer.createGMLWorkspace( designAreaURL );
    FeatureList designAreaFEs = (FeatureList) designArea.getFeatureFromPath( MeasuresConstants.DESIGNAREA_MEMBER_PROP );
    Feature designAreaFE = (Feature) designAreaFEs.get( 0 );
    final FeatureList catchmentList = (FeatureList) modelWorkspace.getFeatureFromPath( "CatchmentCollectionMember/catchmentMember" );
    final ArrayList<Feature> channelCollector = new ArrayList<Feature>();
    final List catchments = catchmentList.query( designAreaFE.getEnvelope(), null );
    for( Iterator iter = catchments.iterator(); iter.hasNext(); )
    {
      final Feature f = (Feature) iter.next();
      final IFeatureType featureType = f.getFeatureType();
      final IRelationType linkCatchmentChannel = (IRelationType) featureType.getProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.LINK_CATCHMENT_CHANNEL ) );
      final Feature resolvedLink = modelWorkspace.resolveLink( f, linkCatchmentChannel );
      if( resolvedLink != null )
        channelCollector.add( resolvedLink );
    }
    return (Feature[]) channelCollector.toArray( new Feature[channelCollector.size()] );
  }

  private void insertPlanningMeasure( final URL measuresPlanningURL, final GMLWorkspace hydrotopWorkspace, final GMLWorkspace initValuesWorkspace, final CalcDataProviderDecorater rrmInputProvider, final Logger logger ) throws Exception
  {
    final URL paramURL = (URL) rrmInputProvider.getInputForID( NaModelConstants.IN_PARAMETER_ID );
    final URL initValuesURL = (URL) rrmInputProvider.getInputForID( NaModelConstants.LZSIM_IN_ID );
    final GMLWorkspace paraWorkspace = GmlSerializer.createGMLWorkspace( paramURL );
    final GMLWorkspace planningWorkspace = GmlSerializer.createGMLWorkspace( measuresPlanningURL );
    final IGMLSchema planningSchema = planningWorkspace.getGMLSchema();
    final QName qNameGeom = new QName( MeasuresConstants.NS_XPLANUNG, MeasuresConstants.XPLANUNG_GEOM_PROP );
    final QName qNameGRZ = new QName( MeasuresConstants.NS_XPLANUNG, MeasuresConstants.XPLANUNG_GRZ_PROP );
    // get all hydros in the model
    FeatureList hydroList = (FeatureList) hydrotopWorkspace.getFeatureFromPath( NaModelConstants.HYDRO_MEMBER );
    // transform planning workspace coordinate system to match coordinate system of hydrotop files
    if( hydroList.size() > 0 )
    {
      final Feature feature = (Feature) hydroList.get( 0 );
      final GM_Object geom = feature.getGeometryProperties()[0];
      final CS_CoordinateSystem targetCS = geom.getCoordinateSystem();
      final TransformVisitor visitor = new TransformVisitor( targetCS );
      planningWorkspace.accept( visitor, "/", FeatureVisitor.DEPTH_INFINITE );
    }
    // handel GruenFlaech features
    final IFeatureType gruenflFT = planningSchema.getFeatureType( new QName( MeasuresConstants.NS_XPLANUNG, MeasuresConstants.XPLANUNG_GRUENFL_FT ) );
    final Feature[] gruenflFE = planningWorkspace.getFeatures( gruenflFT );
    for( int i = 0; i < gruenflFE.length; i++ )
    {
      final Feature gfFE = gruenflFE[i];
      setNewLandUseAndCorrSealingFactor( gfFE, qNameGRZ, qNameGeom, hydrotopWorkspace, paraWorkspace, initValuesWorkspace, MeasuresConstants.XPLANUNG_GRUENFL_LANDUSE_NAME, logger );
    }
    // handel VerkehrsFlaech features
    final IFeatureType verkehrsFlaechFT = planningSchema.getFeatureType( new QName( MeasuresConstants.NS_XPLANUNG, MeasuresConstants.XPLANUNG_VERKEHRSFL_FT ) );
    final Feature[] verkersFEs = planningWorkspace.getFeatures( verkehrsFlaechFT );
    for( int i = 0; i < verkersFEs.length; i++ )
    {
      final Feature vfFE = verkersFEs[i];
      setNewLandUseAndCorrSealingFactor( vfFE, qNameGRZ, qNameGeom, hydrotopWorkspace, paraWorkspace, initValuesWorkspace, MeasuresConstants.XPLANUNG_VERKEHRSFL_LANDUSE_NAME, logger );
    }
    // handel VerkehrsflaecheBesondererZweckbestimmung features
    final IFeatureType verkehrsflaechBesZweckFT = planningSchema.getFeatureType( new QName( MeasuresConstants.NS_XPLANUNG, MeasuresConstants.XPLANUNG_VERKEHRSFMITBESZWECK_FT ) );
    final Feature[] verkehrsflaechBesZweckFEs = planningWorkspace.getFeatures( verkehrsflaechBesZweckFT );
    for( int i = 0; i < verkehrsflaechBesZweckFEs.length; i++ )
    {
      final Feature vfBZbFE = verkehrsflaechBesZweckFEs[i];
      setNewLandUseAndCorrSealingFactor( vfBZbFE, qNameGRZ, qNameGeom, hydrotopWorkspace, paraWorkspace, initValuesWorkspace, MeasuresConstants.XPLANUNG_VERKEHRSFL_LANDUSE_NAME, logger );
    }
    // handel GemeinbedarfsFlaeche features
    final IFeatureType gemeinBedFlaechFT = planningSchema.getFeatureType( new QName( MeasuresConstants.NS_XPLANUNG, MeasuresConstants.XPLANUNG_GEMEINBED_FT ) );
    final Feature[] gemeinBedFlaechFEs = planningWorkspace.getFeatures( gemeinBedFlaechFT );
    for( int i = 0; i < gemeinBedFlaechFEs.length; i++ )
    {
      final Feature gbFE = gemeinBedFlaechFEs[i];
      setNewLandUseAndCorrSealingFactor( gbFE, qNameGRZ, qNameGeom, hydrotopWorkspace, paraWorkspace, initValuesWorkspace, MeasuresConstants.XPLANUNG_GEMEINBED_LANDUSE_NAME, logger );
    }
    // handel BaugebietsFlaechenTeil (Baugebiete) features
    final IFeatureType bauGebFT = planningSchema.getFeatureType( new QName( MeasuresConstants.NS_XPLANUNG, MeasuresConstants.XPLANUNG_BAUGEBIET_FT ) );
    final Feature[] bauGebFEs = planningWorkspace.getFeatures( bauGebFT );
    for( int i = 0; i < bauGebFEs.length; i++ )
    {
      final Feature bgFE = bauGebFEs[i];
      final Object artBaulicherNutzung = bgFE.getProperty( new QName( MeasuresConstants.NS_XPLANUNG, MeasuresConstants.XPLANUNG_ART_BAULICHNUTZ_PROP ) );
      final String type;
      if( artBaulicherNutzung.equals( MeasuresConstants.XPLANUNG_RWG_PROP ) )
        type = MeasuresConstants.XPLANUNG_RWG_LANDUSE_NAME;
      else if( artBaulicherNutzung.equals( MeasuresConstants.XPLANUNG_AWG_PROP ) )
        type = MeasuresConstants.XPLANUNG_AWG_LANDUSE_NAME;
      else if( artBaulicherNutzung.equals( MeasuresConstants.XPLANUNG_BWG_PROP ) )
        type = MeasuresConstants.XPLANUNG_BWG_LANDUSE_NAME;
      else if( artBaulicherNutzung.equals( MeasuresConstants.XPLANUNG_DG_PROP ) )
        type = MeasuresConstants.XPLANUNG_DG_LANDUSE_NAME;
      else if( artBaulicherNutzung.equals( MeasuresConstants.XPLANUNG_GG_PROP ) || artBaulicherNutzung.equals( MeasuresConstants.XPLANUNG_IG_PROP ) )
        type = MeasuresConstants.XPLANUNG_IG_LANDUSE_NAME;
      else if( artBaulicherNutzung.equals( MeasuresConstants.XPLANUNG_KG_PROP ) )
        type = MeasuresConstants.XPLANUNG_KG_LANDUSE_NAME;
      else if( artBaulicherNutzung.equals( MeasuresConstants.XPLANUNG_MG_PROP ) )
        type = MeasuresConstants.XPLANUNG_MG_LANDUSE_NAME;
      else if( artBaulicherNutzung.equals( MeasuresConstants.XPLANUNG_SGE_PROP ) || artBaulicherNutzung.equals( MeasuresConstants.XPLANUNG_SGS_PROP ) )
        type = MeasuresConstants.XPLANUNG_SG_LANDUSE_NAME;
      else if( artBaulicherNutzung.equals( MeasuresConstants.XPLANUNG_KS_PROP ) )
        type = MeasuresConstants.XPLANUNG_KS_LANDUSE_NAME;
      else
        type = MeasuresConstants.XPLANUNG_UNDEFINED_LANDUSE_NAME;

      setNewLandUseAndCorrSealingFactor( bgFE, qNameGRZ, qNameGeom, hydrotopWorkspace, paraWorkspace, initValuesWorkspace, type, logger );
    }
  }

  /**
   * @see org.kalypso.simulation.core.ISimulation#getSpezifikation()
   */
  public URL getSpezifikation( )
  {
    return getClass().getResource( "../resources/dsscalcjob_spec.xml" );
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
      final GM_Envelope selENV = sealFE.getEnvelope();
      final List<JMSpatialIndex> hydrosInENV = hydroList.query( selENV, null );
      for( Iterator iter = hydrosInENV.iterator(); iter.hasNext(); )
      {
        final Feature hydroFE = (Feature) iter.next();
        final double originalSealingFactor = getSealingFactorForHydrotop( hydroFE, paraWorkspace ); // TODO abfangen
        // wenn

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

        final double sealingAreaIntersection = areaIntersection * sealMeasure * corrSealingHydroOld;
        final double sealingAreaOld = (areaHydro - areaIntersection) * originalSealingFactor * corrSealingHydroOld;
        final double virtualSealingFactor = (sealingAreaIntersection + sealingAreaOld) / areaHydro;
        double corrSealingHydroNew = virtualSealingFactor / (originalSealingFactor * corrSealingHydroOld);
        if( Double.isInfinite( corrSealingHydroNew ) )
        {
          corrSealingHydroNew = corrSealingHydroOld;
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
  private double getSealingFactorForHydrotop( final Feature hydroFE, final GMLWorkspace paramWorkspace )
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
      logger.info( "Fehler Rückhaltebecken Measure: " + c_error + "/" + (c_success + c_error) + "\n" );
    }

  }

  private void insertSwaleTrenchMeasure( final URL measuresMrsURL, final GMLWorkspace naModelWorkspace, final GMLWorkspace hydrotopWorkspace, final URL designAreaURL, final Logger logger ) throws Exception
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
    if( designAreaList.size() == 0 )
    {
      // logger.info( "no design area defined can not insert swale and trench meausre!..skipped" );
      return;
    }
    // just get first area of planning (there should only be one element in the List)
    final Feature designAreaFe = (Feature) designAreaList.iterator().next();
    final GM_Object designAreaGEOM = (GM_Object) designAreaFe.getProperty( new QName( MeasuresConstants.NS_DESIGNAREA, MeasuresConstants.DESINGAREA_GEOM_PROP ) );
    final Geometry designAreaJTS = JTSAdapter.export( designAreaGEOM );
    // calculate buffer width
    final double totalLength = mrsJTS.getLength();
    final double area = designAreaJTS.getArea();
    final double bufferWidth = area * percentage.doubleValue() / 100 / totalLength / 2;

    // the buffered area is to large, this inaccuracy is neclegted
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

  private void setGenerateResults( GMLWorkspace modelWorkspace, URL designAreaURL ) throws Exception
  {
    final GMLWorkspace designAreaWorkspace = GmlSerializer.createGMLWorkspace( designAreaURL );
    final FeatureList designAreaList = (FeatureList) designAreaWorkspace.getFeatureFromPath( MeasuresConstants.DESIGNAREA_MEMBER_PROP );
    // always take the first geometry ( it is assumed that there is only one )
    final Feature designAreaFE = (Feature) designAreaList.get( 0 );
    final GM_Object designAreaGeom = (GM_Object) designAreaFE.getProperty( new QName( MeasuresConstants.NS_DESIGNAREA, MeasuresConstants.DESINGAREA_GEOM_PROP ) );
    final Geometry jtsDesignAreaGeom = JTSAdapter.export( designAreaGeom );
    final FeatureList catchmentList = (FeatureList) modelWorkspace.getFeatureFromPath( NaModelConstants.CATCHMENT_MEMBER_PROP );
    final List catchmentEnvList = catchmentList.query( designAreaFE.getEnvelope(), null );
    final List<Feature> affectedChannels = new ArrayList<Feature>();
    for( Iterator iter = catchmentEnvList.iterator(); iter.hasNext(); )
    {
      final Feature catchment = (Feature) iter.next();
      final IRelationType catchmentChannelLink = (IRelationType) catchment.getFeatureType().getProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.LINK_CATCHMENT_CHANNEL ) );
      final GM_Object catchmentGeom = (GM_Object) catchment.getProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.CATCHMENT_GEOM_PROP ) );
      final Geometry jtsCatchementGeom = JTSAdapter.export( catchmentGeom );
      if( jtsCatchementGeom.intersects( jtsDesignAreaGeom ) )
        affectedChannels.add( modelWorkspace.resolveLink( catchment, catchmentChannelLink ) );
    }

  }

  private void setNewLandUseAndCorrSealingFactor( final Feature planningMeasureFE, final QName sealingProp, final QName sealingGeom, final GMLWorkspace hydrotopWorkspace, final GMLWorkspace paraWorkspace, GMLWorkspace initValuesWorkspace, final String landUseProp, final Logger logger ) throws Exception
  {
    // init counter
    int c_success = 0;
    int c_error = 0;
    // find Landuse for measure
    final IGMLSchema parameterSchema = paraWorkspace.getGMLSchema();
    final IFeatureType landUseFT = parameterSchema.getFeatureType( new QName( NaModelConstants.NS_NAPARAMETER, NaModelConstants.PARA_LANDUSE_NAME ) );
    final Feature[] landUseFEs = paraWorkspace.getFeatures( landUseFT );
    final QName qNameLandUse = new QName( NS.GML3, NaModelConstants.GML_FEATURE_NAME_PROP );
    Feature landUseFE = null;
    for( int i = 0; i < landUseFEs.length; i++ )
    {
      landUseFE = landUseFEs[i];
      final Object property = landUseFE.getProperty( qNameLandUse );
      if( landUseProp.equals( property ) )
        break;
    }
    // get all hydrotopes in workspace
    final FeatureList hydroList = (FeatureList) hydrotopWorkspace.getFeatureFromPath( NaModelConstants.HYDRO_MEMBER );
    // do the intersecting business
    final GM_Object measureGEOM = (GM_Object) planningMeasureFE.getProperty( sealingGeom );
    final Geometry jtsMeasureGEOM = JTSAdapter.export( measureGEOM );
    final QName qNameHydroGeom = new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_GEOM );
    final List query = hydroList.query( measureGEOM.getEnvelope(), null );
    final Iterator iter = query.iterator();
    while( iter.hasNext() )
    {
      final Feature hydroFE = (Feature) iter.next();
      if( hydroFE == null )
        continue;
      final GM_Object hydroGEOM = (GM_Object) hydroFE.getProperty( qNameHydroGeom );
      final CS_CoordinateSystem storedCrs = hydroGEOM.getCoordinateSystem();
      final Geometry jtsHydroGEOM = JTSAdapter.export( hydroGEOM );
      Geometry intersection = null;
      Geometry difference = null;
      try
      {

        boolean intersects = jtsMeasureGEOM.intersects( jtsHydroGEOM );
        if( !intersects )
          continue;
        intersection = jtsMeasureGEOM.intersection( jtsHydroGEOM );
        // keep the diffrence from the original hydrotop
        difference = jtsHydroGEOM.difference( jtsMeasureGEOM );
        c_success++;
      }
      catch( TopologyException e )
      {
        c_error++;
        e.printStackTrace();
        logger.warning( "Fehler beim verschneiden der Topoloigien. HydrotopId=" + hydroFE.getId() + " mit measureId=" + planningMeasureFE.getId()
            + ".Es wird trotzdem weitergerechnet diese Operation wurde ingnorier!" );
        continue;
      }
      // get hydroCollection and add new hydroFE to collection
      final Feature hydroRootFeature = hydrotopWorkspace.getRootFeature();
      final IFeatureType rootFT = hydroRootFeature.getFeatureType();
      final IRelationType linkPropHydro = (IRelationType) rootFT.getProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_MEMBER ) );
      final IFeatureType hydroFT = hydrotopWorkspace.getFeatureTypeFromPath( NaModelConstants.HYDRO_MEMBER );
      final Feature newHydroFE = hydrotopWorkspace.createFeature( hydroRootFeature, hydroFT );
      hydrotopWorkspace.addFeatureAsComposition( hydroRootFeature, linkPropHydro, 0, newHydroFE );
      // copy all propterties from the intersected hydrotop to the newly created hydrotop
      FeatureHelper.copyNoRelationPropterty( hydroFE, newHydroFE );
      if( difference != null && !difference.isEmpty() )
      {
        final GM_Object geomWithOldParam = JTSAdapter.wrap( difference );
        // since JTS has no coordiante system we have to set the old one after a JTSAdapter call
        geomWithOldParam.setCoordinateSystem( storedCrs );
        // set the inverse Geometry of the intersection at the old hydrotop FE
        hydroFE.setProperty( qNameHydroGeom, GeometryUtilities.ensureIsMultiPolygon( geomWithOldParam ) );
      }
      // override copied props that are not valid anymore
      final GM_Object geomWithNewMeasure = JTSAdapter.wrap( intersection );
      // since JTS has no coordiante system we have to set the old one after a JTSAdapter call
      geomWithNewMeasure.setCoordinateSystem( storedCrs );
      /**
       * since the original sealing factor (from landuse feature) can not be alterd, the new sealing factor form the
       * measure is achieved by using the sealing correction factor at each hydrotop. It could be that a correction
       * factors has been used to calibrate the model, hence we have to incorporate the change of sealing and the old
       * correction factor into an new correction factor. Now do the business!
       */
      final IRelationType linkSealing = (IRelationType) landUseFE.getFeatureType().getProperty( new QName( NaModelConstants.NS_NAPARAMETER, NaModelConstants.PARA_LANDUSE_PROP_SEALING_LINK ) );
      final Feature landuseSealingFE = paraWorkspace.resolveLink( landUseFE, linkSealing );
      final double landuseSealingFactor = FeatureHelper.getAsDouble( landuseSealingFE, new QName( NaModelConstants.NS_NAPARAMETER, NaModelConstants.PARA_LANDUSE_PROP_SEALING ), 0.5d );
      double measureSealingFactor = FeatureHelper.getAsDouble( planningMeasureFE, sealingProp, landuseSealingFactor );
      final IFeatureType measureFT = planningMeasureFE.getFeatureType();
      final QName qnameMeasure = measureFT.getQName();
      // determine the 50% additional area für Nebenflächen und Stellplätze (Regel in Hamburg)
      // boolean plus50ProzentNebenflaechen = false;
      // if( qnameMeasure.equals( new QName( MeasuresConstants.NS_XPLANUNG, MeasuresConstants.XPLANUNG_BAUGEBIET_FT ) )
      // )
      // plus50ProzentNebenflaechen = true;
      // kappungsgrenze GRZ 0.8
      // if( plus50ProzentNebenflaechen )
      // {
      // double test = measureSealingFactor + measureSealingFactor * 0.5;
      // if( test < 0.8d )
      // measureSealingFactor = test;
      // else
      // measureSealingFactor = 0.8d;
      // }
      final double originalCorrFactor = FeatureHelper.getAsDouble( hydroFE, new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_SEAL_CORR_FACTOR ), 1.0d );
      final double newCorrectionFactor = originalCorrFactor * measureSealingFactor / landuseSealingFactor;
      newHydroFE.setProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_GEOM ), GeometryUtilities.ensureIsMultiPolygon( geomWithNewMeasure ) );
      newHydroFE.setProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_SEAL_CORR_FACTOR ), new Double( newCorrectionFactor ) );
      newHydroFE.setProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_AREA ), new Double( intersection.getArea() ) );
      newHydroFE.setProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_LANDUSE_NAME ), landUseFE.getProperty( new QName( NS.GML3, NaModelConstants.GML_FEATURE_NAME_PROP ) ) );
      newHydroFE.setProperty( new QName( NS.GML3, NaModelConstants.GML_FEATURE_DESCRIPTION_PROP ), "automatically generated hydrotop cloned from hydrotopID=" + hydroFE.getId() );
      setInitialValues( newHydroFE, hydroFE, initValuesWorkspace );
    }
    if( c_error > 0 )
      logger.info( "Fehler BPlan-Measure: " + c_error + " Fehler/" + (c_success + c_error) + " Total\n" );
    else
      logger.info( "BPlan-Maßnahme" + planningMeasureFE.getClass().toString() + " erfolgreich eingefügt!" );
  }

  private void setInitialValues( final Feature newHydroFE, final Feature hydroFE, final GMLWorkspace initValuesWorkspace ) throws Exception
  {
    final IGMLSchema initalValuesSchema = initValuesWorkspace.getGMLSchema();
    final IFeatureType hydIniFT = initalValuesSchema.getFeatureType( new QName( NaModelConstants.NS_INIVALUES, NaModelConstants.INI_HYD_MEMBER_PROP ) );
    final IPropertyType featureIdPT = hydIniFT.getProperty( new QName( NaModelConstants.NS_INIVALUES, NaModelConstants.INI_HYD_FEATUREID_PROP ) );
    final Feature[] hydIniFEs = initValuesWorkspace.getFeatures( hydIniFT );
    for( int i = 0; i < hydIniFEs.length; i++ )
    {
      final Feature hydIniFE = hydIniFEs[i];
      final String value = (String) hydIniFE.getProperty( featureIdPT );
      if( hydroFE.getId().equals( value ) )
      {
        final Feature catchementIniFE = hydIniFE.getParent();
        final Feature newHydIniFE = initValuesWorkspace.createFeature( catchementIniFE, hydIniFT );
        FeatureHelper.copyNoRelationPropterty( hydIniFE, newHydIniFE );
        newHydIniFE.setProperty( new QName( NaModelConstants.NS_INIVALUES, NaModelConstants.INI_HYD_FEATUREID_PROP ), newHydroFE.getId() );
        newHydIniFE.setProperty( new QName( NS.GML3, NaModelConstants.GML_FEATURE_DESCRIPTION_PROP ), "automatically generated inital value for hydrotopID=" + newHydroFE.getId() );
        final IRelationType linkCatchmentHydIni = (IRelationType) catchementIniFE.getFeatureType().getProperty( new QName( NaModelConstants.NS_INIVALUES, NaModelConstants.INI_CATCHMENT_LINK_HYD_PROP ) );
        initValuesWorkspace.addFeatureAsComposition( catchementIniFE, linkCatchmentHydIni, 0, newHydIniFE );
        // there is only one match for hydIni.featureID to hydroFE.getId() -> leave the loop now
        break;
      }

    }
  }

  /**
   * Visits all features in the workspace and sets the <genereateResults/> property.
   * 
   * @param modelWorkspace
   *          the rrm workspace to visit and modify.
   * @param state
   *          true to generate results, false otherwise.
   */

  private void setAllResultFlags( final GMLWorkspace modelWorkspace, final boolean state )
  {
    final Feature rootFeature = modelWorkspace.getRootFeature();
    final IGMLSchema naSchema = modelWorkspace.getGMLSchema();
    final IFeatureType nodeFT = naSchema.getFeatureType( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.NODE_ELEMENT_FT ) );
    final IPropertyType genResultPT = nodeFT.getProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.GENERATE_RESULT_PROP ) );
    final IPropertyType timeSeriesLinkPT = nodeFT.getProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.NODE_RESULT_TIMESERIESLINK_PROP ) );
    final HashMap<IPropertyType, Object> map = new HashMap<IPropertyType, Object>();
    map.put( genResultPT, new Boolean( state ) );
    map.put( timeSeriesLinkPT, null );
    final SetPropertyFeatureVisitor visitor = new SetPropertyFeatureVisitor( map );
    modelWorkspace.accept( visitor, rootFeature, FeatureVisitor.DEPTH_INFINITE );
  }

  // /**
  // * This method generates the net and sets only the lowest node to generate results for.
  // *
  // * @param modelworkspace
  // * the rrm model workspace
  // * @param affectedChannels
  // * the channels to take the downstream node as a possible rootnode of the net
  // */
  //
  // private void setFeaturesWithResults( final GMLWorkspace modelworkspace, final Feature[] affectedChannels ) throws
  // Exception
  // {
  // final NAConfiguration naConfiguration = NAConfiguration.getGml2AsciiConfiguration( modelworkspace.getContext(),
  // null );
  // final NetFileManager netManager = new NetFileManager( naConfiguration );
  // final HashMap<String, NetElement> netElements = netManager.generateNetElements( modelworkspace, null );
  // 
  // for( int i = 0; i < affectedChannels.length; i++ )
  // {
  // final Feature channel = affectedChannels[i];
  // final NetElement rootElement = netElements.get( channel.getId() );
  //
  // final Feature downStreamNode = rootElement.getDownStreamNode();
  // // downStreamNode.setProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.GENERATE_RESULT_PROP ),
  // Boolean.TRUE );
  // getRootNetElements( netElements, downStreamNode );
  // }
  //    
  //    
  // final HashMap<QName, Object> qNames = new HashMap<QName, Object>();
  // qNames.put( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.GENERATE_RESULT_PROP ), new Boolean( true )
  // );
  // final CollectFeaturesWithProperty visitor = new CollectFeaturesWithProperty( qNames, null );
  // modelworkspace.accept( visitor, modelworkspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );
  // final Feature[] resultFE = visitor.getFeatures();
  // for( int i = 0; i < resultFE.length; i++ )
  // {
  // final Feature feature = resultFE[i];
  // Object property = feature.getProperty( new QName( XMLHelper.GMLSCHEMA_NS, NaModelConstants.GML_FEATURE_NAME_PROP )
  // );
  // if( property != null )
  // m_featruesWithResults.add( (String) property );
  // }
  // }
  //
  // private List getRootNetElements( HashMap<String, NetElement> netElements, Feature rootNode ) throws Exception
  // {
  // final Collection<NetElement> netElemtentCollection = netElements.values();
  // // collect netelements that are direct upstream of result nodes
  // final RootNodeCollectorVisitor rootNodeVisitor;
  // if( rootNode == null )
  // rootNodeVisitor = new RootNodeCollectorVisitor();
  // else
  // rootNodeVisitor = new RootNodeCollectorVisitor( rootNode );
  // for( Iterator iter = netElemtentCollection.iterator(); iter.hasNext(); )
  // {
  // NetElement element = (NetElement) iter.next();
  // element.accept( rootNodeVisitor );
  // }
  // return rootNodeVisitor.getRootNodeElements();
  // }

  /**
   * Simple filter class Fthat assures only directories and the optionaly directories with a specific name are valid.
   */

  class FileFilter implements FilenameFilter
  {

    HashSet<String> m_hashtable = new HashSet<String>();

    public boolean accept( File dir, String name )
    {
      File file = new File( dir, name );
      if( file.isDirectory() && (m_hashtable.contains( name ) || m_hashtable.isEmpty()) )
        return true;
      return false;
    }

    public void add( String[] ommit )
    {
      for( int i = 0; i < ommit.length; i++ )
        m_hashtable.add( ommit[i] );
    }

  }

  class ResultFileFilter implements IOFileFilter
  {

    ArrayList<File> m_matchingFiles = new ArrayList<File>();

    HashSet<String> m_hashtable = new HashSet<String>();

    ResultFileFilter( final List<String> toMatch )
    {
      for( Iterator iter = toMatch.iterator(); iter.hasNext(); )
        m_hashtable.add( (String) iter.next() );
    }

    /**
     * @see org.apache.commons.io.filefilter.IOFileFilter#accept(java.io.File)
     */
    public boolean accept( File file )
    {
      final String filename = file.getName();
      if( file.isDirectory() && m_hashtable.contains( filename ) )
      {
        m_matchingFiles.add( file );
        return true;
      }
      return false;
    }

    /**
     * @see org.apache.commons.io.filefilter.IOFileFilter#accept(java.io.File, java.lang.String)
     */
    public boolean accept( File file, String name )
    {
      return false;
    }

    public File[] getMatchingFiles( )
    {
      return m_matchingFiles.toArray( new File[m_matchingFiles.size()] );
    }
  }
}
