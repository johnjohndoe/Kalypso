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
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.visitors.SetPropertyFeatureVisitor;

/**
 * @author kuepfer
 */
public class KalypsoDssCalcJob implements ISimulation
{

  private HashSet<NaModelCalcJob> m_naCalcJobs = new HashSet<NaModelCalcJob>();

  private File m_resultDirDSS = null;

  List<String> m_featruesWithResults = new ArrayList<String>();

  private String m_initValueFilePrefix = "lzsim";

  private String m_initValueFileSuffix = ".gml";

  /** assures to return only directories for a file.list() call */
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
      final GMLWorkspace control = GmlSerializer.createGMLWorkspace( controlURL, null );
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

    final URL calcCases = KalypsoDssCalcJob.class.getResource( "resources/hq1_bis_hq100.zip" );

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
      // if the calc service has a getCapabilites() method this will be set dynamically, for know it is hard coded
      final String[] hqIdentifierToCalculate;
      if( choiseCalcCase.equals( MeasuresConstants.METADATA_CALCCASE_ENUM_ALL ) )
        hqIdentifierToCalculate = new String[] { "HQ1", "HQ5", "HQ10", "HQ20", "HQ50", "HQ100" };
      else
        hqIdentifierToCalculate = new String[] { choiseCalcCase };

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
        final NaSimulationDataProvider naSimulationDataProvider = new NaSimulationDataProvider( baseDir );
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
            }
          }

        };

        monitor.setMessage( "Maßnahmen erfogreich in das Model eingefügt..." );
        final NaModelCalcJob calcJob = new NaModelCalcJob();
        m_naCalcJobs.add( calcJob );
        final File calcDirUrl = (File) rrmInputProvider.getInputForID( NaSimulationDataProvider.CALC_DIR );
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
    final URL initValueURLDir = (URL) rrmInputProvider.getInputForID( NaModelConstants.LZSIM_IN_ID );
    final File calcDir = (File) rrmInputProvider.getInputForID( NaSimulationDataProvider.CALC_DIR );

    GMLWorkspace modelWorkspace = null;
    GMLWorkspace hydrotopWorkspace = null;
    GMLWorkspace initValuesWorkspace = null;
    try
    {
      modelWorkspace = GmlSerializer.createGMLWorkspace( modelURL, null );
      if( planningMeasureURL != null )
      {

        hydrotopWorkspace = GmlSerializer.createGMLWorkspace( hydrotopURL, null );
        // TODO: fix depenency of initial values file name to NaModelInnerCalcJob
        initValuesWorkspace = GmlSerializer.createGMLWorkspace( new URL( initValueURLDir, m_initValueFilePrefix.concat( m_initValueFileSuffix ) ), null );
        // insert measure
        MeasuresHelper.insertPlanningMeasure( planningMeasureURL, hydrotopWorkspace, initValuesWorkspace, rrmInputProvider, logger );
        writeNewHydrotopFile = true;
        writeNewInitalValueFile = true;
      }
      if( measuresRhbURL != null && doMeasures )
      {
        MeasuresHelper.insertStorageChannelMeasure( measuresRhbURL, modelWorkspace, logger );
      }
      if( measuresSealingURL != null && doMeasures )
      {
        if( hydrotopWorkspace == null )
          hydrotopWorkspace = GmlSerializer.createGMLWorkspace( hydrotopURL, null );
        MeasuresHelper.insertSealingChangeMeasure( measuresSealingURL, hydrotopWorkspace, rrmInputProvider, logger );
        writeNewHydrotopFile = true;
      }
      if( measuresMrsURL != null && doMeasures )
      {
        MeasuresHelper.insertSwaleTrenchMeasure( measuresMrsURL, modelWorkspace, hydrotopWorkspace, designAreaURL, logger );
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
          final File initValueFile = new File( calcDir, m_initValueFilePrefix.concat( m_initValueFileSuffix ) );
          initValueFile.deleteOnExit();
          writerInitValue = new BufferedWriter( new OutputStreamWriter( new FileOutputStream( initValueFile ), MeasuresConstants.DEFAULT_ENCONDING ) );
          GmlSerializer.serializeWorkspace( writerInitValue, initValuesWorkspace, MeasuresConstants.DEFAULT_ENCONDING );
          rrmInputProvider.addURL( NaModelConstants.LZSIM_IN_ID, new File( initValueFile.getParent() ).toURL() );
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
    final GMLWorkspace designArea = GmlSerializer.createGMLWorkspace( designAreaURL, null );
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
    return channelCollector.toArray( new Feature[channelCollector.size()] );
  }

  /**
   * @see org.kalypso.simulation.core.ISimulation#getSpezifikation()
   */
  public URL getSpezifikation( )
  {
    return KalypsoDssCalcJob.class.getResource( "resources/dsscalcjob_spec.xml" );
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
