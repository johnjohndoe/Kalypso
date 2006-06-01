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
package org.kalypso.ui.model.wspm.core.wspwin;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.commons.metadata.MetadataObject;
import org.kalypso.commons.resources.SetContentHelper;
import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.result.ValueComponent;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.model.wspm.KalypsoUIModelWspmPlugin;
import org.kalypso.ui.model.wspm.abstraction.TuhhCalculation;
import org.kalypso.ui.model.wspm.abstraction.TuhhReach;
import org.kalypso.ui.model.wspm.abstraction.TuhhWspmProject;
import org.kalypso.ui.model.wspm.abstraction.WspmProfileReference;
import org.kalypso.ui.model.wspm.abstraction.WspmProject;
import org.kalypso.ui.model.wspm.abstraction.WspmWaterBody;
import org.kalypso.ui.model.wspm.abstraction.TuhhCalculation.START_KONDITION_KIND;
import org.kalypso.ui.model.wspm.core.wspwin.CalculationContentBean.ART_ANFANGS_WSP;
import org.kalypso.ui.model.wspm.core.wspwin.CalculationContentBean.FLIESSGESETZ;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author thuel2
 */
public class WspWinImporter
{
  public static final String DIR_PROF = "prof";

  public static final String FILE_PROBEZ = "probez.txt";

  private static final DateFormat DATE_FORMATTER = SimpleDateFormat.getDateTimeInstance( SimpleDateFormat.SHORT, SimpleDateFormat.SHORT );

  private WspWinImporter( )
  {
    // will not be instantiated
  }

  /**
   * Imports a wpswin project into a folder or a project.
   * <p>
   * Prerequisite: container holds a valid wspm-tuhh structure
   * </p>
   */
  public static IStatus importProject( final File wspwinDirectory, final IContainer targetContainer, final IProgressMonitor monitor ) throws Exception
  {
    final MultiStatus logStatus = new MultiStatus( PluginUtilities.id( KalypsoUIModelWspmPlugin.getDefault() ), 0, "Import-Log", null );

    monitor.beginTask( "WspWin Projekt importieren", 1000 );

    monitor.subTask( " - Initialisiere KALYPSO..." );

    try
    {
      // load gml workspace
      monitor.subTask( " - Modell wird geladen..." );
      final IFile modelFile = targetContainer.getFile( new Path( "wspmTuhhModel.gml" ) );
      final URL url = ResourceUtilities.createURL( modelFile );
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( url );
      monitor.worked( 200 );

      monitor.subTask( " - WspWin Projekt wird geladen..." );
      // load wspwin data

      monitor.subTask( " - Daten werden konvertiert..." );
      // fill wspwin data into workspace
      final Feature modelRootFeature = workspace.getRootFeature();

      final WspmProject wspmProject = new WspmProject( modelRootFeature );

      // ////////////// //
      // set model name //
      // ////////////// //
      final String oldProjectName = wspmProject.getName();
      final String modelName = oldProjectName + wspwinDirectory.getName();
      wspmProject.setName( modelName );

      // ///////////////////// //
      // set model description //
      // ///////////////////// //
      final String oldProjectDescription = wspmProject.getDescription();
      final StringBuffer modelDescription = new StringBuffer( oldProjectDescription == null ? "" : oldProjectDescription );
      if( modelDescription.length() != 0 )
        modelDescription.append( "\n\n" );

      modelDescription.append( "Projekt wurde aus WspWin Projekt " );
      modelDescription.append( wspwinDirectory.getAbsolutePath() );
      modelDescription.append( " importiert (" );
      modelDescription.append( DATE_FORMATTER.format( new Date( System.currentTimeMillis() ) ) );
      modelDescription.append( ")." );

      final String wspwinModelDescription = readProjectDescription( wspwinDirectory );
      if( wspwinModelDescription != null )
      {
        modelDescription.append( "\nWspWin Projektbeschreibung: " );
        modelDescription.append( wspwinModelDescription );
      }

      wspmProject.setDescription( modelDescription.toString() );

      // /////////////////// //
      // Load WspWin Project //
      // /////////////////// //
      final WspCfgBean wspCfgBean = WspCfgBean.read( wspwinDirectory );
      if( wspCfgBean.getType() != 'b' )
      {
        PluginUtilities.logToPlugin( KalypsoUIModelWspmPlugin.getDefault(), IStatus.WARNING, "Es wird ein WspWin-Knauf Projekt als TUHH-Pasche-Projekt importiert.", null );
        wspCfgBean.setType( 'b' );
      }

      // from now on, we have tuhh projects: if we later support other kinds of projects, tweak here
      final TuhhWspmProject tuhhProject = new TuhhWspmProject( modelRootFeature );

      // TODO: ask direction from user
      final boolean isDirectionUpstreams = true;

      // /////////////// //
      // import profiles //
      // /////////////// //
      final ProfileBean[] commonProfiles = wspCfgBean.readProfproj( wspwinDirectory );
      final Map<String, WspmProfileReference> importedProfiles = new HashMap<String, WspmProfileReference>( commonProfiles.length );
      logStatus.add( importProfiles( getProfDir( wspwinDirectory ), tuhhProject, commonProfiles, importedProfiles, isDirectionUpstreams ) );

      // ////////////// //
      // import reaches //
      // ////////////// //
      final ZustandBean[] zustaende = wspCfgBean.getZustaende();
      for( final ZustandBean zustandBean : zustaende )
      {
        try
        {
          logStatus.add( importTuhhZustand( tuhhProject, wspCfgBean, zustandBean, importedProfiles, isDirectionUpstreams ) );
        }
        catch( final Exception e )
        {
          logStatus.add( StatusUtilities.statusFromThrowable( e, "Fehler beim Import von Zustand: " + zustandBean.getFileName() ) );
        }
      }

      // stop, if we have errors until now
      if( !logStatus.isOK() )
        return logStatus;

      // /////////////// //
      // write workspace //
      // ////////////// //
      monitor.subTask( "Modell wird geschrieben..." );
      final SetContentHelper contentHelper = new SetContentHelper()
      {
        @Override
        protected void write( final OutputStreamWriter writer ) throws Throwable
        {
          GmlSerializer.serializeWorkspace( writer, workspace );
        }
      };

      contentHelper.setFileContents( modelFile, false, true, new SubProgressMonitor( monitor, 200 ) );
    }
    finally
    {
      monitor.done();
    }

    return logStatus;
  }

  /**
   * Adds the profile beans as profiles to the tuhh-project. For each profile bean, a new profile file is generated and
   * the profile is added as reference to it.
   */
  private static IStatus importProfiles( final File profDir, final TuhhWspmProject tuhhProject, final ProfileBean[] commonProfiles, final Map<String, WspmProfileReference> addedProfiles, final boolean isDirectionUpstreams )
  {
    final MultiStatus status = new MultiStatus( PluginUtilities.id( KalypsoUIModelWspmPlugin.getDefault() ), 0, "Import PROFPROJ.TXT", null );

    for( final ProfileBean bean : commonProfiles )
    {
      try
      {
        importProfile( profDir, tuhhProject, addedProfiles, bean, isDirectionUpstreams );
      }
      catch( final MalformedURLException e )
      {
        status.add( StatusUtilities.statusFromThrowable( e ) );
      }
      catch( final IOException e )
      {
        status.add( StatusUtilities.statusFromThrowable( e ) );
      }
    }

    return status;
  }

  /**
   * Imports a single profile according to the given ProfileBean. If the map already contains a profile with the same id
   * (usually the filename), we return this instead.
   */
  private static WspmProfileReference importProfile( final File profDir, final TuhhWspmProject tuhhProject, final Map<String, WspmProfileReference> knownProfiles, final ProfileBean bean, final boolean isDirectionUpstreams ) throws IOException
  {
    final String fileName = bean.getFileName();

    if( knownProfiles.containsKey( fileName ) )
      return knownProfiles.get( fileName );

    final WspmProfileReference prof = tuhhProject.createNewProfile( bean.getWaterName(), isDirectionUpstreams, fileName );
    knownProfiles.put( fileName, prof );

    // TODO: fill data into profile
    bean.getStation();
    bean.getFileName();

    // HACK: we now just copy the files directly instead
    final URL context = tuhhProject.getFeature().getWorkspace().getContext();
    Writer urlWriter = null;
    Reader fileReader = null;
    try
    {
      final URL targetUrl = new URL( context, prof.getHref() );
      final File file = new File( profDir, fileName );

      urlWriter = UrlResolverSingleton.getDefault().createWriter( targetUrl );
      fileReader = new FileReader( file );
      IOUtils.copy( fileReader, urlWriter );

      return prof;
    }
    finally
    {
      IOUtils.closeQuietly( urlWriter );
      IOUtils.closeQuietly( fileReader );
    }
  }

  /**
   * Adds the zustand-bean to the tuhh-project. Already imported profiles are not imported a second time.
   * <p>
   * If the zustand contains unknown profiles (e.g. due to a wspwin bug), they will be also imported and added to the
   * importedPRofilesMap.
   * </p>
   */
  private static IStatus importTuhhZustand( final TuhhWspmProject tuhhProject, final WspCfgBean wspCfg, final ZustandBean zustandBean, final Map<String, WspmProfileReference> importedProfiles, final boolean isDirectionUpstreams ) throws IOException, ParseException
  {
    final MultiStatus status = new MultiStatus( PluginUtilities.id( KalypsoUIModelWspmPlugin.getDefault() ), 0, "Import " + zustandBean.getFileName(), null );

    final String name = zustandBean.getName();
    final String waterName = zustandBean.getWaterName();

    final TuhhReach reach = tuhhProject.createNewReach( waterName, isDirectionUpstreams );
    reach.setName( name );

    final StringBuffer descBuffer = new StringBuffer();
    descBuffer.append( "Imported from WspWin\n" );
    descBuffer.append( "Originally created: " + DATE_FORMATTER.format( zustandBean.getDate() ) );

    reach.setDescription( descBuffer.toString() );

    // add reachSegments + profiles (.str)
    final ZustandContentBean zustandContent = wspCfg.readZustand( zustandBean );
    // we ignore the profileBeans and just add the segments, so we can even import projects
    // which are slightly inconsistent, maybe add warning messages later
    final ZustandSegmentBean[] segmentBeans = zustandContent.getSegmentBeans();
    final File profDir = getProfDir( wspCfg.getProjectDir() );
    // create reachSegments and associated profiles
    for( final ZustandSegmentBean bean : segmentBeans )
    {
      try
      {
        final ProfileBean fromBean = new ProfileBean( waterName, bean.getStationFrom(), bean.getFileNameFrom(), new HashMap<String, String>() );
        final WspmProfileReference fromProf = importProfile( profDir, tuhhProject, importedProfiles, fromBean, isDirectionUpstreams );

        reach.createProfileSegment( fromProf, bean.getStationFrom(), bean.getDistanceVL(), bean.getDistanceHF(), bean.getDistanceVR() );
      }
      catch( final IOException e )
      {
        status.add( StatusUtilities.statusFromThrowable( e ) );
      }

      if( bean == segmentBeans[segmentBeans.length - 1] )
      {
        try
        {
          // also add last profile
          final ProfileBean toBean = new ProfileBean( waterName, bean.getStationTo(), bean.getFileNameTo(), new HashMap<String, String>() );
          final WspmProfileReference toProf = importProfile( profDir, tuhhProject, importedProfiles, toBean, isDirectionUpstreams );

          reach.createProfileSegment( toProf, bean.getStationTo(), 0.0, 0.0, 0.0 );
        }
        catch( final IOException e )
        {
          status.add( StatusUtilities.statusFromThrowable( e ) );
        }
      }

    }

    final WspmWaterBody waterBody = reach.getWaterBody();

    // base name for runOffEvents and calculations. Needed, because in WspWin the runOffs and calculations belonged
    // always to a zustand.
    final String baseName = waterBody.getName() + "-" + zustandBean.getName() + " - ";

    // ////////////////////////////////////////////////////// //
    // add runoff events and waterlevel fixations(.wsf, .qwt) //
    // ////////////////////////////////////////////////////// //

    // map is used to remember position of runoff: used later by calculation to find reference
    final Map<Integer, String> readRunOffEvents = new HashMap<Integer, String>();
    try
    {
      final RunOffEventBean[] runOffEventBeans = zustandBean.readRunOffs( profDir );
      int countRO = 0;
      for( final RunOffEventBean bean : runOffEventBeans )
      {
        final Feature runOffFeature = waterBody.createRunOffEvent();
        writeRunOffBeanIntoFeature( bean, baseName + bean.getName(), runOffFeature );

        // remember for reference from calculation
        readRunOffEvents.put( countRO++, runOffFeature.getId() );
      }
    }
    catch( final Exception e )
    {
      status.add( StatusUtilities.statusFromThrowable( e ) );
    }
    
    try
    {
      final RunOffEventBean[] wspFixesBeans = zustandBean.readWspFixes( profDir );
      for( final RunOffEventBean bean : wspFixesBeans )
      {
        final Feature wspFixFeature = waterBody.createWspFix();
        writeWspFixBeanIntoFeature( bean, baseName + bean.getName(), wspFixFeature );
      }
    }
    catch( final Exception e )
    {
      status.add( StatusUtilities.statusFromThrowable( e ) );
    }

    // ////////////////////////// //
    // add einzelverluste (.psi) //
    // ////////////////////////// //

    // ///////////////////////////// //
    // add calculations (.ber, .001) //
    // ///////////////////////////// //
    try
    {
      final CalculationBean[] calcBeans = zustandBean.readCalculations( profDir );
      for( final CalculationBean bean : calcBeans )
      {
        try
        {
          final CalculationContentBean contentBean = bean.readCalculationContent( profDir );

          // create calculation
          final TuhhCalculation calc = tuhhProject.createCalculation();

          calc.setName( baseName + bean.getName() );
          calc.setDescription( "Imported from WspWin" );
          calc.setCalcCreation( "WspWin Import", new Date() );
          calc.setReachRef( reach );

          final FLIESSGESETZ fliessgesetz = contentBean.getFliessgesetz();
          switch( fliessgesetz )
          {
            case MANNING_STRICKLER:
              calc.setFliessgesetz( TuhhCalculation.FLIESSGESETZ.MANNING_STRICKLER );
              break;
            case DARCY_WEISBACH_OHNE_FORMEINFLUSS:
              calc.setFliessgesetz( TuhhCalculation.FLIESSGESETZ.DARCY_WEISBACH_OHNE_FORMEINFLUSS );
              break;
            case DARCY_WEISBACH_MIT_FORMEINFLUSS:
              calc.setFliessgesetz( TuhhCalculation.FLIESSGESETZ.DARCY_WEISBACH_MIT_FORMEINFLUSS );
              break;
          }

          calc.setSubReachDef( contentBean.getAnfang(), contentBean.getEnde() );

          final ART_ANFANGS_WSP artAnfangswasserspiegel = contentBean.getArtAnfangswasserspiegel();
          final START_KONDITION_KIND type;
          switch( artAnfangswasserspiegel )
          {
            case DIREKTEINGABE:
              type = TuhhCalculation.START_KONDITION_KIND.WATERLEVEL;
              break;

            default:
            case GRENZTIEFE:
              type = TuhhCalculation.START_KONDITION_KIND.CRITICAL_WATER_DEPTH;
              break;

            case STATIONAER_GLEICHFOERMIGES_GEFAELLE:
              type = TuhhCalculation.START_KONDITION_KIND.UNIFORM_BOTTOM_SLOPE;
              break;
          }
          final double startSlope = contentBean.getGefaelle();
          final double startWsp = contentBean.getHoehe();
          calc.setStartCondition( type, startWsp, startSlope );

          final TuhhCalculation.WSP_ITERATION_TYPE iterationType;
          if( contentBean.isSimpleBerechnungWSPInt() )
            iterationType = TuhhCalculation.WSP_ITERATION_TYPE.SIMPLE;
          else
            iterationType = TuhhCalculation.WSP_ITERATION_TYPE.EXACT;

          final TuhhCalculation.VERZOEGERUNSVERLUST_TYPE verzType;
          switch( contentBean.getVerzoegerungsVerlust() )
          {
            default:
            case BJOERNSEN:
              verzType = TuhhCalculation.VERZOEGERUNSVERLUST_TYPE.BJOERNSEN;
              break;
            case DFG:
              verzType = TuhhCalculation.VERZOEGERUNSVERLUST_TYPE.DFG;
              break;
            case DVWK:
              verzType = TuhhCalculation.VERZOEGERUNSVERLUST_TYPE.DVWK;
              break;
          }

          final TuhhCalculation.REIBUNGSVERLUST_TYPE reibType;
          if( contentBean.isReibungsverlustNachTrapezformel() )
            reibType = TuhhCalculation.REIBUNGSVERLUST_TYPE.TRAPEZ_FORMULA;
          else
            reibType = TuhhCalculation.REIBUNGSVERLUST_TYPE.GEOMETRIC_FORMULA;

          calc.setWaterlevelParameters( iterationType, verzType, reibType, contentBean.isBerechneBruecken(), contentBean.isBerechneWehre() );

          switch( contentBean.getCalcKind() )
          {
            case WATERLEVEL:
              calc.setCalcMode( TuhhCalculation.MODE.WATERLEVEL );
              break;

            case BF_UNIFORM:
              calc.setCalcMode( TuhhCalculation.MODE.BF_UNIFORM );
              break;

            case BF_NON_UNIFORM:
              calc.setCalcMode( TuhhCalculation.MODE.BF_NON_UNIFORM );
              break;
          }

          final String runOffRef = readRunOffEvents.get( contentBean.getAbfluss() );
          calc.setRunOffRef( runOffRef );

          // set q-Range. Remember: Q-Range in CalculationcontentBean is in dl/s
          calc.setQRange( contentBean.getMin() / 100.0, contentBean.getMax() / 100.0, contentBean.getStep() / 100.0 );
        }
        catch( final Exception e )
        {
          status.add( StatusUtilities.statusFromThrowable( e ) );
        }
      }
    }
    catch( final Exception e )
    {
      status.add( StatusUtilities.statusFromThrowable( e ) );
    }

    return status;
  }

  private static void writeRunOffBeanIntoFeature( final RunOffEventBean bean, final String name, final Feature runOffFeature, final IComponent valueComp )
  {

    final IComponent stationComp = new ValueComponent( "Station", "Station", XmlTypes.XS_DOUBLE, "km" );
    final TupleResult result = new TupleResult( new IComponent[] { stationComp, valueComp } );

    final Map<Double, Double> values = bean.getEntries();
    for( final Map.Entry<Double, Double> entry : values.entrySet() )
    {
      final IRecord record = result.createRecord();
      result.add( record );
      record.setValue( stationComp, entry.getKey() );
      record.setValue( valueComp, entry.getValue() );
    }
// TODO: WSP Fixierung nur schreiben, wenn Anzahl größer 0
    final IObservation<TupleResult> obs = new Observation<TupleResult>( name, "Importiert aus WspWin", result, new ArrayList<MetadataObject>() );
    ObservationFeatureFactory.toFeature( obs, runOffFeature );

  }

  private static void writeRunOffBeanIntoFeature( final RunOffEventBean bean, final String name, final Feature runOffFeature )
  {
    final IComponent abflussComp = new ValueComponent( "Abfluss", "Abfluss", XmlTypes.XS_DOUBLE, "m³/s" );
    writeRunOffBeanIntoFeature( bean, name, runOffFeature, abflussComp );
  }

  private static void writeWspFixBeanIntoFeature( final RunOffEventBean bean, final String name, final Feature runOffFeature )
  {
    final IComponent wspComp = new ValueComponent( "Wasserstand", "Wasserstand", XmlTypes.XS_DOUBLE, "mNN" );
    writeRunOffBeanIntoFeature( bean, name, runOffFeature, wspComp );

  }

  /** Returns the content of the prof/probez.txt file */
  public static String readProjectDescription( final File wspwinDirectory ) throws IOException
  {
    final File probezFile = new File( getProfDir( wspwinDirectory ), FILE_PROBEZ );

    return FileUtils.readFileToString( probezFile, null );
  }

  public static File getProfDir( final File wspwinDirectory )
  {
    return new File( wspwinDirectory, DIR_PROF );
  }

}
