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
package org.kalypso.model.wspm.tuhh.core.wspwin;

import java.io.File;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.math.BigDecimal;
import java.net.URL;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.commons.resources.SetContentHelper;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.gml.WspmProject;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSource;
import org.kalypso.model.wspm.core.profil.serializer.ProfilSerializerUtilitites;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.KalypsoModelWspmTuhhCorePlugin;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation.START_KONDITION_KIND;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.wspwin.core.CalculationBean;
import org.kalypso.wspwin.core.CalculationContentBean;
import org.kalypso.wspwin.core.CalculationContentBean.ART_ANFANGS_WSP;
import org.kalypso.wspwin.core.CalculationContentBean.FLIESSGESETZ;
import org.kalypso.wspwin.core.LocalEnergyLossBean;
import org.kalypso.wspwin.core.ProfileBean;
import org.kalypso.wspwin.core.RunOffEventBean;
import org.kalypso.wspwin.core.WspCfgBean;
import org.kalypso.wspwin.core.WspWinHelper;
import org.kalypso.wspwin.core.ZustandBean;
import org.kalypso.wspwin.core.ZustandContentBean;
import org.kalypso.wspwin.core.ZustandSegmentBean;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author thuel2
 */
public class WspWinImporter
{
  public static final String FILE_PROBEZ = "probez.txt"; //$NON-NLS-1$

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
    final MultiStatus logStatus = new MultiStatus( PluginUtilities.id( KalypsoModelWspmTuhhCorePlugin.getDefault() ), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.1" ), null ); //$NON-NLS-1$

    monitor.beginTask( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.2" ), 1000 ); //$NON-NLS-1$

    monitor.subTask( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.3" ) ); //$NON-NLS-1$

    try
    {
      // load gml workspace
      monitor.subTask( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.4" ) ); //$NON-NLS-1$
      final IFile modelFile = targetContainer.getFile( new Path( "modell.gml" ) ); //$NON-NLS-1$
      final URL url = ResourceUtilities.createURL( modelFile );
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( url, null );
      monitor.worked( 200 );

      monitor.subTask( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.6" ) ); //$NON-NLS-1$
      // load wspwin data

      monitor.subTask( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.7" ) ); //$NON-NLS-1$
      // fill wspwin data into workspace
      final Feature modelRootFeature = workspace.getRootFeature();

      final WspmProject wspmProject = (WspmProject) modelRootFeature;

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
      final StringBuffer modelDescription = new StringBuffer( oldProjectDescription == null ? "" : oldProjectDescription ); //$NON-NLS-1$
      if( modelDescription.length() != 0 )
        modelDescription.append( "\n\n" ); //$NON-NLS-1$

      modelDescription.append( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.10", wspwinDirectory.getAbsolutePath(), DATE_FORMATTER.format( new Date( System.currentTimeMillis() ) ) ) ); //$NON-NLS-1$

      final String wspwinModelDescription = readProjectDescription( wspwinDirectory );
      if( wspwinModelDescription != null )
      {
        modelDescription.append( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.13" ) ); //$NON-NLS-1$
        modelDescription.append( wspwinModelDescription );
      }

      wspmProject.setDescription( modelDescription.toString() );

      // /////////////////// //
      // Load WspWin Project //
      // /////////////////// //
      final WspCfgBean wspCfgBean = WspCfgBean.read( wspwinDirectory );

      final boolean isNotTuhhProject = wspCfgBean.getType() != 'b';
      if( isNotTuhhProject )
      {
        PluginUtilities.logToPlugin( KalypsoModelWspmTuhhCorePlugin.getDefault(), IStatus.WARNING, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.14" ), null ); //$NON-NLS-1$
        wspCfgBean.setType( 'b' );
      }

      // from now on, we have tuhh projects: if we later support other kinds of projects, tweak here
      final TuhhWspmProject tuhhProject = (TuhhWspmProject) modelRootFeature;

      // TODO: ask direction from user
      final boolean isDirectionUpstreams = true;

      // /////////////// //
      // import profiles //
      // /////////////// //
      final Map<String, IProfileFeature> importedProfiles = new HashMap<String, IProfileFeature>();
      try
      {
        final ProfileBean[] commonProfiles = wspCfgBean.readProfproj( wspwinDirectory );
        logStatus.add( importProfiles( WspWinHelper.getProfDir( wspwinDirectory ), tuhhProject, commonProfiles, importedProfiles, isDirectionUpstreams, isNotTuhhProject ) );
      }
      catch( final ParseException pe )
      {
        /*
         * If an error in the profproj parsing happens, just give a warning to the user. What we really need are the
         * profile within the existing strand-elements.
         */
        logStatus.add( StatusUtilities.createStatus( IStatus.WARNING, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.15" ), pe ) ); //$NON-NLS-1$
      }

      // ////////////// //
      // import reaches //
      // ////////////// //
      final ZustandBean[] zustaende = wspCfgBean.getZustaende();
      for( final ZustandBean zustandBean : zustaende )
      {
        try
        {
          logStatus.add( importTuhhZustand( tuhhProject, wspCfgBean, zustandBean, importedProfiles, isDirectionUpstreams, isNotTuhhProject ) );
        }
        catch( final Exception e )
        {
          logStatus.add( StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.16" ) + zustandBean.getFileName() ) ); //$NON-NLS-1$
        }
      }

      // stop, if we have errors until now
      if( logStatus.matches( IStatus.ERROR | IStatus.CANCEL ) )
        return logStatus;

      // /////////////// //
      // write workspace //
      // /////////////// //
      monitor.subTask( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.17" ) ); //$NON-NLS-1$
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
  private static IStatus importProfiles( final File profDir, final TuhhWspmProject tuhhProject, final ProfileBean[] commonProfiles, final Map<String, IProfileFeature> addedProfiles, final boolean isDirectionUpstreams, final boolean isNotTuhhProject )
  {
    final MultiStatus status = new MultiStatus( PluginUtilities.id( KalypsoModelWspmTuhhCorePlugin.getDefault() ), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.18" ), null ); //$NON-NLS-1$

    for( final ProfileBean bean : commonProfiles )
    {
      try
      {
        final IProfileFeature profile = importProfile( profDir, tuhhProject, addedProfiles, bean, isDirectionUpstreams, isNotTuhhProject );

        final BigDecimal profStation = profile.getBigStation();
        final BigDecimal beanStation = new BigDecimal( bean.getStation() );

        if( Math.abs( profStation.doubleValue() - beanStation.doubleValue() ) > 0.001 )
        {
          final String msg = Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.19", bean.getFileName(), profStation, bean.getWaterName(), bean.getStateName(), beanStation ); //$NON-NLS-1$
          status.add( StatusUtilities.createWarningStatus( msg ) );
          profile.setBigStation( new BigDecimal( bean.getStation() ) );
        }
      }
      catch( final IOException e )
      {
        final String msg = Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.119", bean.getFileName() );//$NON-NLS-1$
        status.add( StatusUtilities.createStatus( IStatus.WARNING, msg, e ) );
      }
      catch( final GMLSchemaException e )
      {
        status.add( StatusUtilities.statusFromThrowable( e ) );
      }
      catch( final CoreException e )
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
  private static IProfileFeature importProfile( final File profDir, final TuhhWspmProject tuhhProject, final Map<String, IProfileFeature> knownProfiles, final ProfileBean bean, final boolean isDirectionUpstreams, final boolean isNotTuhhProject ) throws GMLSchemaException, IOException, CoreException
  {
    final String fileName = bean.getFileName();

    if( knownProfiles.containsKey( fileName ) )
      return knownProfiles.get( fileName );

    final IProfileFeature prof = tuhhProject.createNewProfile( bean.getWaterName(), isDirectionUpstreams );

    final File prfFile = new File( profDir, fileName );

    final String profiletype = IWspmTuhhConstants.PROFIL_TYPE_PASCHE;

    final IProfilSource prfSource = KalypsoModelWspmCoreExtensions.createProfilSource( "prf" ); //$NON-NLS-1$
    final IProfil[] profile = ProfilSerializerUtilitites.readProfile( prfSource, prfFile, profiletype );

    ProfileFeatureFactory.toFeature( profile[0], prof );
    /* Set state as default name for profile. */
    prof.setName( bean.getStateName() );

    /* Only add profile if no error occurs */
    knownProfiles.put( fileName, prof );

    return prof;
  }

  /**
   * Adds the zustand-bean to the tuhh-project. Already imported profiles are not imported a second time.
   * <p>
   * If the zustand contains unknown profiles (e.g. due to a wspwin bug), they will be also imported and added to the
   * importedPRofilesMap.
   * </p>
   */
  private static IStatus importTuhhZustand( final TuhhWspmProject tuhhProject, final WspCfgBean wspCfg, final ZustandBean zustandBean, final Map<String, IProfileFeature> importedProfiles, final boolean isDirectionUpstreams, final boolean isNotTuhhProject ) throws IOException, ParseException
  {
    final MultiStatus status = new MultiStatus( PluginUtilities.id( KalypsoModelWspmTuhhCorePlugin.getDefault() ), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.21" ) + zustandBean.getFileName(), null ); //$NON-NLS-1$

    final String name = zustandBean.getName();
    final String waterName = zustandBean.getWaterName();

    final TuhhReach reach;
    try
    {
      reach = tuhhProject.createNewReach( waterName, isDirectionUpstreams );
    }
    catch( final GMLSchemaException e )
    {
      return StatusUtilities.statusFromThrowable( e );
    }

    reach.setName( name );

    final StringBuffer descBuffer = new StringBuffer();
    descBuffer.append( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.22" ) ); //$NON-NLS-1$
    descBuffer.append( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.23" ) + DATE_FORMATTER.format( zustandBean.getDate() ) ); //$NON-NLS-1$

    reach.setDescription( descBuffer.toString() );

    // add reachSegments + profiles (.str)
    final ZustandContentBean zustandContent = wspCfg.readZustand( zustandBean );
    // we ignore the profileBeans and just add the segments, so we can even import projects
    // which are slightly inconsistent, maybe add warning messages later
    final ZustandSegmentBean[] segmentBeans = zustandContent.getSegmentBeans();
    final File profDir = WspWinHelper.getProfDir( wspCfg.getProjectDir() );
    // create reachSegments and associated profiles
    for( final ZustandSegmentBean bean : segmentBeans )
    {
      try
      {
        final ProfileBean fromBean = new ProfileBean( waterName, name, bean.getStationFrom(), bean.getFileNameFrom(), new HashMap<String, String>() );
        final IProfileFeature fromProf = importProfile( profDir, tuhhProject, importedProfiles, fromBean, isDirectionUpstreams, isNotTuhhProject );

        reach.createProfileSegment( fromProf, bean.getStationFrom() );

        if( bean == segmentBeans[segmentBeans.length - 1] )
        {
          // also add last profile
          final ProfileBean toBean = new ProfileBean( waterName, name, bean.getStationTo(), bean.getFileNameTo(), new HashMap<String, String>() );
          final IProfileFeature toProf = importProfile( profDir, tuhhProject, importedProfiles, toBean, isDirectionUpstreams, isNotTuhhProject );

          reach.createProfileSegment( toProf, bean.getStationTo() );
        }
      }
      catch( final Exception e )
      {
        status.add( StatusUtilities.statusFromThrowable( e ) );
      }
    }

    final WspmWaterBody waterBody = reach.getWaterBody();

    // base name for runOffEvents and calculations. Needed, because in WspWin the runOffs and calculations belonged
    // always to a zustand.
    final String baseName = waterBody.getName() + "-" + zustandBean.getName() + " - "; //$NON-NLS-1$ //$NON-NLS-2$

    // ////////////////////////////////////////////////////// //
    // add runoff events and waterlevel fixations(.wsf, .qwt) //
    // ////////////////////////////////////////////////////// //

    // map is used to remember position of runoff: used later by calculation to find reference
    final Map<Integer, String> readRunOffEvents = new HashMap<Integer, String>();
    try
    {
      final RunOffEventBean[] runOffEventBeans = zustandBean.readRunOffs( profDir );
      int count = 0;
      for( final RunOffEventBean bean : runOffEventBeans )
      {
        final Feature runOffFeature = waterBody.createRunOffEvent();
        writeRunOffBeanIntoFeature( bean, baseName + bean.getName(), runOffFeature );

        // remember for reference from calculation
        readRunOffEvents.put( count++, runOffFeature.getId() );
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
        if( !bean.getEntries().isEmpty() )
        {
          final Feature wspFixFeature = waterBody.createWspFix();
          writeWspFixBeanIntoFeature( bean, baseName + bean.getName(), wspFixFeature );
        }
      }
    }
    catch( final Exception e )
    {
      status.add( StatusUtilities.statusFromThrowable( e ) );
    }

    // /////////////////////////////////////////// //
    // add einzelverluste / Verlustbeiwerte (.psi) //
    // /////////////////////////////////////////// //

    // map is used to remember station of local energy loss (Einzelverlust/Verlustbeiwert)
    final Map<Double, Integer> readLocEnergyLosses = new HashMap<Double, Integer>();
    LocalEnergyLossBean[] locEnergyLossBeans = null;
    try
    {
      locEnergyLossBeans = zustandBean.readLocalEnergyLosses( profDir );

      int count = 0;
      for( final LocalEnergyLossBean bean : locEnergyLossBeans )
      {
        // remember station for reference
        readLocEnergyLosses.put( bean.getStation(), count++ );
      }
    }
    catch( final Exception e )
    {
      status.add( StatusUtilities.statusFromThrowable( e ) );
    }

    // TODO: don't forget to add the local energy losses to profiles: handle multiple losses at one profile - mean
    // value?
    // each zustand can have losses defined for a station / profile. They are preserved in locEnergyLossBeans /
    // readLocEnergyLosses

    // ///////////////////////////// //
    // add calculations (.ber, .001) //
    // ///////////////////////////// //
    try
    {
      if( !isNotTuhhProject )
        importCalculations( tuhhProject, zustandBean, status, reach, profDir, baseName, readRunOffEvents );
    }
    catch( final Exception e )
    {
      status.add( StatusUtilities.statusFromThrowable( e ) );
    }

    return status;
  }

  private static void importCalculations( final TuhhWspmProject tuhhProject, final ZustandBean zustandBean, final MultiStatus status, final TuhhReach reach, final File profDir, final String baseName, final Map<Integer, String> readRunOffEvents ) throws ParseException, IOException
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
        calc.setDescription( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.26" ) ); //$NON-NLS-1$
        calc.setCalcCreation( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.27" ), new Date() ); //$NON-NLS-1$
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

        final boolean useExtremeRougness = false; // as WSPWIN did not know this option, set to false

        calc.setWaterlevelParameters( iterationType, verzType, reibType, contentBean.isBerechneBruecken(), contentBean.isBerechneWehre(), useExtremeRougness );

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

  private static void writeRunOffBeanIntoFeature( final RunOffEventBean bean, final String name, final Feature runOffFeature )
  {
    final IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation( runOffFeature );
    obs.setName( name );
    obs.setDescription( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.28" ) ); //$NON-NLS-1$

    final TupleResult result = obs.getResult();
    final IComponent[] components = result.getComponents();

    final IComponent stationComp;
    final IComponent valueComp;
// if( components.length < 1 )
// return;
    if( components[0].getName().startsWith( "Station" ) ) //$NON-NLS-1$
    {
      stationComp = components[0];
      valueComp = components[1];
    }
    else
    {
      valueComp = components[0];
      stationComp = components[1];
    }

    final Map<BigDecimal, BigDecimal> values = bean.getEntries();
    for( final Map.Entry<BigDecimal, BigDecimal> entry : values.entrySet() )
    {
      final IRecord record = result.createRecord();
      result.add( record );
      record.setValue( result.indexOfComponent( stationComp ), entry.getKey() );
      record.setValue( result.indexOfComponent( valueComp ), entry.getValue() );
    }

    // TODO: WSP Fixierung nur schreiben, wenn Anzahl größer 0
    ObservationFeatureFactory.toFeature( obs, runOffFeature );
  }

  private static void writeWspFixBeanIntoFeature( final RunOffEventBean bean, final String name, final Feature runOffFeature )
  {
    writeRunOffBeanIntoFeature( bean, name, runOffFeature );
  }

  /** Returns the content of the prof/probez.txt file */
  public static String readProjectDescription( final File wspwinDirectory ) throws IOException
  {
    final File probezFile = new File( WspWinHelper.getProfDir( wspwinDirectory ), FILE_PROBEZ );

    return FileUtils.readFileToString( probezFile, null );
  }
}
