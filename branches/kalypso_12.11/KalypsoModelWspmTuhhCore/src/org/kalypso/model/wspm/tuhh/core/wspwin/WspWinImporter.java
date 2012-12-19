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
import java.nio.charset.Charset;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.commons.resources.SetContentHelper;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.IRunOffEvent;
import org.kalypso.model.wspm.core.gml.ProfileFeatureBinding;
import org.kalypso.model.wspm.core.gml.WspmFixation;
import org.kalypso.model.wspm.core.gml.WspmProject;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.serializer.IProfileSource;
import org.kalypso.model.wspm.core.profil.serializer.ProfileSerializerUtilitites;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.KalypsoModelWspmTuhhCorePlugin;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.profile.energyloss.Energyloss;
import org.kalypso.model.wspm.tuhh.core.profile.energyloss.EnergylossProfileObject;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.IObservationFeature;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.wspwin.core.CalculationBean;
import org.kalypso.wspwin.core.ICalculationContentBean;
import org.kalypso.wspwin.core.LocalEnergyLossBean;
import org.kalypso.wspwin.core.ProfileBean;
import org.kalypso.wspwin.core.RunOffEventBean;
import org.kalypso.wspwin.core.WspCfg;
import org.kalypso.wspwin.core.WspCfg.TYPE;
import org.kalypso.wspwin.core.WspWinProject;
import org.kalypso.wspwin.core.WspWinZustand;
import org.kalypso.wspwin.core.ZustandBean;
import org.kalypso.wspwin.core.ZustandSegmentBean;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author thuel2
 */
public final class WspWinImporter
{
  private static final DateFormat DATE_FORMATTER = SimpleDateFormat.getDateTimeInstance( SimpleDateFormat.SHORT, SimpleDateFormat.SHORT );

  private WspWinImporter( )
  {
  }

  /**
   * Imports a wpswin project into a folder or a project.
   * <p>
   * Prerequisite: container holds a valid wspm-tuhh structure
   * </p>
   */
  public static IStatus importProject( final File wspwinDirectory, final IContainer targetContainer, final IProgressMonitor monitor ) throws Exception
  {
    final IStatusCollector logStatus = new StatusCollector( KalypsoModelWspmTuhhCorePlugin.PLUGIN_ID );
    final String problemMessage = Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.1" ); //$NON-NLS-1$

    monitor.beginTask( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.2" ), 1000 ); //$NON-NLS-1$

    monitor.subTask( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.3" ) ); //$NON-NLS-1$

    try
    {
      final WspWinProject wspWinProject = new WspWinProject( wspwinDirectory );

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

      final WspmProject wspmProject = (WspmProject)modelRootFeature;

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
      {
        modelDescription.append( "\n\n" ); //$NON-NLS-1$
      }

      final String description = Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.10", wspwinDirectory.getAbsolutePath(), DATE_FORMATTER.format( new Date( System.currentTimeMillis() ) ) ); //$NON-NLS-1$
      modelDescription.append( description );

      final String wspwinModelDescription = readProjectDescription( wspWinProject );
      if( wspwinModelDescription != null )
      {
        modelDescription.append( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.13" ) ); //$NON-NLS-1$
        modelDescription.append( wspwinModelDescription );
      }

      wspmProject.setDescription( modelDescription.toString() );

      // /////////////////// //
      // Load WspWin Project //
      // /////////////////// //
      final WspCfg wspCfgBean = new WspCfg( wspWinProject, StringUtils.EMPTY );
      final IStatus wspCfgStatus = wspCfgBean.read();
      logStatus.add( wspCfgStatus );

      // from now on, we have tuhh projects: if we later support other kinds of projects, tweak here
      final TuhhWspmProject tuhhProject = (TuhhWspmProject)modelRootFeature;

      // TODO: ask direction from user
      final boolean isDirectionUpstreams = true;

      // /////////////// //
      // import profiles //
      // /////////////// //
      final Map<String, IProfileFeature> importedProfiles = new HashMap<>();
      final ProfileBean[] commonProfiles = wspCfgBean.getProfiles();

      final File profDir = wspWinProject.getProfDir();
      logStatus.add( importProfiles( profDir, tuhhProject, commonProfiles, importedProfiles, isDirectionUpstreams ) );

      // ////////////// //
      // import reaches //
      // ////////////// //
      final WspWinZustand[] zustaende = wspCfgBean.getZustaende();
      for( final WspWinZustand zustand : zustaende )
      {
        try
        {
          logStatus.add( importTuhhZustand( tuhhProject, wspCfgBean, zustand, importedProfiles, isDirectionUpstreams, targetContainer ) );
        }
        catch( final Exception e )
        {
          final String fileName = zustand.getBean().getFileName();
          logStatus.add( IStatus.ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.16", fileName ), e ); //$NON-NLS-1$
        }
      }

      // stop, if we have errors until now
      final IStatus tmpStatus = logStatus.asMultiStatus( problemMessage );
      if( tmpStatus.matches( IStatus.ERROR | IStatus.CANCEL ) )
        return tmpStatus;

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

    final MultiStatus completeStatus = logStatus.asMultiStatus( problemMessage );
    if( completeStatus.isOK() )
    {
      final String okMessage = Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.29" ); //$NON-NLS-1$
      return logStatus.asMultiStatus( okMessage );
    }

    return completeStatus;
  }

  /**
   * Adds the profile beans as profiles to the tuhh-project. For each profile bean, a new profile file is generated and
   * the profile is added as reference to it.
   */
  private static IStatus importProfiles( final File profDir, final TuhhWspmProject tuhhProject, final ProfileBean[] commonProfiles, final Map<String, IProfileFeature> addedProfiles, final boolean isDirectionUpstreams )
  {
    final IStatusCollector log = new StatusCollector( KalypsoModelWspmTuhhCorePlugin.PLUGIN_ID );

    for( final ProfileBean bean : commonProfiles )
    {
      try
      {
        final IProfileFeature profile = importProfile( profDir, tuhhProject, addedProfiles, bean, isDirectionUpstreams, true );
        if( profile != null )
        {
          final BigDecimal profStation = profile.getBigStation();
          final BigDecimal beanStation = bean.getStation();

          // if( Math.abs( profStation.doubleValue() - beanStation.doubleValue() ) > 0.0001 )
          if( profStation.compareTo( beanStation ) != 0 )
          {
            final BigDecimal fixedStation = fixStation( profStation, beanStation );

            final String msg = Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.19", bean.getFileName(), profStation, bean.getWaterName(), bean.getStateName(), beanStation ); //$NON-NLS-1$
            log.add( IStatus.INFO, msg );

            // FIXME: bad: probably the station of the profproj.txt is rounded, so we are using the worse number here.
            // We should instead use the station with the most significant digits.
            profile.setBigStation( fixedStation );
          }
        }
      }
      catch( final IOException e )
      {
        final String msg = Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.119", bean.getFileName() );//$NON-NLS-1$
        log.add( IStatus.INFO, msg, e );
      }
      catch( final GMLSchemaException e )
      {
        log.add( IStatus.ERROR, e.getLocalizedMessage(), e );
      }
      catch( final CoreException e )
      {
        log.add( e.getStatus() );
      }
    }

    final String okMessage = Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.120" );//$NON-NLS-1$
    return log.asMultiStatusOrOK( okMessage, okMessage );
  }

  /**
   * Returns the station with the most significant decimals.
   */
  private static BigDecimal fixStation( final BigDecimal profStation, final BigDecimal beanStation )
  {
    final int profScale = profStation.scale();
    final int beanScale = beanStation.scale();

    if( profScale > beanScale )
      return profStation;
    else
      return beanStation;
  }

  /**
   * Imports a single profile according to the given ProfileBean. If the map already contains a profile with the same id
   * (usually the filename), we return this instead.
   */
  private static IProfileFeature importProfile( final File profDir, final TuhhWspmProject tuhhProject, final Map<String, IProfileFeature> knownProfiles, final ProfileBean bean, final boolean isDirectionUpstreams, final boolean ignoreMissingFile ) throws GMLSchemaException, IOException, CoreException
  {
    final String fileName = bean.getFileName();

    if( knownProfiles.containsKey( fileName ) )
      return knownProfiles.get( fileName );

    final IProfileFeature prof = tuhhProject.createNewProfile( bean.getWaterName(), isDirectionUpstreams );

    final File prfFile = new File( profDir, fileName );
    if( !prfFile.exists() )
    {
      if( ignoreMissingFile )
      {
        // REMARK: this is a situation that occurs rather often, because WspWin does not correctly keep
        // track of removed profiles (i.e. if a profile is removed from its strand, the profile is not removed
        // from the profproj file. Because of this, we silently ignore htis problem.
        return null;
      }
      else
      {
        // REMARK: if file is missing and referenced from a strand, we really have a problem.
        final String message = String.format( Messages.getString( "WspWinImporter.0" ), fileName ); //$NON-NLS-1$
        final IStatus status = new Status( IStatus.WARNING, KalypsoModelWspmTuhhCorePlugin.PLUGIN_ID, message );
        throw new CoreException( status );
      }
    }

    final String profiletype = IWspmTuhhConstants.PROFIL_TYPE_PASCHE;

    final IProfileSource prfSource = KalypsoModelWspmCoreExtensions.createProfilSource( "prf" ); //$NON-NLS-1$
    final IProfile[] profile = ProfileSerializerUtilitites.readProfile( prfSource, prfFile, profiletype );

    ((ProfileFeatureBinding)prof).setProfile( profile[0] );

    /* Set filename as profile name in order to make it unique */
    prof.setName( FilenameUtils.removeExtension( fileName ) );

    /* Set state as default description for profile. */
    prof.setDescription( bean.getStateName() );

    /* Only add profile if no error occurs */
    knownProfiles.put( fileName, prof );

    return prof;
  }

  /**
   * Adds the zustand-bean to the tuhh-project. Already imported profiles are not imported a second time.
   * <p>
   * If the zustand contains unknown profiles (e.g. due to a wspwin bug), they will be also imported and added to the importedPRofilesMap.
   * </p>
   */
  private static IStatus importTuhhZustand( final TuhhWspmProject tuhhProject, final WspCfg wspCfg, final WspWinZustand zustand, final Map<String, IProfileFeature> importedProfiles, final boolean isDirectionUpstreams, final IContainer targetContainer ) throws GMLSchemaException
  {
    final ZustandBean zustandBean = zustand.getBean();

    final String message = Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.21", zustandBean.getFileName() ); //$NON-NLS-1$
    final IStatusCollector log = new StatusCollector( KalypsoModelWspmTuhhCorePlugin.PLUGIN_ID );

    final String name = zustandBean.getName();
    final String waterName = zustandBean.getWaterName();

    final TuhhReach reach = tuhhProject.createNewReach( waterName, isDirectionUpstreams );

    reach.setName( name );

    final StringBuffer descBuffer = new StringBuffer();
    descBuffer.append( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.22" ) ); //$NON-NLS-1$
    descBuffer.append( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.23" ) + DATE_FORMATTER.format( zustandBean.getDate() ) ); //$NON-NLS-1$

    reach.setDescription( descBuffer.toString() );

    // add reachSegments + profiles (.str)
    // we ignore the profileBeans and just add the segments, so we can even import projects
    // which are slightly inconsistent, maybe add warning messages later
    final ZustandSegmentBean[] segmentBeans = zustand.getSegmentBeans();

    final WspWinProject wspwinProject = wspCfg.getProject();
    final File profDir = wspwinProject.getProfDir();
    // create reachSegments and associated profiles
    for( final ZustandSegmentBean bean : segmentBeans )
    {
      try
      {
        final ProfileBean fromBean = new ProfileBean( waterName, name, bean.getStationFrom(), bean.getFileNameFrom() );
        final IProfileFeature fromProf = importProfile( profDir, tuhhProject, importedProfiles, fromBean, isDirectionUpstreams, false );

        reach.createProfileSegment( fromProf, bean.getStationFrom().doubleValue() );

        if( bean == segmentBeans[segmentBeans.length - 1] )
        {
          // also add last profile
          final ProfileBean toBean = new ProfileBean( waterName, name, bean.getStationTo(), bean.getFileNameTo() );
          final IProfileFeature toProf = importProfile( profDir, tuhhProject, importedProfiles, toBean, isDirectionUpstreams, false );

          reach.createProfileSegment( toProf, bean.getStationTo().doubleValue() );
        }
      }
      catch( final CoreException e )
      {
        log.add( e.getStatus() );
      }
      catch( final Exception e )
      {
        log.add( IStatus.ERROR, e.getLocalizedMessage(), e );
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
    final Map<Integer, String> runOffEventsByCount = new HashMap<>();
    final Map<String, String> runOffEventsByName = new HashMap<>();
    try
    {
      final RunOffEventBean[] runOffEventBeans = zustand.getRunOffEvents();
      int count = 0;
      for( final RunOffEventBean bean : runOffEventBeans )
      {
        final IFeatureBindingCollection<IRunOffEvent> runoffEvents = waterBody.getRunoffEvents();
        final IRunOffEvent newEvent = runoffEvents.addNew( IRunOffEvent.FEATURE_RUNOFF_EVENT );
        final String runoffEventName = baseName + bean.getName();
        writeRunOffBeanIntoFeature( bean, runoffEventName, newEvent );

        // remember for reference from calculation
        runOffEventsByCount.put( count++, newEvent.getId() );
        runOffEventsByName.put( runoffEventName, newEvent.getId() );
      }
    }
    catch( final Exception e )
    {
      log.add( IStatus.ERROR, e.getLocalizedMessage(), e );
    }

    try
    {
      final RunOffEventBean[] wspFixesBeans = zustand.getWspFixations();
      for( final RunOffEventBean bean : wspFixesBeans )
      {
        if( !bean.getEntries().isEmpty() )
        {
          final IFeatureBindingCollection<WspmFixation> fixations = waterBody.getWspFixations();
          final WspmFixation wspFixation = fixations.addNew( WspmFixation.QNAME_FEATURE_WSPM_FIXATION );
          writeRunOffBeanIntoFeature( bean, baseName + bean.getName(), wspFixation );
        }
      }
    }
    catch( final Exception e )
    {
      log.add( IStatus.ERROR, e.getLocalizedMessage(), e );
    }

    // /////////////////////////////////////////// //
    // add einzelverluste / Verlustbeiwerte (.psi) //
    // /////////////////////////////////////////// //

    // map is used to remember station of local energy loss (Einzelverlust/Verlustbeiwert)
    final Map<BigDecimal, Integer> readLocEnergyLosses = new HashMap<>();
    try
    {
      final LocalEnergyLossBean[] locEnergyLossBeans = zustand.getLosses();

      int count = 0;
      for( final LocalEnergyLossBean bean : locEnergyLossBeans )
      {
        // remember station for reference
        readLocEnergyLosses.put( bean.getStation(), count++ );
      }
    }
    catch( final Exception e )
    {
      log.add( IStatus.ERROR, e.getLocalizedMessage(), e );
    }

    for( final LocalEnergyLossBean energyLossBean : zustand.getLosses() )
    {
      final IProfileFeature profileFeature = reach.findProfile( energyLossBean.getStation() );

      final EnergylossProfileObject energyLoss = new EnergylossProfileObject();

      final Pair<String, BigDecimal>[] entries = energyLossBean.getEntries();
      for( final Pair<String, BigDecimal> entry : entries )
      {
        final Energyloss newEnergyloss = new Energyloss( entry.getLeft(), StringUtils.EMPTY, entry.getRight() );
        energyLoss.addEnergyloss( newEnergyloss );
      }

      ((ProfileFeatureBinding)profileFeature).getProfile().addProfileObjects( energyLoss );
    }

    // ///////////////////////////// //
    // add calculations (.ber, .001) //
    // ///////////////////////////// //
    try
    {
      final TYPE type = wspCfg.getType();
      importCalculations( zustand, log, reach, wspwinProject, baseName, runOffEventsByCount, runOffEventsByName, type, targetContainer );
    }
    catch( final Exception e )
    {
      log.add( IStatus.ERROR, e.getLocalizedMessage(), e );
    }

    return log.asMultiStatus( message );
  }

  private static void importCalculations( final WspWinZustand zustand, final IStatusCollector log, final TuhhReach reach, final WspWinProject wspwinProject, final String baseName, final Map<Integer, String> readRunOffEventsByCount, final Map<String, String> runOffEventsByName, final TYPE projectType, final IContainer targetContainer )
  {
    final ICalculationWspmConverter converter = createCalculationConverter( projectType, reach, baseName, readRunOffEventsByCount, runOffEventsByName );

    final File dathDir = wspwinProject.getDathDir();
    final File profDir = wspwinProject.getProfDir();
    final CalculationResultConverter resultConverter = new CalculationResultConverter( projectType, baseName, log, targetContainer );

    final ICalculationContentBean[] calculations = zustand.getCalculations();
    for( final ICalculationContentBean calculation : calculations )
    {
      try
      {
        converter.convert( calculation, profDir );

        final CalculationBean calculationBean = calculation.getCalculationBean();
        resultConverter.convert( calculationBean, dathDir );
      }
      catch( final Exception e )
      {
        log.add( IStatus.ERROR, e.getLocalizedMessage(), e );
      }
    }
  }

  protected static ICalculationWspmConverter createCalculationConverter( final TYPE projectType, final TuhhReach reach, final String baseName, final Map<Integer, String> readRunOffEventsByCount, final Map<String, String> runOffEventsByName )
  {
    switch( projectType )
    {
      case PASCHE:
        return new CalculationPasche2WspmConverter( reach, baseName, readRunOffEventsByCount );

      case KNAUF:
        return new CalculationKnauf2WspmConverter( reach, baseName, runOffEventsByName );

      default:
        throw new IllegalArgumentException();
    }
  }

  private static void writeRunOffBeanIntoFeature( final RunOffEventBean bean, final String name, final IObservationFeature observationFeature )
  {
    final IObservation<TupleResult> obs = observationFeature.toObservation();
    obs.setName( name );
    obs.setDescription( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.WspWinImporter.28" ) ); //$NON-NLS-1$

    final TupleResult result = obs.getResult();

    final int stationIndex = result.indexOfComponent( IRunOffEvent.COMPONENT_STATION );
    final int runoffIndex = result.indexOfComponent( IRunOffEvent.COMPONENT_RUNOFF );
    final int wspIndex = result.indexOfComponent( WspmFixation.COMPONENT_WSP );
    final int valueIndex = runoffIndex == -1 ? wspIndex : runoffIndex;

    final Map<BigDecimal, BigDecimal> values = bean.getEntries();
    for( final Map.Entry<BigDecimal, BigDecimal> entry : values.entrySet() )
    {
      final IRecord record = result.createRecord();
      result.add( record );
      record.setValue( stationIndex, entry.getKey() );
      record.setValue( valueIndex, entry.getValue() );
    }

    // TODO: WSP Fixierung nur schreiben, wenn Anzahl größer 0
    observationFeature.saveObservation( obs );
  }

  /** Returns the content of the prof/probez.txt file */
  private static String readProjectDescription( final WspWinProject wspWinProject ) throws IOException
  {
    final File probezFile = wspWinProject.getProbezFile();

    return FileUtils.readFileToString( probezFile, (Charset)null );
  }
}