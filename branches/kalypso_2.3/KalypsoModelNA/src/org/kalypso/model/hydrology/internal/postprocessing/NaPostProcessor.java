/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.hydrology.internal.postprocessing;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.io.FileUtils;
import org.kalypso.commons.lhwz.LhwzHelper;
import org.kalypso.contribs.java.io.filter.MultipleWildCardFileFilter;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.convert.namodel.DefaultPathGenerator;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.manager.IDManager;
import org.kalypso.convert.namodel.timeseries.Block;
import org.kalypso.convert.namodel.timeseries.BlockTimeSeries;
import org.kalypso.convert.namodel.timeseries.NATimeSettings;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.model.hydrology.binding.NAControl;
import org.kalypso.model.hydrology.binding.NAModellControl;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.Node;
import org.kalypso.model.hydrology.binding.model.StorageChannel;
import org.kalypso.model.hydrology.internal.NaAsciiDirs;
import org.kalypso.model.hydrology.internal.NaResultDirs;
import org.kalypso.model.hydrology.internal.NaSimulationDirs;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.postprocessing.statistics.NAStatistics;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.HydroHash;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IAxisRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTupleModel;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.timeseries.envelope.TranProLinFilterUtilities;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Gernot Belger
 */
public class NaPostProcessor
{
  private static final String SUFFIX_QGS = "qgs";

  private static final String STRING_RESULT_SUCCESSFUL_1 = "berechnung wurde ohne fehler beendet"; //$NON-NLS-1$

  private static final String STRING_RESULT_SUCCESSFUL_2 = "Berechnung wurde ohne Fehler beendet!";

  private static final String FILENAME_OUTPUT_RES = "output.res"; //$NON-NLS-1$

  private final NAStatistics m_naStatistics;

  private final NAConfiguration m_conf;

  private final Logger m_logger;

  private final GMLWorkspace m_modelWorkspace;

  private boolean m_isSucceeded;

  private final NAModellControl m_naControl;

  private final HydroHash m_hydroHash;

  public NaPostProcessor( final NAConfiguration conf, final Logger logger, final GMLWorkspace modelWorkspace, final NAModellControl naControl, final HydroHash hydroHash )
  {
    m_conf = conf;
    m_logger = logger;
    m_modelWorkspace = modelWorkspace;
    m_naControl = naControl;
    m_hydroHash = hydroHash;
    m_naStatistics = new NAStatistics( logger );
  }

  public void process( final NaAsciiDirs asciiDirs, final NaSimulationDirs simDirs ) throws Exception
  {
    final NaResultDirs currentResultDirs = simDirs.currentResultDirs;
    translateLogs( asciiDirs, currentResultDirs );

    m_isSucceeded = checkSuccess( asciiDirs );

    if( !m_isSucceeded )
      return;

    // FIXME: (much) better error handling! and error recovery...

    loadTSResults( asciiDirs.outWeNatDir, simDirs.currentResultDir );
    try
    {
      loadTesultTSPredictionIntervals( simDirs.outputDir );
    }
    catch( final Exception e )
    {
      m_logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.83", e.getLocalizedMessage() ) ); //$NON-NLS-1$
    }

    copyStatisticResultFile( asciiDirs, currentResultDirs );

    final Date[] initialDates = m_naControl.getInitialDatesToBeWritten();
    final LzsimReader lzsimManager = new LzsimReader( initialDates, currentResultDirs.anfangswertDir );
    lzsimManager.readInitialValues( m_conf.getIdManager(), m_hydroHash, asciiDirs.lzsimDir, m_logger );

    m_naStatistics.writeStatistics( simDirs.currentResultDir, currentResultDirs.reportDir );
  }

  /**
   * Translates the id inside the KalypsoNA log to KalypsoHydrology id's.
   */
  private void translateLogs( final NaAsciiDirs asciiDirs, final NaResultDirs resultDirs )
  {
    final IDManager idManager = m_conf.getIdManager();
    final NaFortranLogTranslater logTranslater = new NaFortranLogTranslater( asciiDirs.asciiDir, idManager, m_logger );

    final File resultFile = new File( resultDirs.logDir, "error.gml" ); //$NON-NLS-1$
    resultFile.getParentFile().mkdirs();

    logTranslater.translate( resultFile );
  }

  private boolean checkSuccess( final NaAsciiDirs asciiDirs )
  {
    final String logContent = readOutputRes( asciiDirs.startDir );
    if( logContent == null )
      return false;

    if( logContent.contains( STRING_RESULT_SUCCESSFUL_1 ) )
      return true;
    if( logContent.contains( STRING_RESULT_SUCCESSFUL_2 ) )
      return true;

    return false;
  }

  private String readOutputRes( final File startDir )
  {
    try
    {
      return FileUtils.readFileToString( new File( startDir, FILENAME_OUTPUT_RES ), null );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      return null;
    }
  }

  public boolean isSucceeded( )
  {
    return m_isSucceeded;
  }

  /** kopiere statistische Ergebnis-Dateien */
  // FIXME: why copy? bilanz does not belong to ascii and should be directly written to result dirs
  private void copyStatisticResultFile( final NaAsciiDirs asciiDirs, final NaResultDirs resultDirs )
  {
    resultDirs.bilanzDir.mkdirs();

    final String[] wildcards = new String[] { "*bil*" }; //$NON-NLS-1$
    final MultipleWildCardFileFilter filter = new MultipleWildCardFileFilter( wildcards, false, false, true );

    final File outWeNatDir = asciiDirs.outWeNatDir;
    final File[] bilFiles = outWeNatDir.listFiles( filter );
    for( final File bilFile : bilFiles )
    {
      try
      {
        m_logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.220", bilFile.getName() ) ); //$NON-NLS-1$ //$NON-NLS-2$
        final File resultFile = new File( resultDirs.bilanzDir, "Bilanz.txt" ); //$NON-NLS-1$ 
        FileUtils.copyFile( bilFile, resultFile );
      }
      catch( final IOException e )
      {
        final String inputPath = outWeNatDir.getName() + bilFile.getName();
        e.printStackTrace();

        final String msg = Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.224", inputPath, e.getLocalizedMessage() ); //$NON-NLS-1$ //$NON-NLS-2$
        m_logger.severe( msg );
      }
    }
  }

  private void loadTSResults( final File outWeNatDir, final File resultDir ) throws MalformedURLException, SensorException
  {
    final TSResultDescriptor[] descriptors = TSResultDescriptor.values();
    for( final TSResultDescriptor descriptor : descriptors )
      loadTSResults( outWeNatDir, resultDir, descriptor );
  }

  private void loadTSResults( final File outWeNatDir, final File resultDir, final TSResultDescriptor descriptor ) throws MalformedURLException, SensorException
  {
    final IFeatureType resultFT = m_modelWorkspace.getGMLSchema().getFeatureType( descriptor.getFeatureType() );
    final String titlePropName = "name";

    final String suffix = descriptor.name();

    final String metadataTSLink = descriptor.getMetadataTSLink();
    final String targetTSLink = descriptor.getTargetTSLink();

    final IDManager idManager = m_conf.getIdManager();
    // ASCII-Files
    // generiere ZML Ergebnis Dateien
    final MultipleWildCardFileFilter filter = new MultipleWildCardFileFilter( new String[] { "*" + suffix + "*" }, false, false, true ); //$NON-NLS-1$ //$NON-NLS-2$
    final File[] qgsFiles = outWeNatDir.listFiles( filter );
    if( qgsFiles.length != 0 )
    {
      // read ascii result file
      m_logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.123" ) + qgsFiles[0].getName() + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
      final BlockTimeSeries ts = new BlockTimeSeries();
      ts.importBlockFile( qgsFiles[0] );

      // iterate model nodes/Catchments/rhbChannels and generate zml

      final Feature[] resultFeatures = m_modelWorkspace.getFeatures( resultFT );
      for( final Feature resultFeature : resultFeatures )
      {
        if( resultFeature instanceof Node && !((Node) resultFeature).isGenerateResults() )
          continue;

        if( resultFeature instanceof Catchment && !((Catchment) resultFeature).isGenerateResults() )
          continue;

        if( resultFeature instanceof StorageChannel && !((StorageChannel) resultFeature).isGenerateResults() )
          continue;

        final String key = Integer.toString( idManager.getAsciiID( resultFeature ) );

        final ITupleModel qTuppelModel = readBlockDataForKey( ts, key, descriptor );
        if( qTuppelModel == null )
        {
          m_logger.info( String.format( "Missing result data for element: %s", key ) ); //$NON-NLS-1$
          continue;
        }

        m_logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.125", key, resultFeature.getFeatureType().getQName(), suffix ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$

        final MetadataList metadataList = new MetadataList();

        // if pegel exists, copy metadata (inclusive wq-function)
        TimeseriesLinkType pegelLink = null;
        if( metadataTSLink != null )
          pegelLink = (TimeseriesLinkType) resultFeature.getProperty( metadataTSLink );
        if( pegelLink != null )
        {
          final URL pegelURL = new URL( m_modelWorkspace.getContext(), pegelLink.getHref() );
          boolean itExists;
          // test if url exists
          try
          {
            pegelURL.openStream();
            itExists = true;
          }
          catch( final Exception e )
          {
            itExists = false;
          }
          if( itExists )
          {
            m_logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.132" ) ); //$NON-NLS-1$
            final IObservation pegelObservation = ZmlFactory.parseXML( pegelURL ); //$NON-NLS-1$

            copyMetaData( pegelObservation.getMetadataList(), metadataList, new String[] { ITimeseriesConstants.MD_ALARM_1, ITimeseriesConstants.MD_ALARM_2, ITimeseriesConstants.MD_ALARM_3,
                ITimeseriesConstants.MD_ALARM_4, ITimeseriesConstants.MD_GEWAESSER, ITimeseriesConstants.MD_FLUSSGEBIET, ITimeseriesConstants.MD_GKH, ITimeseriesConstants.MD_GKR,
                ITimeseriesConstants.MD_HOEHENANGABEART, ITimeseriesConstants.MD_PEGELNULLPUNKT, ITimeseriesConstants.MD_WQWECHMANN, ITimeseriesConstants.MD_WQTABLE, ITimeseriesConstants.MD_TIMEZONE,
                ITimeseriesConstants.MD_VORHERSAGE_START, ITimeseriesConstants.MD_VORHERSAGE_ENDE } );
          }
        }

        // lese ergebnis-link um target fuer zml zu finden
        String resultPathRelative = ""; //$NON-NLS-1$
        if( targetTSLink != null )
        {
          try
          {
            final TimeseriesLinkType resultLink = (TimeseriesLinkType) resultFeature.getProperty( targetTSLink );
            if( resultLink == null )
            {
              m_logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.134", resultFeature.getId() ) ); //$NON-NLS-1$ 
              resultPathRelative = DefaultPathGenerator.generateResultPathFor( resultFeature, titlePropName, suffix, null );
            }
            else
            {
              final String href = resultLink.getHref();

              // WTF!?
              // resultPathRelative = href.substring( 19 );

              resultPathRelative = "Pegel" + href.substring( href.lastIndexOf( "/" ) ); //$NON-NLS-1$ //$NON-NLS-2$
            }
          }
          catch( final Exception e )
          {
            // if there is target defined or there are some problems with that
            // we generate one
            resultPathRelative = DefaultPathGenerator.generateResultPathFor( resultFeature, titlePropName, suffix, null );
          }
        }
        else
        {
          resultPathRelative = DefaultPathGenerator.generateResultPathFor( resultFeature, titlePropName, suffix, null );
        }

        final File resultFile = tweakResultPath( resultDir, resultPathRelative, resultFeature, titlePropName, suffix );
        resultFile.getParentFile().mkdirs();

        // FIXME: Performance: use this observation to calculate statistics,
        // no need to read the observation a second time
        if( SUFFIX_QGS.equals( suffix ) )
          m_naStatistics.add( resultFeature, resultFile );

        // create observation object
        final String titleForObservation = DefaultPathGenerator.generateTitleForObservation( resultFeature, titlePropName, suffix );

        final IObservation resultObservation = new SimpleObservation( resultPathRelative, titleForObservation, metadataList, qTuppelModel ); //$NON-NLS-1$

        ZmlFactory.writeToFile( resultObservation, resultFile );
      }
    }
  }

  private ITupleModel readBlockDataForKey( BlockTimeSeries ts, String key, TSResultDescriptor descriptor )
  {
    if( !ts.dataExistsForKey( key ) )
      return null;

    final String suffix = descriptor.name();
    final double resultFactor = descriptor.getResultFactor();
    final String resultType = descriptor.getAxisType();

    // transform data to tuppelmodel
    final Block block = ts.getTimeSerie( key );

    Date[] dates = block.getDates();

    final Object[][] tupelData = new Object[dates.length][3];
    int pos = 0;
    for( Date date : dates )
    {
      Double value = block.getValue(date);
      tupelData[pos][0] = date;

      if( value.isNaN() )
      {
        tupelData[pos][1] = 0.0;
        tupelData[pos][2] = new Integer( KalypsoStati.BIT_CHECK );
      }
      else
      {
        tupelData[pos][1] = value * resultFactor;
        tupelData[pos][2] = new Integer( KalypsoStati.BIT_OK );
      }

      pos++;
    }

    final String axisTitle = TSResultDescriptor.getAxisTitleForSuffix( suffix );
    final IAxis dateAxis = new DefaultAxis( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.4" ), ITimeseriesConstants.TYPE_DATE, "", Date.class, true ); //$NON-NLS-1$ //$NON-NLS-2$
    final IAxis qAxis = new DefaultAxis( axisTitle, resultType, TimeserieUtils.getUnit( resultType ), Double.class, false );
    final IAxis statusAxis = KalypsoStatusUtils.createStatusAxisFor( qAxis, true );
    final IAxis[] axis = new IAxis[] { dateAxis, qAxis, statusAxis };
    return new SimpleTupleModel( axis, tupelData );
  }

  private static void copyMetaData( final MetadataList srcMeta, final MetadataList destMeta, final String[] mdKeys )
  {
    for( final String key : mdKeys )
    {
      final String property = srcMeta.getProperty( key );
      if( property != null )
        destMeta.put( key, property );
    }
  }

  private File tweakResultPath( final File resultDir, final String resultPathRelative, final Feature resultFeature, final String titlePropName, final String suffix )
  {
    final File resultFile = new File( resultDir, resultPathRelative ); //$NON-NLS-1$
    if( !resultFile.exists() )
      return resultFile;

    // FIXME: Arrg! Is this really possible to happen? Most probably something else is wrong. We should not
    // do such terrible things here!
    m_logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.136", resultPathRelative ) ); //$NON-NLS-1$
    final IDManager idManager = m_conf.getIdManager();
    final String extra = "(ID" + Integer.toString( idManager.getAsciiID( resultFeature ) ).trim() + ")";
    final String resultPath = DefaultPathGenerator.generateResultPathFor( resultFeature, titlePropName, suffix, extra ); //$NON-NLS-1$ //$NON-NLS-2$
    m_logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.140", resultPath ) ); //$NON-NLS-1$

    return new File( resultDir, resultPath ); //$NON-NLS-1$
  }

  // FIXME: this is hwv specific. Should be done in a separate task after the real calculation
  private void loadTesultTSPredictionIntervals( final File resultDir ) throws Exception
  {
    // Load the calculated prediction
    final IObservation resultObservation = loadPredictedResult( resultDir );
    final IAxis[] axisList = resultObservation.getAxisList();
    final String axisType = determineTranpolinAxis( resultObservation );

    final File fileMitte = getAblageFileFor( resultDir, m_naControl.getQAblageMittlererLink() );
    final File fileUnten = getAblageFileFor( resultDir, m_naControl.getQAblageUntererLink() );
    final File fileOben = getAblageFileFor( resultDir, m_naControl.getQAblageObererLink() );

    // Initalize some commen variables
    final ITupleModel resultValues = resultObservation.getValues( null );
    final IAxis resultDateAxis = ObservationUtilities.findAxisByClass( axisList, Date.class );
    final IAxis resultValueAxis = ObservationUtilities.findAxisByType( axisList, axisType );

    final NAControl metaControl = m_conf.getMetaControl();
    final Date startForecast = metaControl.getStartForecast();
    final Date endForecast = metaControl.getSimulationEnd();

    final IAxisRange rangeFor = resultValues.getRangeFor( resultDateAxis );
    final Date endPrediction = (Date) rangeFor.getUpper();

    final NATimeSettings timeSettings = NATimeSettings.getInstance();
    final Calendar calBegin = timeSettings.getCalendar( startForecast );
    // REMARK: using endPrediction instead of endForecast, as they are not equals (but they should...)
    final Calendar calEnd = timeSettings.getCalendar( endPrediction );

    final double calcStartValue = ObservationUtilities.getInterpolatedValueAt( resultValues, resultDateAxis, resultValueAxis, startForecast );
    final double calcEndValue = ObservationUtilities.getInterpolatedValueAt( resultValues, resultDateAxis, resultValueAxis, endForecast );

    //
    // First, we adapt the result: correction at start and/or end of the calculated timeserie
    //

    double deltaMeasureCalculation;
    try
    {
      final URL pegelURL = getMeasuredURL();

      // from measuered timeseries
      final IObservation pegelObservation = ZmlFactory.parseXML( pegelURL ); //$NON-NLS-1$
      final ITupleModel pegelValues = pegelObservation.getValues( null );
      final IAxis pegelDateAxis = ObservationUtilities.findAxisByClass( pegelObservation.getAxisList(), Date.class );
      final IAxis pegelValueAxis = ObservationUtilities.findAxisByType( pegelObservation.getAxisList(), axisType );
      final double measureValue = ObservationUtilities.getInterpolatedValueAt( pegelValues, pegelDateAxis, pegelValueAxis, startForecast );
      deltaMeasureCalculation = measureValue - calcStartValue;
    }
    catch( final Exception e )
    {
      deltaMeasureCalculation = 0;
    }

    final double offsetStartPrediction;
    final double offsetEndPrediction;

    if( m_naControl.doUseOffsetStartPred() )
      offsetStartPrediction = deltaMeasureCalculation;
    else
      offsetStartPrediction = 0;

    if( m_naControl.doUseOffsetEndPred() )
      offsetEndPrediction = deltaMeasureCalculation;
    else
      offsetEndPrediction = 0;

    final Calendar tranpolinEnd = timeSettings.getCalendar( startForecast );
    tranpolinEnd.add( Calendar.HOUR, 24 );

    final IRequest request = new ObservationRequest( calBegin.getTime(), calEnd.getTime() );
    TranProLinFilterUtilities.transformAndWrite( resultObservation, calBegin, tranpolinEnd, offsetStartPrediction, offsetEndPrediction, "+", axisType, KalypsoStati.BIT_DERIVATED, fileMitte, " - Spur Mitte", request ); //$NON-NLS-1$ //$NON-NLS-2$

    // read the freshly created file into a new observation, we are going to umh�ll it
    final IObservation adaptedResultObservation = ZmlFactory.parseXML( fileMitte.toURI().toURL() ); //$NON-NLS-1$

    //
    // Second, we build the umh�llenden for the adapted result
    //
    double accuracyPrediction = LhwzHelper.getDefaultUmhuellendeAccuracy();
    final Double featureAccuracy = m_naControl.getPredictionAccuracy();
    if( featureAccuracy == null )
      m_logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.44", accuracyPrediction ) ); //$NON-NLS-1$ 
    else
      accuracyPrediction = featureAccuracy.doubleValue();

    // accuracyPrediction // %/60h
    final long millisOf60hours = 1000 * 60 * 60 * 60;
    // endAccuracy: %/simulationRange
    final double endAccuracy = accuracyPrediction * (((double) (endForecast.getTime() - startForecast.getTime())) / ((double) millisOf60hours));

    final double endOffset = calcEndValue * (endAccuracy / 100);

    TranProLinFilterUtilities.transformAndWrite( adaptedResultObservation, calBegin, calEnd, 0, endOffset, "-", axisType, KalypsoStati.BIT_DERIVATED, fileUnten, " - spur Unten", request ); //$NON-NLS-1$ //$NON-NLS-2$
    TranProLinFilterUtilities.transformAndWrite( adaptedResultObservation, calBegin, calEnd, 0, endOffset, "+", axisType, KalypsoStati.BIT_DERIVATED, fileOben, " - spur Oben", request ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  private IObservation loadPredictedResult( final File resultDir ) throws MalformedURLException, SensorException
  {
    final TimeseriesLinkType resultLink = m_naControl.getResultLink();

    // from predicted timeseries
    final UrlResolver urlResolver = new UrlResolver();
    final URL resultURL = urlResolver.resolveURL( resultDir.toURI().toURL(), resultLink.getHref() );
    return ZmlFactory.parseXML( resultURL ); //$NON-NLS-1$
  }

  /**
   * Return with which axis we are going to transform the umh�llenden. Q or W, depending on what is present)
   */
  private String determineTranpolinAxis( final IObservation observation ) throws SimulationException
  {
    final IAxis[] axisList = observation.getAxisList();

    if( ObservationUtilities.hasAxisOfType( axisList, ITimeseriesConstants.TYPE_RUNOFF ) )
      return ITimeseriesConstants.TYPE_RUNOFF;

    if( ObservationUtilities.hasAxisOfType( axisList, ITimeseriesConstants.TYPE_WATERLEVEL ) )
      return ITimeseriesConstants.TYPE_WATERLEVEL;

    throw new SimulationException( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.50" ), null ); //$NON-NLS-1$
  }

  private File getAblageFileFor( final File resultDir, final TimeseriesLinkType trackLink )
  {
    try
    {
      final String href = trackLink.getHref();
      final File resultFile = new File( resultDir, href );
      resultFile.getParentFile().mkdirs();
      return resultFile;
    }
    catch( final Exception e )
    {
      Logger.getAnonymousLogger().log( Level.WARNING, e.getLocalizedMessage() );
      return null; // no track available
    }
  }

  // FIXME: does not belong into this class
  public URL getMeasuredURL( ) throws MalformedURLException
  {
    final TimeseriesLinkType link = m_naControl.getPegelZRLink();
    if( link == null )
      return null;

    // optionen loeschen
    final String href = link.getHref().replaceAll( "\\?.*", "" ); //$NON-NLS-1$ //$NON-NLS-2$
    final URL zmlContext = m_conf.getZMLContext();
    return new URL( zmlContext, href );
  }
}
