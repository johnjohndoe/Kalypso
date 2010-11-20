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
package org.kalypso.model.hydrology.internal.postprocessing;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Date;
import java.util.logging.Logger;
import java.util.zip.ZipOutputStream;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.java.io.filter.MultipleWildCardFileFilter;
import org.kalypso.convert.namodel.DefaultPathGenerator;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.manager.IDManager;
import org.kalypso.convert.namodel.timeseries.Block;
import org.kalypso.convert.namodel.timeseries.BlockTimeSeries;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.model.hydrology.binding.NAControl;
import org.kalypso.model.hydrology.binding.NAModellControl;
import org.kalypso.model.hydrology.binding.NAOptimize;
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
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTupleModel;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.timeseries.TimeseriesUtils;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
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

  // FIXME: move postprocessing regarding optimize elsewhere (into hwv client)
  private final NAOptimize m_naOptimize;

  private final HydroHash m_hydroHash;

  public NaPostProcessor( final NAConfiguration conf, final Logger logger, final GMLWorkspace modelWorkspace, final NAModellControl naControl, final NAOptimize optimize, final HydroHash hydroHash )
  {
    m_conf = conf;
    m_logger = logger;
    m_modelWorkspace = modelWorkspace;
    m_naControl = naControl;
    m_naOptimize = optimize;
    m_hydroHash = hydroHash;
    m_naStatistics = new NAStatistics( logger );
  }

  public void process( final NaAsciiDirs asciiDirs, final NaSimulationDirs simDirs ) throws Exception
  {
    final NaResultDirs currentResultDirs = simDirs.currentResultDirs;
    translateErrorGml( asciiDirs, currentResultDirs );

    copyNaExeLogs( asciiDirs, currentResultDirs );

    m_isSucceeded = checkSuccess( asciiDirs );

    if( !m_isSucceeded )
      return;

    // FIXME: we need (much) better error handling! and error recovery...

    loadTSResults( asciiDirs.outWeNatDir, simDirs.currentResultDir );

    // FIXME: remove this from here
    loadTesultTSPredictionIntervals( simDirs.outputDir );

    copyStatisticResultFile( asciiDirs, currentResultDirs );

    final Date[] initialDates = m_naControl.getInitialDatesToBeWritten();
    final LzsimReader lzsimManager = new LzsimReader( initialDates, currentResultDirs.anfangswertDir );
    lzsimManager.readInitialValues( m_conf.getIdManager(), m_hydroHash, asciiDirs.lzsimDir, m_logger );

    m_naStatistics.writeStatistics( simDirs.currentResultDir, currentResultDirs.reportDir );
  }

  private void loadTesultTSPredictionIntervals( final File resultDir )
  {
    try
    {
      if( m_naOptimize == null )
        return;

      final NAControl metaControl = m_conf.getMetaControl();
      final NaUmhuellendeOperation naUmhuellendeOperation = new NaUmhuellendeOperation( m_naOptimize, metaControl, resultDir );
      naUmhuellendeOperation.execute( new NullProgressMonitor() );
    }
    catch( final InvocationTargetException e )
    {
      m_logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.83", e.getTargetException() ) );
    }
    catch( final CoreException e )
    {
      m_logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.83", e ) );
    }
  }

  private void copyNaExeLogs( final NaAsciiDirs asciiDirs, final NaResultDirs currentResultDirs )
  {
    ZipOutputStream zos = null;
    try
    {
      zos = new ZipOutputStream( new BufferedOutputStream( new FileOutputStream( currentResultDirs.exe_logs_zip ) ) );
      // REMARK: We rename the files in the zip, else the windoes explorer will show an empty zip by default (unknown
      // extensions)
      ZipUtilities.writeZipEntry( zos, asciiDirs.output_res, "output.txt" ); //$NON-NLS-1$
      ZipUtilities.writeZipEntry( zos, asciiDirs.output_err, "error.txt" ); //$NON-NLS-1$
      zos.close();
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      final String msg = String.format( "Failed to copy Kalypso-NA log files.", e.getLocalizedMessage() );
      m_logger.severe( msg );
    }
    finally
    {
      IOUtils.closeQuietly( zos );
    }
  }

  /**
   * Translates the id inside the KalypsoNA log to KalypsoHydrology id's.
   */
  private void translateErrorGml( final NaAsciiDirs asciiDirs, final NaResultDirs resultDirs )
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
        m_logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.220", bilFile.getName() ) );
        final File resultFile = new File( resultDirs.bilanzDir, "Bilanz.txt" ); //$NON-NLS-1$ 
        FileUtils.copyFile( bilFile, resultFile );
      }
      catch( final IOException e )
      {
        final String inputPath = outWeNatDir.getName() + bilFile.getName();
        e.printStackTrace();

        final String msg = Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.224", inputPath, e.getLocalizedMessage() );
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
      m_logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.123" ) + qgsFiles[0].getName() + "\n" ); //$NON-NLS-2$
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

        // FIXME: wrong: we must consider the element type here: else we might read a catchment node for a result result
        final String key = Integer.toString( idManager.getAsciiID( resultFeature ) );

        final ITupleModel qTuppelModel = readBlockDataForKey( ts, key, descriptor );
        if( qTuppelModel == null )
        {
          m_logger.info( String.format( "Missing result data for element: %s", key ) ); //$NON-NLS-1$
          continue;
        }

        m_logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.125", key, resultFeature.getFeatureType().getQName(), suffix ) + "\n" ); //$NON-NLS-2$

        MetadataList metadataList = new MetadataList();

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
            m_logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.132" ) );
            final IObservation pegelObservation = ZmlFactory.parseXML( pegelURL );

            metadataList = (MetadataList) pegelObservation.getMetadataList().clone();
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

  private ITupleModel readBlockDataForKey( final BlockTimeSeries ts, final String key, final TSResultDescriptor descriptor )
  {
    if( !ts.dataExistsForKey( key ) )
      return null;

    final String suffix = descriptor.name();
    final double resultFactor = descriptor.getResultFactor();
    final String resultType = descriptor.getAxisType();

    // transform data to tuppelmodel
    final Block block = ts.getTimeSerie( key );

    final Date[] dates = block.getDates();

    final Object[][] tupelData = new Object[dates.length][3];
    int pos = 0;
    for( final Date date : dates )
    {
      final Double value = block.getValue( date );
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
    final IAxis qAxis = new DefaultAxis( axisTitle, resultType, TimeseriesUtils.getUnit( resultType ), Double.class, false );
    final IAxis statusAxis = KalypsoStatusUtils.createStatusAxisFor( qAxis, true );
    final IAxis[] axis = new IAxis[] { dateAxis, qAxis, statusAxis };
    return new SimpleTupleModel( axis, tupelData );
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
}
