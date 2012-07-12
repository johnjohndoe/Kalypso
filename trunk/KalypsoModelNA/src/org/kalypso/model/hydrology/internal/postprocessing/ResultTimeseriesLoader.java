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

import java.io.File;
import java.util.Date;
import java.util.logging.Logger;

import org.kalypso.contribs.java.io.filter.MultipleWildCardFileFilter;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.channels.StorageChannel;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.postprocessing.statistics.NAStatistics;
import org.kalypso.model.hydrology.internal.postprocessing.statistics.NAStatisticsData;
import org.kalypso.model.hydrology.timeseries.TSResultDescriptor;
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
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Converts result timeseries of Kalypso-NA.exe to zml's.
 *
 * @author Gernot Belger
 */
public class ResultTimeseriesLoader
{
  private final File m_inputDir;

  private final GMLWorkspace m_modelWorkspace;

  private final IDManager m_idManager;

  private final Logger m_logger;

  private final File m_outputDir;

  private final NAStatistics m_naStatistics;

  private final ENACoreResultsFormat m_resultsFormat;

  public ResultTimeseriesLoader( final File inputDir, final File outputDir, final GMLWorkspace modelWorkspace, final IDManager idManager, final ENACoreResultsFormat resultsFormat, final Logger logger )
  {
    m_inputDir = inputDir;
    m_outputDir = outputDir;
    m_modelWorkspace = modelWorkspace;
    m_idManager = idManager;
    m_resultsFormat = resultsFormat;
    m_logger = logger;

    m_naStatistics = new NAStatistics( logger );
  }

  public void processResults( ) throws SensorException
  {
    final TSResultDescriptor[] descriptors = TSResultDescriptor.values();
    for( final TSResultDescriptor descriptor : descriptors )
      loadTSResults( descriptor );
  }

  private void loadTSResults( final TSResultDescriptor descriptor ) throws SensorException
  {
    final String suffix = descriptor.name();

    final MultipleWildCardFileFilter filter = new MultipleWildCardFileFilter( new String[] { "*" + suffix + "*" }, false, false, true ); //$NON-NLS-1$ //$NON-NLS-2$
    final File[] qgsFiles = m_inputDir.listFiles( filter );
    if( qgsFiles.length == 0 )
      return;

    m_logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.123" ) + qgsFiles[0].getName() + "\n" ); //$NON-NLS-1$//$NON-NLS-2$

    final BlockTimeSeries ts = new BlockTimeSeries( m_resultsFormat );
    ts.importBlockFile( qgsFiles[0] );

    final Feature[] resultFeatures = FeatureHelper.getFeaturesWithName( m_modelWorkspace, descriptor.getFeatureType() );
    for( final Feature resultFeature : resultFeatures )
      processResultFeature( resultFeature, descriptor, ts );
  }

  private void processResultFeature( final Feature resultFeature, final TSResultDescriptor descriptor, final BlockTimeSeries ts ) throws SensorException
  {
    if( resultFeature instanceof Node && !((Node) resultFeature).isGenerateResults() )
      return;

    if( resultFeature instanceof Catchment && !((Catchment) resultFeature).isGenerateResults() )
      return;

    if( resultFeature instanceof StorageChannel && !((StorageChannel) resultFeature).isGenerateResults() )
      return;

    // FIXME: wrong: we must consider the element type here: else we might read a catchment node for a result result
    final String key = Integer.toString( m_idManager.getAsciiID( resultFeature ) );

    final ITupleModel qTuppelModel = readBlockDataForKey( ts, key, descriptor );
    if( qTuppelModel == null )
    {
      m_logger.info( String.format( "Missing result data for element: %s", key ) ); //$NON-NLS-1$
      return;
    }

    m_logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.125", key, resultFeature.getFeatureType().getQName(), descriptor ) + "\n" ); //$NON-NLS-1$//$NON-NLS-2$

    final String resultPathRelative = DefaultPathGenerator.generateResultPathFor( resultFeature, descriptor, null );

    final File resultFile = tweakResultPath( m_outputDir, resultPathRelative, resultFeature, descriptor );
    resultFile.getParentFile().mkdirs();

    // create observation object
    final String titleForObservation = DefaultPathGenerator.generateTitleForObservation( resultFeature, descriptor );

    final MetadataList metadataList = getMetadata( resultFeature );

    final IObservation resultObservation = new SimpleObservation( resultPathRelative, titleForObservation, metadataList, qTuppelModel ); //$NON-NLS-1$
    ZmlFactory.writeToFile( resultObservation, resultFile );

    if( TSResultDescriptor.qgs == descriptor && resultFeature instanceof Node )
    {
      final NAStatisticsData data = new NAStatisticsData( resultObservation, resultFile );
      m_naStatistics.add( resultFeature, data );
    }
  }

  /**
   * For nodes, we copy the metadata of the pegel.
   */
  private MetadataList getMetadata( final Feature resultFeature ) throws SensorException
  {
    // TODO: this is mainly needed for hwv; we could use the naoptimize-schema instead and copy the data during creation
// of the official result timeseries.

    // TODO: read every input timeseries (again) to get the metadata. Better: keep metadata from pre-processing instead.
    if( resultFeature instanceof Node )
    {
      final ZmlLink pegelLink = ((Node) resultFeature).getPegelLink();
      final IObservation pegelObservation = pegelLink.loadObservation();
      if( pegelObservation != null )
        return (MetadataList) pegelObservation.getMetadataList().clone();
    }

    return new MetadataList();
  }

  private ITupleModel readBlockDataForKey( final BlockTimeSeries ts, final String key, final TSResultDescriptor descriptor )
  {
    if( !ts.dataExistsForKey( key ) )
      return null;

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

    final String axisTitle = descriptor.getAxisTitle();
    final IAxis dateAxis = new DefaultAxis( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.4" ), ITimeseriesConstants.TYPE_DATE, "", Date.class, true ); //$NON-NLS-1$ //$NON-NLS-2$
    final IAxis qAxis = new DefaultAxis( axisTitle, resultType, TimeseriesUtils.getUnit( resultType ), Double.class, false );
    final IAxis statusAxis = KalypsoStatusUtils.createStatusAxisFor( qAxis, true );
    final IAxis[] axis = new IAxis[] { dateAxis, qAxis, statusAxis };
    return new SimpleTupleModel( axis, tupelData );
  }

  private File tweakResultPath( final File resultDir, final String resultPathRelative, final Feature resultFeature, final TSResultDescriptor descriptor )
  {
    final File resultFile = new File( resultDir, resultPathRelative ); //$NON-NLS-1$
    if( !resultFile.exists() )
      return resultFile;

    // FIXME: Arrg! Is this really possible to happen? Most probably something else is wrong. We should not
    // do such terrible things here!
    m_logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.136", resultPathRelative ) ); //$NON-NLS-1$
    final String extra = "(ID" + Integer.toString( m_idManager.getAsciiID( resultFeature ) ).trim() + ")"; //$NON-NLS-1$ //$NON-NLS-2$
    final String resultPath = DefaultPathGenerator.generateResultPathFor( resultFeature, descriptor, extra ); //$NON-NLS-1$ //$NON-NLS-2$
    m_logger.info( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.140", resultPath ) ); //$NON-NLS-1$

    return new File( resultDir, resultPath ); //$NON-NLS-1$
  }

  public NAStatistics getStatistics( )
  {
    return m_naStatistics;
  }
}
