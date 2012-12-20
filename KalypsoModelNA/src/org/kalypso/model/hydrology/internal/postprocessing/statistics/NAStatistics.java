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
package org.kalypso.model.hydrology.internal.postprocessing.statistics;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TimeZone;
import java.util.TreeMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.statistics.INaStatistics;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTupleModel;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.timeseries.TimeseriesUtils;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class NAStatistics implements INaStatistics
{
  /**
   * Comparator that ensures to write the statistical entries in alphabetical order.
   */
  public class NAStatisticComparator implements Comparator<Feature>
  {
    @Override
    public int compare( final Feature o1, final Feature o2 )
    {
      final String title1 = getNodeTitle( o1 );
      final String title2 = getNodeTitle( o2 );
      return title1.compareToIgnoreCase( title2 );
    }
  }

  // REMARK: using extension 'txt', so excel opens the file correctly
  private static final String FILENAME_CSV = "statistics.txt"; //$NON-NLS-1$

  private static final String FILENAME_ZML = "statistics.zml"; //$NON-NLS-1$

  private final SimpleDateFormat m_csvDateFormat = new SimpleDateFormat( "dd.MM.yyyy HH:mm:ss" ); //$NON-NLS-1$

  private static final char SEPARATOR_CSV = '\t'; //$NON-NLS-1$

  private final Map<Feature, NAStatisticsData> m_resultMap = new TreeMap<>( new NAStatisticComparator() );

  private final Logger m_logger;

  public NAStatistics( final Logger logger )
  {
    m_logger = logger;
    m_csvDateFormat.setTimeZone( KalypsoCorePlugin.getDefault().getTimeZone() );
  }

  public void writeStatistics( final File inputDir, final File reportDir )
  {
    try
    {
      final Object[][] result = gatherData( inputDir );

      reportDir.mkdirs();

      final IObservation observation = createObservation( result );

      writeZml( observation, reportDir );
      writeCsv( observation, reportDir );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final String msg = String.format( Messages.getString( "NAStatistics.0" ), e.getLocalizedMessage() ); //$NON-NLS-1$
      m_logger.severe( msg );
    }
  }

  private IObservation createObservation( final Object[][] resultValues )
  {
    final IAxis[] resultAxes = createAxes();
    final ITupleModel resultTuppleModel = new SimpleTupleModel( resultAxes, resultValues );
    return new SimpleObservation( FILENAME_ZML, Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.167" ), new MetadataList(), resultTuppleModel ); //$NON-NLS-1$
  }

  private Object[][] gatherData( final File inputDir ) throws SensorException
  {
    final List<Object[]> resultValuesList = new ArrayList<>();

    for( final Entry<Feature, NAStatisticsData> entry : m_resultMap.entrySet() )
    {
      final Feature feature = entry.getKey();
      final NAStatisticsData data = entry.getValue();
      data.calculateStatistics();

      final File resultFile = data.getResultFile();

      final String nodeTitle = getNodeTitle( feature );
      final String nodeDescription = getNodeDescription( feature );

      final Double maxValue = data.getMaxValue();
      final Date maxValueDate = data.getMaxValueDate();
      final Double volume = data.getVolume();
      if( maxValueDate == null )
        m_logger.log( Level.WARNING, Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.157", resultFile.getAbsolutePath() ) ); //$NON-NLS-1$ //$NON-NLS-2$

      final String relativePath = getResultRelativePath( inputDir, resultFile );

      // We write it anyways....
      final Object[] row = new Object[] { nodeTitle, nodeDescription, maxValueDate, maxValue, relativePath, volume };
      resultValuesList.add( row );
    }

    return resultValuesList.toArray( new Object[0][] );
  }

  private String getResultRelativePath( final File inputDir, final File resultFile )
  {
    final String relativePath = FileUtilities.getRelativePathTo( inputDir, resultFile );
    final String relativePathWithSlashes = relativePath.replace( '\\', '/' ); //$NON-NLS-1$ //$NON-NLS-2$
    if( relativePathWithSlashes.startsWith( "/" ) ) //$NON-NLS-1$
      return relativePathWithSlashes.substring( 1 );

    return relativePathWithSlashes;
  }

  static String getNodeDescription( final Feature feature )
  {
    final String featureDescription = feature.getDescription();
    // make sure the description is not empty, this will create a problem when parsing the
    // observation later using StringUtilities.splitString()
    if( StringUtils.isBlank( featureDescription ) )
      return feature.getName();

    return featureDescription;
  }

  static String getNodeTitle( final Feature feature )
  {
    final String featureName = feature.getName();
    if( StringUtils.isBlank( featureName ) )
      return feature.getId();

    return featureName;
  }

  private IAxis[] createAxes( )
  {
    final List<IAxis> resultAxisList = new ArrayList<>();
    resultAxisList.add( new DefaultAxis( AXIS_NODE_ID, ITimeseriesConstants.TYPE_NODEID, "", String.class, true ) ); //$NON-NLS-1$
    resultAxisList.add( new DefaultAxis( AXIS_STATION, ITimeseriesConstants.TYPE_PEGEL, "", String.class, false ) ); //$NON-NLS-1$
    resultAxisList.add( new DefaultAxis( AXIS_DATE, ITimeseriesConstants.TYPE_DATE, "", Date.class, false ) ); //$NON-NLS-1$
    resultAxisList.add( new DefaultAxis( AXIS_DISCHARGE, ITimeseriesConstants.TYPE_RUNOFF, TimeseriesUtils.getUnit( ITimeseriesConstants.TYPE_RUNOFF ), Double.class, false ) ); //$NON-NLS-1$
    resultAxisList.add( new DefaultAxis( AXIS_PATH, ITimeseriesConstants.TYPE_DESCRIPTION, "", String.class, false ) ); //$NON-NLS-1$
    resultAxisList.add( new DefaultAxis( AXIS_VOLUME, ITimeseriesConstants.TYPE_VOLUME, TimeseriesUtils.getUnit( ITimeseriesConstants.TYPE_VOLUME ), Double.class, false ) ); //$NON-NLS-1$
    return resultAxisList.toArray( new IAxis[resultAxisList.size()] );
  }

  private void writeZml( final IObservation observation, final File reportDir ) throws SensorException
  {
    final File reportFileZML = new File( reportDir, FILENAME_ZML );
    ZmlFactory.writeToFile( observation, reportFileZML );
  }

  private void writeCsv( final IObservation observation, final File reportDir ) throws IOException, SensorException
  {
    final File reportFileCSV = new File( reportDir, FILENAME_CSV );

    final ITupleModel values = observation.getValues( null );
    final IAxis[] resultAxisList = observation.getAxes();

    final TimeZone timeZone = KalypsoCorePlugin.getDefault().getTimeZone();
    final String timezoneName = timeZone.getDisplayName();

    try (PrintWriter pw = new PrintWriter( reportFileCSV ))
    {
      /* Header */
      pw.print( Messages.getString("NAStatistics.1") ); //$NON-NLS-1$
      pw.print( SEPARATOR_CSV );
      pw.print( Messages.getString("NAStatistics.2") ); //$NON-NLS-1$
      pw.print( SEPARATOR_CSV );
      pw.format( Messages.getString("NAStatistics.3"), timezoneName ); //$NON-NLS-1$
      pw.print( SEPARATOR_CSV );
      pw.print( Messages.getString("NAStatistics.4") ); //$NON-NLS-1$
      pw.print( SEPARATOR_CSV );
      pw.print( Messages.getString("NAStatistics.5") ); //$NON-NLS-1$
      pw.print( SEPARATOR_CSV );
      pw.println( Messages.getString("NAStatistics.6") ); //$NON-NLS-1$

      // REMARK/BUGFIX: using the default charset here, because this file is usually intended to be opened with excel
      // Excel automatically assumes the default charset of the platform
      for( int i = 0; i < values.size(); i++ )
      {
        for( int j = 0; j < 6; j++ )
        {
          final Object currentElement = values.get( i, resultAxisList[j] );

          final String asText = asText( currentElement, j );
          pw.print( asText );

          if( j == 5 )
            pw.format( "%n" ); //$NON-NLS-1$
          else
            pw.print( SEPARATOR_CSV );
        }
      }

      pw.flush();
      pw.close();
    }
  }

  private String asText( final Object currentElement, final int j )
  {
    if( currentElement == null )
      return ""; //$NON-NLS-1$

    switch( j )
    {
      case 0:
      case 1:
      case 4:
        return currentElement.toString();

      case 2:
        return m_csvDateFormat.format( currentElement );

      case 3:
      case 5:
      {
        final Double value = (Double) currentElement;

        // REMARK: using platform default (',' in german) because we want excel to open the file

        // TODO: probably no fixable: if the user changes the decimal separator on his/her system to '.', but leaves the
        // normal settings as german java is not abe to follow... so we will stil get a ',', but excel wil refuse to
        // read the file.

        return String.format( "%.8f", value ); //$NON-NLS-1$
      }
    }

    throw new UnsupportedOperationException();
  }

  public void add( final Feature resultFeature, final NAStatisticsData data )
  {
    m_resultMap.put( resultFeature, data );
  }
}