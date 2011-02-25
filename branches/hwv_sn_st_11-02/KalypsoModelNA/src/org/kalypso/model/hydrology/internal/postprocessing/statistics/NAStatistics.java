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
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.NotImplementedException;
import org.apache.commons.lang.StringUtils;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.model.hydrology.internal.i18n.Messages;
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
public class NAStatistics
{
  /**
   * Comparator that ensures to write the statistical entries in alphabetical order.
   */
  public class NAStatisticComparator implements Comparator<Feature>
  {
    /**
     * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
     */
    @Override
    public int compare( final Feature o1, final Feature o2 )
    {
      final String title1 = getNodeTitle( o1 );
      final String title2 = getNodeTitle( o2 );
      return title1.compareToIgnoreCase( title2 );
    }
  }

  private static final String FILENAME_CSV = "statistics.csv"; //$NON-NLS-1$

  private static final String FILENAME_ZML = "statistics.zml"; //$NON-NLS-1$

  private static final SimpleDateFormat CSV_DATE_FORMAT = new SimpleDateFormat( "dd.MM.yyyy HH:mm:ss Z" ); //$NON-NLS-1$

  private static final String SEPARATOR_CSV = ","; //$NON-NLS-1$

  private final Map<Feature, NAStatisticsData> m_resultMap = new TreeMap<Feature, NAStatisticsData>( new NAStatisticComparator() );

  private final Logger m_logger;

  public NAStatistics( final Logger logger )
  {
    m_logger = logger;
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
      final String msg = String.format( "Failed to create statistics: %s", e.getLocalizedMessage() );
      m_logger.severe( msg );
    }
  }

  private IObservation createObservation( final Object[][] resultValues )
  {
    final IAxis[] resultAxes = createAxes();
    final ITupleModel resultTuppleModel = new SimpleTupleModel( resultAxes, resultValues );
    return new SimpleObservation( FILENAME_ZML, Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.167" ), new MetadataList(), resultTuppleModel );
  }

  private Object[][] gatherData( final File inputDir ) throws SensorException
  {
    final List<Object[]> resultValuesList = new ArrayList<Object[]>();

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
    final String relativePathWithSlashes = relativePath.replace( '\\', '/' );
    if( relativePathWithSlashes.startsWith( "/" ) )
      return relativePathWithSlashes.substring( 1 );

    return relativePathWithSlashes;
  }

  static String getNodeDescription( final Feature feature )
  {
    final String featureDescription = feature.getDescription();
    if( StringUtils.isBlank( featureDescription ) )
      return "";//$NON-NLS-1$

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
    final List<IAxis> resultAxisList = new ArrayList<IAxis>();
    resultAxisList.add( new DefaultAxis( "NODE_ID", ITimeseriesConstants.TYPE_NODEID, "", String.class, true ) ); //$NON-NLS-1$ //$NON-NLS-2$
    resultAxisList.add( new DefaultAxis( "STATION", ITimeseriesConstants.TYPE_PEGEL, "", String.class, false ) ); //$NON-NLS-1$ //$NON-NLS-2$
    resultAxisList.add( new DefaultAxis( "DATE", ITimeseriesConstants.TYPE_DATE, "", Date.class, false ) ); //$NON-NLS-1$ //$NON-NLS-2$
    resultAxisList.add( new DefaultAxis( "DISCHARGE", ITimeseriesConstants.TYPE_RUNOFF, TimeseriesUtils.getUnit( ITimeseriesConstants.TYPE_RUNOFF ), Double.class, false ) ); //$NON-NLS-1$
    resultAxisList.add( new DefaultAxis( "PATH", ITimeseriesConstants.TYPE_DESCRIPTION, "", String.class, false ) ); //$NON-NLS-1$ //$NON-NLS-2$
    resultAxisList.add( new DefaultAxis( "VOLUME", ITimeseriesConstants.TYPE_VOLUME, TimeseriesUtils.getUnit( ITimeseriesConstants.TYPE_VOLUME ), Double.class, false ) ); //$NON-NLS-1$
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

    FileOutputStream streamCSV = null;
    OutputStreamWriter writerCSV = null;
    try
    {
      streamCSV = new FileOutputStream( reportFileCSV );
      writerCSV = new OutputStreamWriter( streamCSV, "UTF-8" ); //$NON-NLS-1$
      for( int i = 0; i < values.size(); i++ )
      {
        for( int j = 0; j < 6; j++ )
        {
          final Object currentElement = values.get( i, resultAxisList[j] );

          final String asText = asText( currentElement, j );
          writerCSV.write( asText );

          if( j == 5 )
            writerCSV.write( "\n" ); //$NON-NLS-1$
          else
            writerCSV.write( SEPARATOR_CSV );
        }
      }
      writerCSV.flush();
    }
    finally
    {
      IOUtils.closeQuietly( writerCSV );
      IOUtils.closeQuietly( streamCSV );
    }
  }

  private String asText( final Object currentElement, final int j )
  {
    if( currentElement == null )
      return "";

    switch( j )
    {
      case 0:
      case 1:
      case 4:
        return currentElement.toString();

      case 2:
        return CSV_DATE_FORMAT.format( currentElement );

      case 3:
      case 5:
      {
        final Double value = getDoubleValueFormatted( currentElement );
        if( value == null )
          return "";//$NON-NLS-1$
        return String.format( Locale.ENGLISH, "%.8f", value ); //$NON-NLS-1$
      }
    }

    throw new NotImplementedException();
  }

  private final Double getDoubleValueFormatted( final Object object )
  {
    if( object == null )
      return null;

    try
    {
      if( object instanceof Double )
        return (Double) object;

      if( object instanceof String )
        return Double.parseDouble( (String) object );
    }
    catch( final Exception e )
    {
      Logger.getAnonymousLogger().log( Level.WARNING, e.getLocalizedMessage() );
    }

    return null;
  }

  public void add( final Feature resultFeature, final NAStatisticsData data )
  {
    m_resultMap.put( resultFeature, data );
  }
}
