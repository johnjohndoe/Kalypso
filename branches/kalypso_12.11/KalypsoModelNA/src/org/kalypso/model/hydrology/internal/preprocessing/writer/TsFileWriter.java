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
package org.kalypso.model.hydrology.internal.preprocessing.writer;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.NAOptimize;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.channels.Channel;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.preprocessing.NAPreprocessorException;
import org.kalypso.model.hydrology.internal.preprocessing.preparation.NetElement;
import org.kalypso.model.hydrology.internal.preprocessing.preparation.TimeseriesFileManager;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Gernot Belger
 */
class TsFileWriter
{
  private final URL m_zmlContext;

  private final NetElement[] m_channels;

  private final TimeseriesFileManager m_tsFileManager;

  private final GMLWorkspace m_synthNWorkspace;

  private final NAControl m_metaControl;

  private final NAOptimize m_naOptimize;

  public static enum TSFormat
  {
    GRAP,
    EX2
  }

  public TsFileWriter( final GMLWorkspace synthNWorkspace, final NAControl naControl, final NAOptimize naOptimize, final NetElement[] channels, final URL zmlContext, final TimeseriesFileManager tsFileManager )
  {
    m_synthNWorkspace = synthNWorkspace;
    m_metaControl = naControl;
    m_naOptimize = naOptimize;
    m_channels = channels;
    m_zmlContext = zmlContext;
    m_tsFileManager = tsFileManager;
  }

  public void write( final File targetDir ) throws NAPreprocessorException
  {
    for( final NetElement netElement : m_channels )
    {
      final Channel channel = netElement.getChannel();
      generateTimeSeries( channel, targetDir );
    }
  }

  private void generateTimeSeries( final Channel channel, final File klimaDir ) throws NAPreprocessorException
  {
    final Date simulationStart = m_metaControl.getSimulationStart();
    final Date simulationEnd = m_metaControl.getSimulationEnd();
    final DateRange simulationRange = new DateRange( simulationStart, simulationEnd );

    final Catchment[] catchments = channel.findCatchments();

    for( final Catchment catchment : catchments )
    {
      try
      {
        writeCatchmentTimeseries( klimaDir, simulationRange, catchment );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        final String message = String.format( Messages.getString( "TsFileWriter.0" ), catchment.getName() ); //$NON-NLS-1$
        throw new NAPreprocessorException( message, e );
      }
    }
  }

  private void writeCatchmentTimeseries( final File klimaDir, final DateRange simulationRange, final Catchment catchment ) throws SensorException, IOException, NAPreprocessorException
  {
    final File targetFileN = m_tsFileManager.getNiederschlagEingabeDatei( catchment, klimaDir ); //$NON-NLS-1$

    if( m_metaControl.isUsePrecipitationForm() )
    {
      if( !targetFileN.exists() )
        writeSynthNFile( targetFileN, catchment );
    }
    else
    {
      if( !targetFileN.exists() )
        writeNFile( targetFileN, catchment );

      final ZmlLink linkT = catchment.getTemperatureLink();
      final File targetFileT = m_tsFileManager.getTemperaturEingabeDatei( catchment, klimaDir ); //$NON-NLS-1$
      writeExtTimeseries( targetFileT, linkT, m_zmlContext, ITimeseriesConstants.TYPE_MEAN_TEMPERATURE, "1.0", simulationRange ); //$NON-NLS-1$

      final ZmlLink linkV = catchment.getEvaporationLink();
      final File targetFileV = m_tsFileManager.getVerdunstungEingabeDatei( catchment, klimaDir ); //$NON-NLS-1$
      writeExtTimeseries( targetFileV, linkV, m_zmlContext, ITimeseriesConstants.TYPE_EVAPORATION_LAND_BASED, "0.5", simulationRange ); //$NON-NLS-1$
    }
  }

  private void writeNFile( final File targetFileN, final Catchment catchment ) throws SensorException, IOException
  {
    final ZmlLink linkN = catchment.getPrecipitationLink();

    final IObservation observation = loadObservationWithFilter( linkN, m_zmlContext, null );
    if( observation == null )
      return;

    final StringBuilder writer = new StringBuilder();

    final GrapWriter grapWriter = new GrapWriter( ITimeseriesConstants.TYPE_RAINFALL, observation );

    final RangeFactor forecastFactor = createForecastFactor( catchment );
    grapWriter.setRangeFactor( forecastFactor );

    grapWriter.write( writer );

    FileUtils.writeStringToFile( targetFileN, writer.toString() );
  }

  /**
   * Very specialized: we have a special factor for precipitation in the forecast date range.<br/>
   * This makes actually only sense for highwater prediction.
   */
  private RangeFactor createForecastFactor( final Catchment catchment )
  {
    // FIXME: cannot work any more

    final Date startForecast = m_metaControl.getSimulationEnd();
    if( startForecast == null )
      return null;

    final Date simulationEnd = m_metaControl.getSimulationEnd();
    if( startForecast.equals( simulationEnd ) )
      return null;

    if( m_naOptimize == null )
      return null;

    final IFeatureBindingCollection<Catchment> catchmentCollection = m_naOptimize.getCatchmentCollection();
    if( !catchmentCollection.contains( catchment ) )
      return null;

    final double faktn = catchment.getFaktn();

    final double faktnPrognose = m_naOptimize.getFaktnPrognose();
    // THE BIG HACK: We always devide by the catchment factor, else we would get two factors: the catchment factor
    // (applied by kalypso-na.exe) AND the forecast factor. But we only want one factor.
    final double forcastFactorN = faktnPrognose / faktn;

    final DateRange forecastRange = new DateRange( startForecast, simulationEnd );
    return new RangeFactor( forecastRange, forcastFactorN );
  }

  public static final void writeGrapTimeseries( final File targetFile, final ZmlLink link, final URL zmlContext, final String valueAxisType, final String filter ) throws SensorException, IOException
  {
    if( targetFile.exists() )
      return;

    final IObservation observation = loadObservationWithFilter( link, zmlContext, filter );
    if( observation == null )
      return;

    final GrapWriter grapWriter = new GrapWriter( valueAxisType, observation );
    final StringBuilder writer = new StringBuilder();
    grapWriter.write( writer );
    FileUtils.writeStringToFile( targetFile, writer.toString() );
  }

  public static final void writeExtTimeseries( final File targetFile, final ZmlLink link, final URL zmlContext, final String valueAxisType, final String defaultValue, final DateRange simulationRange ) throws SensorException, IOException
  {
    if( targetFile.exists() )
      return;

    final IObservation observation = loadObservationWithFilter( link, zmlContext, null );
    if( observation == null )
      return;

    // TODO: calculate start/end here
    final Ext2InterpolationWriter writer = new Ext2InterpolationWriter( observation, valueAxisType, defaultValue );
    writer.write( targetFile, simulationRange );
  }

  private static IObservation loadObservationWithFilter( final ZmlLink link, final URL zmlContext, final String filter ) throws MalformedURLException, SensorException
  {
    if( link == null )
      return null;

    final String href = link.getHref();

    final String hrefWithFilter = filter == null ? href : ZmlURL.insertFilter( href, filter );

    final URL location = UrlResolverSingleton.getDefault().resolveURL( zmlContext, hrefWithFilter );

    return ZmlFactory.parseXML( location );
  }

  // FIXME: does not belong here -> move into own writer
  private void writeSynthNFile( final File targetFileN, final Catchment catchment ) throws SensorException, IOException, NAPreprocessorException
  {
    final List<Feature> statNList = new ArrayList<>();

    final StringBuffer buffer = new StringBuffer();
    final Double annualityKey = m_metaControl.getAnnuality();

    // Kostra-Kachel/ synth. N gebietsab‰ngig
    final String synthNKey = catchment.getSynthZR();

    statNList.addAll( Arrays.asList( FeatureHelper.getFeaturesWithName( m_synthNWorkspace, NaModelConstants.SYNTHN_STATN_FT ) ) );

    // Performance & readability: linear search loop; first hash the synth-definitions; then write....

    for( final Feature statNFE : statNList )
    {
      final String statFNEname = statNFE.getName();
      if( statFNEname != null )
      {
        if( statFNEname.equals( synthNKey ) )
        {
          // FIXME: probably one one file should be written, stop when rainfall was found and written

          final List< ? > statNParameterList = (List< ? >)statNFE.getProperty( NaModelConstants.STATNPARA_MEMBER );
          for( final Object object : statNParameterList )
          {
            final Feature fe = (Feature)object;
            final String annuality = Double.toString( 1d / (Double)fe.getProperty( NaModelConstants.STATN_PROP_XJAH ) );
            if( annuality.equals( annualityKey.toString() ) )
            {
              final IObservation tnProp = (IObservation)fe.getProperty( NaModelConstants.STATN_PROP_STATN_DIAG );
              if( tnProp != null )
              {
                final IObservation observation = tnProp;
                final IAxis[] axisList = observation.getAxes();
                final IAxis minutesAxis = ObservationUtilities.findAxisByType( axisList, ITimeseriesConstants.TYPE_MIN );
                final IAxis precipitationAxis = ObservationUtilities.findAxisByType( axisList, ITimeseriesConstants.TYPE_RAINFALL );
                buffer.append( FortranFormatHelper.printf( annualityKey, "f6.3" ) + " " + "1" + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
                final ITupleModel values = observation.getValues( null );
                final int count = values.size();
                // if( count > 20 )
                // throw new Exception( "Fehler!!! NA-Modell: Anzahl Wertepaare synth Niederschlag > maximale Anzahl
                // (20) \n Niederschlag:" + synthNKey + "\n Wiederkehrwahrscheinlichkeit: "
                // + annualityKey );
                for( int row = 0; row < count; row++ )
                {
                  final Double minutesValue = (Double)values.get( row, minutesAxis );
                  final Double hoursValue = minutesValue / 60d;
                  if( hoursValue.equals( m_metaControl.getDurationHours() ) )
                  {
                    final Double precipitationValue = (Double)values.get( row, precipitationAxis );
                    buffer.append( FortranFormatHelper.printf( hoursValue, "f9.3" ) + " " + FortranFormatHelper.printf( precipitationValue, "*" ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
                  }
                }

                // FIXME: highly dubious: the same buffer is written again and again, but never cleared during the
                // loop... is this correct?

                final FileWriter writer = new FileWriter( targetFileN );
                writer.write( buffer.toString() );
                IOUtils.closeQuietly( writer );
              }
              else
              {
                final String msg = String.format( Messages.getString("TsFileWriter.1"), catchment.getName(), annualityKey ); //$NON-NLS-1$
                // TODO: CHECK: simulation continued at this point before, but probably simulation should stop now.
                throw new NAPreprocessorException( msg );
              }
            }
          }
        }
      }
    }
  }
}
