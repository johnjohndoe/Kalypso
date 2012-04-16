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
package org.kalypso.ui.rrm.internal.calccase;

import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLEncoder;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TimeZone;

import javax.xml.namespace.QName;

import org.apache.commons.lang3.ObjectUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.joda.time.Duration;
import org.joda.time.Interval;
import org.joda.time.LocalTime;
import org.joda.time.Period;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.java.math.IntervalUtilities;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.model.rcm.binding.ICatchment;
import org.kalypso.model.rcm.binding.ILinearSumGenerator;
import org.kalypso.model.rcm.binding.IMultiGenerator;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.model.rcm.util.RainfallGeneratorUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.TIMESERIES_TYPE;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.timeseries.AxisUtils;
import org.kalypso.ogc.sensor.timeseries.TimeseriesUtils;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.IXLinkedFeature;

import com.google.common.base.Charsets;

/**
 * This class contains functions for dealing with catchment models.
 * 
 * @author Holger Albert
 */
public class CatchmentModelHelper
{
  /**
   * The constructor.
   */
  private CatchmentModelHelper( )
  {
  }

  /**
   * This function bulds a link for the timeseries of the given catchment.
   * 
   * @param prefix
   *          This prefix will be used, if set. May be null.
   * @param parameterType
   *          The parameter type of the timeseries.
   * @param catchment
   *          The catchment, the timeseries is for.
   */
  public static String buildLink( final String prefix, final String parameterType, final Catchment catchment ) throws UnsupportedEncodingException
  {
    final String folderName = getTargetLinkFolderName( parameterType );

    if( prefix == null || prefix.length() == 0 )
      return String.format( "../%s/%s_%s.zml", folderName, parameterType, URLEncoder.encode( catchment.getName(), Charsets.UTF_8.name() ) ); //$NON-NLS-1$

    return String.format( "../%s/%s_%s_%s.zml", folderName, prefix, parameterType, URLEncoder.encode( catchment.getName(), Charsets.UTF_8.name() ) ); //$NON-NLS-1$
  }

  // FIXME: use CalcCaseAccessor for that?
  private static String getTargetLinkFolderName( final String parameterType )
  {
    switch( parameterType )
    {
      case ITimeseriesConstants.TYPE_RAINFALL:
        return Messages.getString( "UpdateSimulationWorker.3" ); // $NON-NLS-1$
      case ITimeseriesConstants.TYPE_MEAN_TEMPERATURE:
      case ITimeseriesConstants.TYPE_EVAPORATION_LAND_BASED:
        return Messages.getString( "UpdateSimulationWorker.4" ); // $NON-NLS-1$
    }

    throw new IllegalArgumentException();
  }

  /**
   * This function sets a link to the catchment.
   * 
   * @param catchment
   *          This catchment will get the link set.
   * @param targetLink
   *          The qname of the property for setting the link.
   * @param link
   *          The link to set.
   */
  public static void setLink( final Catchment catchment, final QName targetLink, final String link )
  {
    /* Create the timeseries link type. */
    final TimeseriesLinkType tsLink = new TimeseriesLinkType();
    tsLink.setHref( link );

    /* Set the property. */
    catchment.setProperty( targetLink, tsLink );
  }

  /**
   * This function checks, if the sub generators contained in the multi generator apply to special rules.<br/>
   * <br/>
   * It will check the following rules:
   * <ul>
   * <li>All generators must be of the type ILinearSumGenerator.</li>
   * <li>The timestep must be the same in all generators.</li>
   * <li>The timestamp must be the same in all generators.</li>
   * <li>The areas must be the same and must have the same order in all generators.</li>
   * <li>Generators may not overlap. Touch is ok.</li>
   * <li>There are no gaps allowed between the validity ranges of adjacent generators.</li>
   * </ul>
   * 
   * @param multiGenerator
   *          The multi generator.
   * @param control
   *          The na control.
   * @return A status. If the severity is ERROR, the validation has failed.
   */
  public static IStatus validateMultiGenerator( final IMultiGenerator multiGenerator, final NAControl control )
  {
    /* The status collector. */
    final StatusCollector collector = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    /* Get the generators. */
    final IFeatureBindingCollection<IRainfallGenerator> generators = multiGenerator.getSubGenerators();
    if( generators.size() == 0 )
    {
      collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), String.format( "The multi generator '%s' does not have any generators.", multiGenerator.getDescription() ) ) );
      return collector.asMultiStatus( String.format( "Validation of the multi generator '%s'", multiGenerator.getDescription() ) );
    }

    /* The values of the first generator will be the reference for the others. */
    final ILinearSumGenerator firstGenerator = (ILinearSumGenerator) generators.get( 0 );

    /* Check each generator. */
    for( final IRainfallGenerator generator : generators )
    {
      /* Perfomance: Do not check the first generator with itself. */
      if( generator.getId().equals( firstGenerator.getId() ) )
        continue;

      /* (1) All generators must be of the type ILinearSumGenerator. */
      if( !(generator instanceof ILinearSumGenerator) )
      {
        collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), String.format( "The generator '%s' is not of the type ILinearSumGenerator.", generator.getDescription() ) ) );
        continue;
      }

      /* Cast. */
      final ILinearSumGenerator linearSumGenerator = (ILinearSumGenerator) generator;

      /* (2) The timestep must be the same in all generators. */
      final Integer firstTimestep = firstGenerator.getTimestep();
      final Integer timestep = linearSumGenerator.getTimestep();
      if( !ObjectUtils.equals( firstTimestep, timestep ) )
      {
        collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), String.format( "The timestep of the generator '%s' does not match the timestep of the first generator '%s'.", generator.getDescription(), firstGenerator.getDescription() ) ) );
        continue;
      }

      /* (3) The timestamp must be the same in all generators. */
      final LocalTime firstTimestamp = firstGenerator.getTimestamp();
      final LocalTime timestamp = linearSumGenerator.getTimestamp();
      if( !ObjectUtils.equals( firstTimestamp, timestamp ) )
      {
        collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), String.format( "The timestamp of the generator '%s' does not match the timestamp of the first generator '%s'.", generator.getDescription(), firstGenerator.getDescription() ) ) );
        continue;
      }

      /* (4) The areas must be the same and must have the same order in all generators. */
      if( !compareGeneratorCatchments( firstGenerator, linearSumGenerator, false ) )
      {
        collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), String.format( "The catchments of the generator '%s' does not match the catchments of the first generator '%s'.", generator.getDescription(), firstGenerator.getDescription() ) ) );
        continue;
      }

      /* HINT: If there is only one generator, we do not reach the code here. */
      /* HINT: If we do reach here, it will be the 2nd loop or one after. */

      /* (5) Generators may not overlap. Touch is ok. */
      if( !compareGeneratorValidityOverlap( generator, generators ) )
      {
        collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), String.format( "The validity range of generator '%s' overlaps the validity range of one other generator.", generator.getDescription() ) ) );
        continue;
      }
    }

    /* (6) There are no gaps allowed between the validity ranges of adjacent generators. */
    if( !compareGeneratorValidityGaps( generators, control, firstGenerator.getTimestep(), firstGenerator.getTimestamp() ) )
      collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), String.format( "There are gaps in the validity ranges of the generators of the multi generator '%s'", multiGenerator.getDescription() ) ) );

    return collector.asMultiStatus( String.format( "Validation of the multi generator '%s'", multiGenerator.getDescription() ) );
  }

  /**
   * This function compares the catchments of two linear sum generators.<br/>
   * <br/>
   * It will check the following:
   * <ul>
   * <li>The number of catchments.</li>
   * <li>The order of the catchments.</li>
   * <li>Optional: The factors and timeseries in the catchments.</li>
   * </ul>
   * 
   * @param generator1
   *          The first linear sum generator.
   * @param generator2
   *          The second linear sum generator.
   * @param includeTimeseries
   *          If true, the factors and timeseries in the catchments are compared, too.
   * @return True, if the catchments of the linear sum generators are equal. False otherwise.
   */
  public static boolean compareGeneratorCatchments( final ILinearSumGenerator generator1, final ILinearSumGenerator generator2, final boolean includeTimeseries )
  {
    /* Get the catchments. */
    final IFeatureBindingCollection<ICatchment> catchments1 = generator1.getCatchments();
    final IFeatureBindingCollection<ICatchment> catchments2 = generator2.getCatchments();
    if( catchments1.size() != catchments2.size() )
      return false;

    /* Compare the catchments. */
    for( int i = 0; i < catchments1.size(); i++ )
    {
      /* Get the catchments. */
      final ICatchment catchment1 = catchments1.get( i );
      final ICatchment catchment2 = catchments2.get( i );

      /* If the linked areas do not match, this are completely different lists or not in the same order. */
      final String areaHref1 = ((IXLinkedFeature) catchment1.getProperty( ICatchment.PROPERTY_AREA_LINK )).getHref();
      final String areaHref2 = ((IXLinkedFeature) catchment2.getProperty( ICatchment.PROPERTY_AREA_LINK )).getHref();
      if( !areaHref1.equals( areaHref2 ) )
        return false;

      /* If true, the factors and timeseries in the catchments are compared, too. */
      if( includeTimeseries )
      {
        /* Build the hash. */
        final String hash1 = RainfallGeneratorUtilities.buildHash( catchment1 );
        final String hash2 = RainfallGeneratorUtilities.buildHash( catchment2 );
        if( !hash1.equals( hash2 ) )
          return false;
      }
    }

    return true;
  }

  /**
   * This function compares the validity ranges of the generators.
   * 
   * @param compareGenerator
   *          The compare generator.
   * @param generators
   *          All generators the compare generator will be compared against. If the compare generator is contained, it
   *          will be ignored.
   * @return <ul>
   *         <li>True: The validity range of the compare generator does not overlap the validity ranges of the other
   *         generators.</li>
   *         <li>False: The validity range of the compare generator overlaps one validity range of the other generators.
   *         </li>
   *         </ul>
   */
  private static boolean compareGeneratorValidityOverlap( final IRainfallGenerator compareGenerator, final IFeatureBindingCollection<IRainfallGenerator> generators )
  {
    /* No generators available, to compare to. */
    if( generators.size() == 0 )
      return true;

    /* The interval of the compare generator. */
    final Interval compareInterval = new Interval( new DateTime( compareGenerator.getValidFrom() ), new DateTime( compareGenerator.getValidTo() ) );

    /* Check if the interval overlaps one of the other intervals. */
    for( final IRainfallGenerator generator : generators )
    {
      /* Do not compare the compare generator with itself. */
      if( compareGenerator.getId().equals( generator.getId() ) )
        continue;

      /* The interval of the generator. */
      final Interval interval = new Interval( new DateTime( generator.getValidFrom() ), new DateTime( generator.getValidTo() ) );
      if( compareInterval.overlaps( interval ) )
      {
        final Interval overlapInterval = compareInterval.overlap( interval );
        final Duration overlapDuration = overlapInterval.toDuration();
        final long standardMinutes = overlapDuration.getStandardMinutes();
        if( standardMinutes > 0 )
          return false;
      }
    }

    return true;
  }

  /**
   * This function checks the validity ranges of the generators for gaps.
   * 
   * @param generators
   *          The generators to be checked.
   * @param control
   *          The na control.
   * @param timestep
   *          The timestep.
   * @param timestamp
   *          The timestamp.
   * @return True, if the validity ranges of the generators do not have gaps or only timestep sized gaps. False
   *         otherwise.
   */
  private static boolean compareGeneratorValidityGaps( final IFeatureBindingCollection<IRainfallGenerator> generators, final NAControl control, final Integer timestep, final LocalTime timestamp )
  {
    /* No generators available. */
    /* Only one generator available. */
    if( generators.size() <= 1 )
      return true;

    /* The generators. */
    final IRainfallGenerator[] generatorArray = generators.toArray( new IRainfallGenerator[] {} );

    /* Build the simulation interval. */
    final DateTime simulationStart = new DateTime( control.getSimulationStart() );
    final DateTime simulationEnd = new DateTime( control.getSimulationEnd() );
    final DateRange simulationRange = modifyWithTimestamp( timestamp, simulationStart, simulationEnd );
    final Date simulationStartDate = simulationRange.getFrom();
    final Date simulationEndDate = simulationRange.getTo();
    final long simulationStartTime = simulationStartDate.getTime();
    final long simulationEndTime = simulationEndDate.getTime();
    final org.kalypso.contribs.java.math.Interval simulationInterval = new org.kalypso.contribs.java.math.Interval( simulationStartTime, simulationEndTime );
    org.kalypso.contribs.java.math.Interval[] simulationRest = new org.kalypso.contribs.java.math.Interval[] { simulationInterval };

    /* Check each generator. */
    for( final IRainfallGenerator generator : generatorArray )
    {
      /* Get the generator dates. */
      DateTime generatorStartDateTime = new DateTime( generator.getValidFrom() );
      final DateTime generatorEndDateTime = new DateTime( generator.getValidTo() );

      /* Adjust the check for sum values. */
      final String parameterType = generator.getParameterType();
      final TIMESERIES_TYPE type = TimeseriesUtils.getType( parameterType );
      if( type.equals( TIMESERIES_TYPE.eSumValue ) )
        generatorStartDateTime = generatorStartDateTime.plusDays( 1 );

      /* Build the generator interval. */
      final Date generatorStartDate = generatorStartDateTime.toDate();
      final Date generatorEndDate = generatorEndDateTime.toDate();
      final long generatorStartTime = generatorStartDate.getTime();
      final long generatorEndTime = generatorEndDate.getTime();
      final org.kalypso.contribs.java.math.Interval generatorInterval = new org.kalypso.contribs.java.math.Interval( generatorStartTime, generatorEndTime );

      /* Substract the generator interval from all rest intervals. */
      simulationRest = IntervalUtilities.difference( simulationRest, generatorInterval );
    }

    /* No gaps. */
    if( simulationRest.length == 0 )
      return true;

    for( final org.kalypso.contribs.java.math.Interval restInterval : simulationRest )
    {
      /* The gaps are only allowed to be of the size of the timestep. */
      /* HINT: The timestep is in minutes -> convert to milliseconds. */
      if( restInterval.getWidth() > timestep.intValue() * 60 * 1000 )
        return false;
    }

    return true;
  }

  private static DateRange modifyWithTimestamp( final LocalTime timestamp, final DateTime simulationStart, final DateTime simulationEnd )
  {
    /* Nothing to do. */
    if( timestamp == null )
      return new DateRange( simulationStart.toDate(), simulationEnd.toDate() );

    /* Convert to a date with the kalypso timezone. */
    /* The date fields are ignored. */
    final DateTime timestampUTC = timestamp.toDateTimeToday( DateTimeZone.forTimeZone( TimeZone.getTimeZone( "UTC" ) ) );
    final DateTime timestampDate = new DateTime( timestampUTC.toDate(), DateTimeZone.forTimeZone( KalypsoCorePlugin.getDefault().getTimeZone() ) );

    /* Further adjust range by predefined time. */
    final DateTime startWithTime = simulationStart.withTime( timestampDate.getHourOfDay(), timestampDate.getMinuteOfHour(), timestampDate.getSecondOfMinute(), timestampDate.getMillisOfSecond() );
    final DateTime endWithTime = simulationEnd.withTime( timestampDate.getHourOfDay(), timestampDate.getMinuteOfHour(), timestampDate.getSecondOfMinute(), timestampDate.getMillisOfSecond() );

    return new DateRange( startWithTime.toDate(), endWithTime.toDate() );
  }

  /**
   * This function calculates the range for the timeseries to be generated. The range equals the range defined in the
   * simulation adjusted as follows:
   * <ul>
   * <li>1 timestep earlier</li>
   * <li>3 timesteps later</li>
   * </ul>
   * 
   * @param control
   *          The na control.
   * @param timestep
   *          The timestep.
   * @param timestamp
   *          The timestamp in UTC.
   * @return The date range.
   */
  public static DateRange getRange( final NAControl control, final Period timestep, final LocalTime timestamp )
  {
    final Date simulationStart = control.getSimulationStart();
    final Date simulationEnd = control.getSimulationEnd();

    final DateTime start = new DateTime( simulationStart );
    final DateTime end = new DateTime( simulationEnd );

    final DateTime adjustedStart = start.minus( timestep );
    final DateTime adjustedEnd = end.plus( timestep ).plus( timestep ).plus( timestep );

    if( timestep.getDays() == 0 || timestamp == null )
      return new DateRange( adjustedStart.toDate(), adjustedEnd.toDate() );

    /* Convert to a date with the kalypso timezone. */
    /* The date fields are ignored. */
    final DateTime timestampUTC = timestamp.toDateTimeToday( DateTimeZone.forTimeZone( TimeZone.getTimeZone( "UTC" ) ) );
    final DateTime timestampDate = new DateTime( timestampUTC.toDate(), DateTimeZone.forTimeZone( KalypsoCorePlugin.getDefault().getTimeZone() ) );

    /* Further adjust range by predefined time. */
    final DateTime startWithTime = adjustedStart.withTime( timestampDate.getHourOfDay(), timestampDate.getMinuteOfHour(), timestampDate.getSecondOfMinute(), timestampDate.getMillisOfSecond() );
    final DateTime endWithTime = adjustedEnd.withTime( timestampDate.getHourOfDay(), timestampDate.getMinuteOfHour(), timestampDate.getSecondOfMinute(), timestampDate.getMillisOfSecond() );

    /* New start must always be before unadjusted start, fix, if this is not the case. */
    DateTime startWithTimeFixed;
    if( startWithTime.isAfter( adjustedStart ) )
      startWithTimeFixed = startWithTime.minus( timestep );
    else
      startWithTimeFixed = startWithTime;

    /* New end must always be after unadjusted end, fix, if this is not the case. */
    DateTime endWithTimeFixed;
    if( endWithTime.isBefore( adjustedEnd ) )
      endWithTimeFixed = endWithTime.plus( timestep );
    else
      endWithTimeFixed = endWithTime;

    return new DateRange( startWithTimeFixed.toDate(), endWithTimeFixed.toDate() );

  }

  public static NAControl loadControl( final RrmSimulation simulation ) throws Exception
  {
    final IFile calculationGml = simulation.getCalculationGml();
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( calculationGml );
    return (NAControl) workspace.getRootFeature();
  }

  /**
   * This function compares the timeseries of two simulations.<br>
   * <br/>
   * Details:
   * <ul>
   * <li>It will use the second simulation as reference.</li>
   * <li>All dates/values in the second simulation must exist and be equal (to a degree) in the first simulation.</li>
   * <li>If a date/value of the second simulation misses or is not equal in the first simulation, this is a difference.</li>
   * <li>It will not compare the length or metadata of the timeseries.</li>
   * </ul>
   * 
   * @param simulationFolder
   *          The folder of the first simulation.
   * @param simulationTmpFolder
   *          The folder of the second simulation.
   * @return A WARNING status, if there are differences. A OK status otherwise.
   */
  public static IStatus compareTimeseries( final IFolder simulationFolder, final IFolder simulationTmpFolder ) throws Exception
  {
    /* The catchments. */
    IFeatureBindingCollection<Catchment> catchments = null;
    IFeatureBindingCollection<Catchment> tmpCatchments = null;

    try
    {
      /* The status collector. */
      final StatusCollector collector = new StatusCollector( KalypsoUIRRMPlugin.getID() );

      /* Load the model. */
      final NaModell model = loadModel( new RrmSimulation( simulationFolder ) );
      catchments = model.getCatchments();

      /* Load the temporary model. */
      final NaModell tmpModel = loadModel( new RrmSimulation( simulationTmpFolder ) );
      tmpCatchments = tmpModel.getCatchments();

      /* Compare the catchments. */
      /* Both lists must be in the same order, because the temporary one is a copy. */
      for( int i = 0; i < catchments.size(); i++ )
      {
        final Catchment catchment = catchments.get( i );
        final Catchment tmpCatchment = tmpCatchments.get( i );

        final IStatus status = compareCatchments( catchment, tmpCatchment );
        collector.add( status );
      }

      return collector.asMultiStatus( "Verifying of the timeseries finished." );
    }
    finally
    {
      /* Dispose the catchments. */
      if( catchments != null )
        catchments.getParentFeature().getWorkspace().dispose();

      /* Dispose the temporary catchments. */
      if( tmpCatchments != null )
        tmpCatchments.getParentFeature().getWorkspace().dispose();
    }
  }

  /**
   * This function loads the na model.
   * 
   * @param simulation
   *          The simulation.
   * @return The na model.
   */
  public static NaModell loadModel( final RrmSimulation simulation ) throws Exception
  {
    final IFile modelGml = simulation.getModelGml();
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( modelGml );
    return (NaModell) workspace.getRootFeature();
  }

  private static IStatus compareCatchments( final Catchment catchment, final Catchment tmpCatchment ) throws MalformedURLException, SensorException
  {
    /* The status collector. */
    final StatusCollector collector = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    /* Get the timeseries links of the catchment. */
    final URL context = catchment.getWorkspace().getContext();
    final TimeseriesLinkType precipitationLink = catchment.getPrecipitationLink();
    final TimeseriesLinkType evaporationLink = catchment.getEvaporationLink();
    final TimeseriesLinkType temperatureLink = catchment.getTemperatureLink();

    /* Get the timeseries links of the temporary catchment. */
    final URL tmpContext = tmpCatchment.getWorkspace().getContext();
    final TimeseriesLinkType tmpPrecipitationLink = tmpCatchment.getPrecipitationLink();
    final TimeseriesLinkType tmpEvaporationLink = tmpCatchment.getEvaporationLink();
    final TimeseriesLinkType tmpTemperatureLink = tmpCatchment.getTemperatureLink();

    /* Compare the precipitation timeseries. */
    final IStatus precipitationStatus = compareTimeseries( context, precipitationLink, tmpContext, tmpPrecipitationLink, "Precipitation" );
    collector.add( precipitationStatus );

    /* Compare the evaporation timeseries. */
    final IStatus evaporationStatus = compareTimeseries( context, evaporationLink, tmpContext, tmpEvaporationLink, "Evaporation" );
    collector.add( evaporationStatus );

    /* Compare the temperature timeseries. */
    final IStatus temperatureStatus = compareTimeseries( context, temperatureLink, tmpContext, tmpTemperatureLink, "Temperature" );
    collector.add( temperatureStatus );

    return collector.asMultiStatus( String.format( "Verifying the imported timeseries of the catchment '%s'.", catchment.getDescription() ) );
  }

  private static IStatus compareTimeseries( final URL context, final TimeseriesLinkType link, final URL tmpContext, final TimeseriesLinkType tmpLink, final String timeseriesType ) throws MalformedURLException, SensorException
  {
    /* The status collector. */
    final StatusCollector collector = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    /* Load the timeseries. */
    /* The time zone may be different to that of the newly generated timeseries. */
    final URL location = UrlResolverSingleton.resolveUrl( context, link.getHref() );
    final IObservation observation = ZmlFactory.parseXML( location );

    /* Load the temporary timeseries. */
    /* The time zone may be different to that of the original timeseries. */
    final URL tmpLocation = UrlResolverSingleton.resolveUrl( tmpContext, tmpLink.getHref() );
    final IObservation tmpObservation = ZmlFactory.parseXML( tmpLocation );

    /* Get the values of both timeseries. */
    final ITupleModel values = observation.getValues( null );
    final ITupleModel tmpValues = tmpObservation.getValues( null );

    /* Build a hash date->value for the old timeseries. */
    final Map<Long, Double> hash = buildHash( values );

    /* Build a hash date->value for the new timeseries. */
    final Map<Long, Double> tmpHash = buildHash( tmpValues );

    /* Loop through the new hash. */
    int differences = 0;
    for( final Entry<Long, Double> tmpEntry : tmpHash.entrySet() )
    {
      /* Get the key and the value of the new timeseries. */
      final Long tmpKey = tmpEntry.getKey();
      final Double tmpValue = tmpEntry.getValue();

      /* Get the value of the old timeseries. */
      final Double value = hash.get( tmpKey );

      /* Compare the values of the new timeseries with the ones in the old timeseries. */
      // TODO 0.01 different with other datatypes?
      if( value == null || Math.abs( tmpValue.doubleValue() - value.doubleValue() ) > 0.01 )
        differences++;
    }

    /* Calculate the procentual difference. */
    if( differences > 0 )
    {
      final int percent = (differences * 100) / tmpHash.size();
      collector.add( new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), String.format( "The new timeseries' values differ by %d%% (%d differences).", percent, differences ) ) );
    }

    return collector.asMultiStatus( timeseriesType );
  }

  private static Map<Long, Double> buildHash( final ITupleModel values ) throws SensorException
  {
    /* Memory for the hash. */
    final Map<Long, Double> hash = new LinkedHashMap<Long, Double>();

    /* Find the needed axes. */
    final IAxis[] axes = values.getAxes();
    final IAxis dateAxis = AxisUtils.findDateAxis( axes );
    final IAxis[] valueAxes = AxisUtils.findValueAxes( axes, false );
    final IAxis valueAxis = valueAxes[0];

    /* Store each date->value pair. */
    for( int i = 0; i < values.size(); i++ )
    {
      final Date date = (Date) values.get( i, dateAxis );
      final Double value = (Double) values.get( i, valueAxis );

      hash.put( new Long( date.getTime() ), value );
    }

    return hash;
  }
}