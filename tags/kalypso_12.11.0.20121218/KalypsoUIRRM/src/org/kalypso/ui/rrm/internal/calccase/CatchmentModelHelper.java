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
import java.net.URL;
import java.net.URLEncoder;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TimeZone;

import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;

import org.apache.commons.lang3.ObjectUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.joda.time.Duration;
import org.joda.time.Interval;
import org.joda.time.LocalTime;
import org.joda.time.Period;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.java.math.IntervalUtilities;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.model.hydrology.binding.cm.ICatchment;
import org.kalypso.model.hydrology.binding.cm.ILinearSumGenerator;
import org.kalypso.model.hydrology.binding.cm.IMultiGenerator;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.timeseriesMappings.IMappingElement;
import org.kalypso.model.hydrology.binding.timeseriesMappings.ITimeseriesMapping;
import org.kalypso.model.hydrology.binding.timeseriesMappings.TimeseriesMappingType;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.model.hydrology.util.cm.CatchmentHelper;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
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
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.cm.view.MultiBean;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.IXLinkedFeature;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;

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
   * @param simulation
   *          The rrm simulation.
   * @param prefix
   *          This prefix will be used, if set. May be null.
   * @param parameterType
   *          The parameter type of the timeseries.
   * @param catchment
   *          The catchment, the timeseries is for.
   */
  public static String buildLink( final RrmSimulation simulation, final String prefix, final String folderName, final String parameterType, final Feature modelElement ) throws UnsupportedEncodingException
  {
    String relativeLink = null;
    if( prefix == null || prefix.length() == 0 )
      relativeLink = String.format( "%s/%s_%s.zml", folderName, parameterType, URLEncoder.encode( modelElement.getName(), Charsets.UTF_8.name() ) ); //$NON-NLS-1$
    else
      relativeLink = String.format( "%s/%s_%s_%s.zml", folderName, prefix, parameterType, URLEncoder.encode( modelElement.getName(), Charsets.UTF_8.name() ) ); //$NON-NLS-1$

    final IFolder simulationFolder = simulation.getSimulationFolder();
    final IPath simulationPath = simulationFolder.getFullPath();
    final IPath fullLink = simulationPath.append( relativeLink );

    return UrlResolver.createProjectPath( fullLink );
  }

  public static String getTargetLinkFolderName( final RrmSimulation simulation, final String parameterType )
  {
    switch( parameterType )
    {
      case ITimeseriesConstants.TYPE_RAINFALL:
        return simulation.getPrecipitationFolder().getName();
      case ITimeseriesConstants.TYPE_MEAN_TEMPERATURE:
        return simulation.getClimateFolder().getName();
      case ITimeseriesConstants.TYPE_EVAPORATION_LAND_BASED:
        return simulation.getClimateFolder().getName();
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
    /* Get the generators. */
    final IFeatureBindingCollection<IRainfallGenerator> subGenerators = multiGenerator.getSubGenerators();
    final IRainfallGenerator[] generators = subGenerators.toArray( new IRainfallGenerator[] {} );

    /* Get the simulation start and end dates. */
    final Date simulationStart = control.getSimulationStart();
    final Date simulationEnd = control.getSimulationEnd();

    /* Get the description. */
    final String description = multiGenerator.getDescription();

    return performValidation( generators, simulationStart, simulationEnd, description );
  }

  /**
   * This function checks, if the sub generators contained in the multi bean apply to special rules.<br/>
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
   * @param multiBean
   *          The multi bean.
   * @return A status. If the severity is ERROR, the validation has failed.
   */
  public static IStatus validateMultiBean( final MultiBean multiBean )
  {
    /* Get the generators. */
    final ILinearSumGenerator[] generators = multiBean.getSubGenerators();

    /* Get the simulation start and end dates. */
    /* The validity range of the multi bean will be used. */
    final XMLGregorianCalendar start = (XMLGregorianCalendar) multiBean.getProperty( IMultiGenerator.PROPERTY_VALID_FROM );
    final XMLGregorianCalendar end = (XMLGregorianCalendar) multiBean.getProperty( IMultiGenerator.PROPERTY_VALID_TO );
    final Date simulationStart = DateUtilities.toDate( start );
    final Date simulationEnd = DateUtilities.toDate( end );

    /* Get the description. */
    final String description = (String) multiBean.getProperty( IMultiGenerator.QN_DESCRIPTION );

    return performValidation( generators, simulationStart, simulationEnd, description );
  }

  /**
   * This function checks, if the generators apply to special rules.<br/>
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
   * @param generators
   *          The generators to validate.
   * @param simulationStart
   *          The simulation start date.
   * @param simulationEnd
   *          The simulation end date.
   * @return A status. If the severity is ERROR, the validation has failed.
   */
  private static IStatus performValidation( final IRainfallGenerator[] generators, final Date simulationStart, final Date simulationEnd, final String description )
  {
    /* The status collector. */
    final StatusCollector collector = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    /* No generators available. */
    if( generators.length == 0 )
    {
      collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), String.format( Messages.getString( "CatchmentModelHelper_0" ), description ) ) ); //$NON-NLS-1$
      return collector.asMultiStatus( String.format( Messages.getString( "CatchmentModelHelper_1" ), description ) ); //$NON-NLS-1$
    }

    /* The values of the first generator will be the reference for the others. */
    final ILinearSumGenerator firstGenerator = (ILinearSumGenerator) generators[0];

    /* Check each generator. */
    for( final IRainfallGenerator generator : generators )
    {
      /* Check all generators, if their validity ranges are set. */
      final Date validFrom = generator.getValidFrom();
      final Date validTo = generator.getValidTo();
      if( validFrom == null || validTo == null )
      {
        collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), String.format( Messages.getString("CatchmentModelHelper.0"), generator.getDescription() ) ) ); //$NON-NLS-1$
        continue;
      }

      /* Perfomance: Do not check the first generator with itself. */
      if( generator.getId().equals( firstGenerator.getId() ) )
        continue;

      /* (1) All generators must be of the type ILinearSumGenerator. */
      if( !(generator instanceof ILinearSumGenerator) )
      {
        collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), String.format( Messages.getString( "CatchmentModelHelper_2" ), generator.getDescription() ) ) ); //$NON-NLS-1$
        continue;
      }

      /* Cast. */
      final ILinearSumGenerator linearSumGenerator = (ILinearSumGenerator) generator;

      /* (2) The timestep must be the same in all generators. */
      /* (3) The timestamp must be the same in all generators. */
      final IStatus generalStatus = compareGeneralProperties( firstGenerator, linearSumGenerator );
      if( !generalStatus.isOK() )
      {
        collector.add( generalStatus );
        continue;
      }

      /* (4) The areas must be the same and must have the same order in all generators. */
      if( !compareGeneratorCatchments( firstGenerator, linearSumGenerator, false ) )
      {
        collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), String.format( Messages.getString( "CatchmentModelHelper_3" ), generator.getDescription(), firstGenerator.getDescription() ) ) ); //$NON-NLS-1$
        continue;
      }

      /* HINT: If there is only one generator, we do not reach the code here. */
      /* HINT: If we do reach here, it will be the 2nd loop or one after. */

      /* (5) Generators may not overlap. Touch is ok. */
      if( !compareGeneratorValidityOverlap( linearSumGenerator, generators ) )
      {
        collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), String.format( Messages.getString( "CatchmentModelHelper_4" ), generator.getDescription() ) ) ); //$NON-NLS-1$
        continue;
      }
    }

    /* (6) There are no gaps allowed between the validity ranges of adjacent generators. */
    if( !compareGeneratorValidityGaps( generators, simulationStart, simulationEnd, firstGenerator.getTimestep(), firstGenerator.getTimestamp() ) )
      collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), String.format( Messages.getString( "CatchmentModelHelper_5" ), description ) ) ); //$NON-NLS-1$

    return collector.asMultiStatus( String.format( Messages.getString( "CatchmentModelHelper_6" ), description ) ); //$NON-NLS-1$
  }

  /**
   * This function checks two linear sum generators for its general properties.<br/>
   * <br/>
   * It will check the following rules:
   * <ul>
   * <li>The timestep must be the same.</li>
   * <li>The timestamp must be the same.</li>
   * </ul>
   *
   * @param generator1
   *          The first linear sum generator.
   * @param generator2
   *          The second linear sum generator.
   * @return A status. If the severity is ERROR, the validation has failed.
   */
  private static IStatus compareGeneralProperties( final ILinearSumGenerator generator1, final ILinearSumGenerator generator2 )
  {
    final Integer timestep1 = generator1.getTimestep();
    final Integer timestep2 = generator2.getTimestep();
    if( !ObjectUtils.equals( timestep1, timestep2 ) )
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), String.format( Messages.getString( "CatchmentModelHelper_7" ), generator2.getDescription(), generator1.getDescription() ) ); //$NON-NLS-1$

    final LocalTime timestamp1 = generator1.getTimestamp();
    final LocalTime timestamp2 = generator2.getTimestamp();
    if( !ObjectUtils.equals( timestamp1, timestamp2 ) )
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), String.format( Messages.getString( "CatchmentModelHelper_8" ), generator2.getDescription(), generator1.getDescription() ) ); //$NON-NLS-1$

    return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), String.format( Messages.getString( "CatchmentModelHelper_9" ), generator2.getDescription(), generator1.getDescription() ) ); //$NON-NLS-1$
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
        final String hash1 = CatchmentHelper.buildHash( catchment1 );
        final String hash2 = CatchmentHelper.buildHash( catchment2 );
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
  private static boolean compareGeneratorValidityOverlap( final IRainfallGenerator compareGenerator, final IRainfallGenerator[] generators )
  {
    /* No generators available, to compare to. */
    if( generators.length == 0 )
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
   * @param simulationStart
   *          The start of the simulation.
   * @param simulationEnd
   *          The end of the simulation.
   * @param timestep
   *          The timestep.
   * @param timestamp
   *          The timestamp.
   * @return True, if the validity ranges of the generators do not have gaps or only timestep sized gaps. False
   *         otherwise.
   */
  private static boolean compareGeneratorValidityGaps( final IRainfallGenerator[] generators, final Date simulationStart, final Date simulationEnd, final Integer timestep, final LocalTime timestamp )
  {
    /* No generators available. */
    /* Only one generator available. */
    if( generators.length <= 1 )
      return true;

    /* Build the simulation interval. */
    final DateTime simulationStartDateTime = new DateTime( simulationStart );
    final DateTime simulationEndDateTime = new DateTime( simulationEnd );
    final DateRange simulationRange = modifyWithTimestamp( timestamp, simulationStartDateTime, simulationEndDateTime );
    final Date simulationStartDate = simulationRange.getFrom();
    final Date simulationEndDate = simulationRange.getTo();
    final long simulationStartTime = simulationStartDate.getTime();
    final long simulationEndTime = simulationEndDate.getTime();
    final org.kalypso.contribs.java.math.Interval simulationInterval = new org.kalypso.contribs.java.math.Interval( simulationStartTime, simulationEndTime );
    org.kalypso.contribs.java.math.Interval[] simulationRest = new org.kalypso.contribs.java.math.Interval[] { simulationInterval };

    /* Check each generator. */
    for( final IRainfallGenerator generator : generators )
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
    final DateTime timestampUTC = timestamp.toDateTimeToday( DateTimeZone.forTimeZone( TimeZone.getTimeZone( "UTC" ) ) ); //$NON-NLS-1$
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
    final DateTime timestampUTC = timestamp.toDateTimeToday( DateTimeZone.forTimeZone( TimeZone.getTimeZone( "UTC" ) ) ); //$NON-NLS-1$
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
    /* The models. */
    NaModell model = null;
    NaModell tmpModel = null;

    try
    {
      /* The status collector. */
      final IStatusCollector collector = new StatusCollector( KalypsoUIRRMPlugin.getID() );

      /* Load the models. */
      model = loadModel( new RrmSimulation( simulationFolder ) );
      tmpModel = loadModel( new RrmSimulation( simulationTmpFolder ) );

      /* Get the contexts. */
      final URL context = model.getWorkspace().getContext();
      final URL tmpContext = tmpModel.getWorkspace().getContext();

      /* Compare the catchments. */
      final QName[] linkProperties = new QName[] { Catchment.PROP_PRECIPITATION_LINK, Catchment.PROP_TEMPERATURE_LINK, Catchment.PROP_EVAPORATION_LINK };
      final String[] linkLabels = new String[] { Messages.getString( "CatchmentModelHelper_12" ), Messages.getString( "CatchmentModelHelper_13" ), Messages.getString( "CatchmentModelHelper_14" ) }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      for( int i = 0; i < linkLabels.length; i++ )
      {
        final IStatus status = compareCatchments( model, tmpModel, context, tmpContext, linkProperties[i], linkLabels[i] );
        if( status != null )
          collector.add( status );
      }

      /* Compare other timeseries (created by timeseries mappings). */
      for( final TimeseriesMappingType mappingType : TimeseriesMappingType.values() )
      {
        final IStatus status = compareMapping( model, tmpModel, context, tmpContext, mappingType );
        if( status != null )
          collector.add( status );
      }

      return collector.asMultiStatus( Messages.getString( "CatchmentModelHelper_15" ) ); //$NON-NLS-1$
    }
    finally
    {
      /* Dispose the model. */
      if( model != null )
        model.getWorkspace().dispose();

      /* Dispose the temporary model. */
      if( tmpModel != null )
        tmpModel.getWorkspace().dispose();
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

  private static IStatus compareCatchments( final NaModell model, final NaModell tmpModel, final URL context, final URL tmpContext, final QName linkProperty, final String linkLabel )
  {
    /* The status collector. */
    final StatusCollector collector = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    /* Both lists must be in the same order, because the temporary one is a copy. */
    final IFeatureBindingCollection<Catchment> catchments = model.getCatchments();
    final IFeatureBindingCollection<Catchment> tmpCatchments = tmpModel.getCatchments();

    for( int i = 0; i < catchments.size(); i++ )
    {
      final Catchment catchment = catchments.get( i );
      final Catchment tmpCatchment = tmpCatchments.get( i );

      final IStatus status = compareCatchment( catchment, tmpCatchment, context, tmpContext, linkProperty );
      if( status != null )
        collector.add( status );
    }

    return collector.asMultiStatus( String.format( Messages.getString( "CatchmentModelHelper_16" ), linkLabel ) ); //$NON-NLS-1$
  }

  private static IStatus compareCatchment( final Catchment catchment, final Catchment tmpCatchment, final URL context, final URL tmpContext, final QName linkProperty )
  {
    /* Get the timeseries links of the catchment. */
    final ZmlLink zmlLink = new ZmlLink( catchment, new GMLXPath( linkProperty ), context );

    /* Get the timeseries links of the temporary catchment. */
    final ZmlLink tmpZmlLink = new ZmlLink( tmpCatchment, new GMLXPath( linkProperty ), tmpContext );

    /* Compare the temperature timeseries. */
    return compareTimeseries( zmlLink, tmpZmlLink, catchment.getName() );
  }

  private static IStatus compareMapping( final NaModell model, final NaModell tmpModel, final URL context, final URL tmpContext, final TimeseriesMappingType mappingType )
  {
    /* The status collector. */
    final IStatusCollector collector = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    /* Both lists must be in the same order, because the temporary one is a copy. */
    final Feature[] modelElements = mappingType.getModelElements( model );
    final Feature[] tmpModelElements = mappingType.getModelElements( tmpModel );

    final QName linkProperty = mappingType.getModelLinkProperty();
    for( int i = 0; i < modelElements.length; i++ )
    {
      final Feature modelElement = modelElements[i];
      final Feature tmpModelElement = tmpModelElements[i];

      final ZmlLink zmlLink = new ZmlLink( modelElement, new GMLXPath( linkProperty ), context );
      final ZmlLink tmpZmlLink = new ZmlLink( tmpModelElement, new GMLXPath( linkProperty ), tmpContext );

      final IStatus status = compareTimeseries( zmlLink, tmpZmlLink, modelElement.getName() );
      if( status != null )
        collector.add( status );
    }

    return collector.asMultiStatus( String.format( Messages.getString( "CatchmentModelHelper_17" ), mappingType.getLabel() ) ); //$NON-NLS-1$
  }

  private static IStatus compareTimeseries( final ZmlLink link, final ZmlLink tmpLink, final String statusLabel )
  {
    /* No links? */
    if( link == null && tmpLink == null )
      return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), statusLabel );

    /* The status collector. */
    final StatusCollector collector = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    try
    {
      /* If there is no link in the new model, no status. */
      if( !(tmpLink.isLinkSet() && tmpLink.isLinkExisting()) )
        return null;

      /* If there is no link in the old model, create a warning. */
      if( !(link.isLinkSet() && link.isLinkExisting()) )
        return new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), Messages.getString("CatchmentModelHelper.1") ); //$NON-NLS-1$

      /* Load the timeseries. */
      /* The time zone may be different to that of the newly generated timeseries. */
      final IObservation observation = link.loadObservation();

      /* Load the temporary timeseries. */
      /* The time zone may be different to that of the original timeseries. */
      final IObservation tmpObservation = tmpLink.loadObservation();

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
        collector.add( IStatus.WARNING, Messages.getString( "CatchmentModelHelper_18" ), null, percent, differences ); //$NON-NLS-1$
      }

      return collector.asMultiStatusOrOK( statusLabel, statusLabel );
    }
    catch( final Exception ex )
    {
      collector.add( IStatus.WARNING, Messages.getString( "CatchmentModelHelper_19" ), null, ex.getLocalizedMessage() ); //$NON-NLS-1$
      return collector.asMultiStatus( statusLabel );
    }
  }

  private static Map<Long, Double> buildHash( final ITupleModel values ) throws SensorException
  {
    /* Memory for the hash. */
    final Map<Long, Double> hash = new LinkedHashMap<>();

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

  /**
   * This function compares the catchments of the na model with the catchment of the given generator.
   *
   * @param model
   *          The na model.
   * @param generator
   *          The generator.
   * @return A ERROR status, if the catchments do not match. A OK status otherwise.
   */
  public static IStatus compareCatchments( final NaModell model, final ILinearSumGenerator generator )
  {
    /* These catchments must be matched. */
    final IFeatureBindingCollection<Catchment> modelCatchments = model.getCatchments();

    /* Thease are the catchments, that must match the others. */
    final IFeatureBindingCollection<ICatchment> generatorCatchments = generator.getCatchments();

    /* Does the size match? */
    if( modelCatchments.size() != generatorCatchments.size() )
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString( "CatchmentModelHelper_20" ) ); //$NON-NLS-1$

    /* Does the order match? */
    for( int i = 0; i < modelCatchments.size(); i++ )
    {
      final Catchment modelCatchment = modelCatchments.get( i );
      final String modelId = modelCatchment.getId();

      final ICatchment generatorCatchment = generatorCatchments.get( i );
      final IXLinkedFeature generatorLink = (IXLinkedFeature) generatorCatchment.getAreaLink();
      final String generatorId = generatorLink.getFeatureId();

      if( !modelId.equals( generatorId ) )
        return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString( "CatchmentModelHelper_21" ) ); //$NON-NLS-1$
    }

    return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), Messages.getString( "CatchmentModelHelper_22" ) ); //$NON-NLS-1$
  }

  public static boolean compareMultiGenerators( final IMultiGenerator generator1, final IMultiGenerator generator2 )
  {
    final IFeatureBindingCollection<IRainfallGenerator> subGenerators1 = generator1.getSubGenerators();
    final IFeatureBindingCollection<IRainfallGenerator> subGenerators2 = generator2.getSubGenerators();

    if( subGenerators1.size() != subGenerators2.size() )
      return false;

    /* The order and types must be identical. */
    for( int i = 0; i < subGenerators1.size(); i++ )
    {
      final IRainfallGenerator subGenerator1 = subGenerators1.get( i );
      final IRainfallGenerator subGenerator2 = subGenerators2.get( i );

      if( (subGenerator1 instanceof ILinearSumGenerator && subGenerator2 instanceof IMultiGenerator) || (subGenerator1 instanceof IMultiGenerator && subGenerator2 instanceof ILinearSumGenerator) )
        return false;

      if( subGenerator1 instanceof ILinearSumGenerator && subGenerator2 instanceof ILinearSumGenerator )
      {
        if( !compareGeneratorCatchments( (ILinearSumGenerator) subGenerator1, (ILinearSumGenerator) subGenerator2, true ) )
          return false;

        continue;
      }

      if( subGenerator1 instanceof IMultiGenerator && subGenerator2 instanceof IMultiGenerator )
      {
        if( !compareMultiGenerators( (IMultiGenerator) subGenerator1, (IMultiGenerator) subGenerator2 ) )
          return false;

        continue;
      }
    }

    return true;
  }

  public static boolean compareTimeseriesMappings( final ITimeseriesMapping existingMapping, final ITimeseriesMapping mapping )
  {
    final IFeatureBindingCollection<IMappingElement> existingMappings = existingMapping.getMappings();
    final IFeatureBindingCollection<IMappingElement> mappings = mapping.getMappings();

    if( existingMappings.size() != mappings.size() )
      return false;

    for( int i = 0; i < existingMappings.size(); i++ )
    {
      final IMappingElement existingElement = existingMappings.get( i );
      final IMappingElement element = mappings.get( i );

      final ZmlLink existingLink = existingElement.getLinkedTimeseries();
      final ZmlLink link = element.getLinkedTimeseries();

      final String existingHref = existingLink.getHref();
      final String href = link.getHref();
      if( !ObjectUtils.equals( existingHref, href ) )
        return false;
    }

    return true;
  }
}