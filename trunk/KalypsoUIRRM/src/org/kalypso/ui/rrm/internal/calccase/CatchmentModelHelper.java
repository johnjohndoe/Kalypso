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
import java.net.URLEncoder;
import java.util.Date;

import javax.xml.namespace.QName;

import org.apache.commons.lang3.ObjectUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.joda.time.DateTime;
import org.joda.time.Interval;
import org.joda.time.LocalTime;
import org.joda.time.Period;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.java.math.IntervalUtilities;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.rcm.binding.ICatchment;
import org.kalypso.model.rcm.binding.ILinearSumGenerator;
import org.kalypso.model.rcm.binding.IMultiGenerator;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.model.rcm.util.RainfallGeneratorUtilities;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.zml.obslink.TimeseriesLinkType;
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
   * This function executes the catchment model.
   * 
   * @param info
   *          The catchment model info.
   * @param monitor
   *          A progress monitor.
   */
  public static void executeCatchmentModel( final CatchmentModelInfo info, final IProgressMonitor monitor ) throws CoreException
  {
    /* Get the generator. */
    final IRainfallGenerator generator = info.getGenerator();

    /* Find the responsible catchment model runner. */
    AbstractCatchmentModelRunner modelRunner = null;
    if( generator instanceof ILinearSumGenerator )
      modelRunner = new LinearSumCatchmentModelRunner( null );
    else if( generator instanceof IMultiGenerator )
      modelRunner = new MultiCatchmentModelRunner();
    else
      throw new IllegalArgumentException( "The type of the generator must be that of ILinearSumGenerator or IMultiGenerator..." ); // $NON-NLS-1$

    /* Execute the catchment model. */
    modelRunner.executeCatchmentModel( info, monitor );
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
      case ITimeseriesConstants.TYPE_MEAN_EVAPORATION_LAND_BASED:
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
   * <li>Only one generator may be overlapped.</li>
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
      if( generator == firstGenerator )
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
      if( !compareGeneratorCatchments( firstGenerator, linearSumGenerator ) )
      {
        collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), String.format( "The catchments of the generator '%s' does not match the catchments of the first generator '%s'.", generator.getDescription(), firstGenerator.getDescription() ) ) );
        continue;
      }

      /* HINT: If there is only one generator, we do not reach the code here. */
      /* HINT: If we do reach here, it will be the 2nd loop or one after. */

      /* (5) Only one generator may be overlapped. */
      if( !compareGeneratorValidityOverlap( generator, generators ) )
      {
        collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), String.format( "The validity range of generator '%s' overlaps the validity range of more than one generator.", generator.getDescription() ) ) );
        continue;
      }
    }

    /* (6) There are no gaps allowed between the validity ranges of adjacent generators. */
    if( !compareGeneratorValidityGaps( generators, control, firstGenerator.getTimestep() ) )
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
   * <li>The factors and timeseries in the catchments.</li>
   * </ul>
   * 
   * @param generator1
   *          The first linear sum generator.
   * @param generator2
   *          The second linear sum generator.
   * @return True, if the catchments of the linear sum generators are equal. False otherwise.
   */
  public static boolean compareGeneratorCatchments( final ILinearSumGenerator generator1, final ILinearSumGenerator generator2 )
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

      /* Build the hash. */
      final String hash1 = RainfallGeneratorUtilities.buildHash( catchment1 );
      final String hash2 = RainfallGeneratorUtilities.buildHash( catchment2 );
      if( !hash1.equals( hash2 ) )
        return false;
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
   * @return True, if the validity range of the compare generator does not overlap the validity ranges of the other
   *         generators, or overlaps only one. False, if it overlaps two or more.
   */
  private static boolean compareGeneratorValidityOverlap( final IRainfallGenerator compareGenerator, final IFeatureBindingCollection<IRainfallGenerator> generators )
  {
    /* No generators available, to compare to. */
    if( generators.size() == 0 )
      return true;

    /* The interval of the compare generator. */
    final Interval compareInterval = new Interval( new DateTime( compareGenerator.getValidFrom() ), new DateTime( compareGenerator.getValidTo() ) );

    /* Check if the interval overlaps one of the other intervals. */
    boolean oneOverlapped = false;
    for( final IRainfallGenerator generator : generators )
    {
      /* Do not compare the compare generator with itself. */
      if( compareGenerator == generator )
        continue;

      /* The interval of the generator. */
      final Interval interval = new Interval( new DateTime( generator.getValidFrom() ), new DateTime( generator.getValidTo() ) );
      if( compareInterval.overlaps( interval ) )
      {
        /* If there was already one overlapped, this is the second than. Return false. */
        if( oneOverlapped )
          return false;

        /* If there was none overlapped yet, this is the first one. This is allowed. */
        oneOverlapped = true;
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
   * @return True, if the validity ranges of the generators do not have gaps or only timestep sized gaps. False
   *         otherwise.
   */
  private static boolean compareGeneratorValidityGaps( final IFeatureBindingCollection<IRainfallGenerator> generators, final NAControl control, final Integer timestep )
  {
    /* No generators available. */
    /* Only one generator available. */
    if( generators.size() <= 1 )
      return true;

    /* The generators. */
    final IRainfallGenerator[] generatorArray = generators.toArray( new IRainfallGenerator[] {} );

    /* Build the simulation interval. */
    final Date simulationStartDate = control.getSimulationStart();
    final Date simulationEndDate = control.getSimulationEnd();
    final long simulationStartTime = simulationStartDate.getTime();
    final long simulationEndTime = simulationEndDate.getTime();
    final org.kalypso.contribs.java.math.Interval simulationInterval = new org.kalypso.contribs.java.math.Interval( simulationStartTime, simulationEndTime );
    org.kalypso.contribs.java.math.Interval[] simulationRest = new org.kalypso.contribs.java.math.Interval[] { simulationInterval };

    /* Check each generator. */
    for( final IRainfallGenerator generator : generatorArray )
    {
      /* Build the generator interval. */
      final Date generatorStartDate = generator.getValidFrom();
      final Date generatorEndDate = generator.getValidTo();
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
   *          The timestamp.
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

    /* Further adjust range by predefined time. */
    final DateTime startWithTime = adjustedStart.withTime( timestamp.getHourOfDay(), timestamp.getMinuteOfHour(), timestamp.getSecondOfMinute(), timestamp.getMillisOfSecond() );
    final DateTime endWithTime = adjustedEnd.withTime( timestamp.getHourOfDay(), timestamp.getMinuteOfHour(), timestamp.getSecondOfMinute(), timestamp.getMillisOfSecond() );

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
}