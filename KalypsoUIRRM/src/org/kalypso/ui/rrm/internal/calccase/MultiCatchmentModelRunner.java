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

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.joda.time.DateTime;
import org.joda.time.Interval;
import org.joda.time.LocalTime;
import org.joda.time.Period;
import org.kalypso.model.hydrology.binding.cm.ILinearSumGenerator;
import org.kalypso.model.hydrology.binding.cm.IMultiGenerator;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * This class executes a catchment model with a multi generator.
 * 
 * @author Holger Albert
 */
public class MultiCatchmentModelRunner extends AbstractCatchmentModelRunner
{
  /**
   * The constructor.
   */
  public MultiCatchmentModelRunner( )
  {
  }

  /**
   * @see org.kalypso.ui.rrm.internal.calccase.AbstractCatchmentModelRunner#executeCatchmentModel(org.kalypso.ui.rrm.internal.calccase.ICatchmentModelInfo, org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public void executeCatchmentModel( final ICatchmentModelInfo info, final IProgressMonitor monitor ) throws CoreException
  {
    /* Get the parameters. */
    final RrmSimulation simulation = info.getSimulation();
    final NAControl control = info.getControl();
    final NaModell model = info.getModel();
    final IRainfallGenerator generator = info.getGenerator();
    final QName targetLink = info.getTargetLink();
    final String parameterType = info.getParameterType();

    /* Only IMultiGenerator's are supported. */
    if( !(generator instanceof IMultiGenerator) )
      throw new UnsupportedOperationException( "Only IMultiGenerator's are supported..." ); //$NON-NLS-1$

    /* Cast. */
    final IMultiGenerator multiGenerator = (IMultiGenerator)generator;

    /* Get the sub generators. */
    final IFeatureBindingCollection<IRainfallGenerator> subGenerators = multiGenerator.getSubGenerators();

    try
    {
      /* Monitor. */
      monitor.beginTask( Messages.getString( "MultiCatchmentModelRunner_0" ), subGenerators.size() * 100 + 300 ); //$NON-NLS-1$
      monitor.subTask( Messages.getString( "MultiCatchmentModelRunner_1" ) ); //$NON-NLS-1$

      /* Validate the multi generator. */
      final IStatus validateStatus = CatchmentModelHelper.validateMultiGenerator( multiGenerator, control );
      if( !validateStatus.isOK() )
        throw new CoreException( validateStatus );

      /* Monitor. */
      monitor.worked( 100 );
      monitor.subTask( String.format( Messages.getString( "MultiCatchmentModelRunner_2" ), subGenerators.size() ) ); //$NON-NLS-1$

      /* Hash for catchment to timeseries links. */
      /* HINT: One catchment can contain more than one timeseries link. */
      /* HINT: There should be the same number of timeseries links per catchment in the end, than there are generators. */
      /* HINT: The order of the timeseries links (per catchment) should be same, than the order of the generators. */
      final CatchmentTimeseriesHash hash = new CatchmentTimeseriesHash();

      /* Run all contained generators. */
      for( int i = 0; i < subGenerators.size(); i++ )
        runGenerator( String.format( Locale.PRC, "%d", i ), simulation, control, model, (ILinearSumGenerator)subGenerators.get( i ), targetLink, parameterType, hash, new SubProgressMonitor( monitor, 100 ) ); //$NON-NLS-1$

      /* Monitor. */
      monitor.subTask( Messages.getString( "MultiCatchmentModelRunner_4" ) ); //$NON-NLS-1$

      /* The timeseries must be merged. */
      final IFolder modelsFolder = simulation.getModelsFolder();
      final IPath location = modelsFolder.getLocation();
      final Map<String, IObservation> mergedObservations = hash.merge( location.toFile().toURI().toURL() );

      /* Monitor. */
      monitor.worked( 100 );
      monitor.subTask( Messages.getString( "MultiCatchmentModelRunner_5" ) ); //$NON-NLS-1$

      /* The model.gml links needs to be adjusted. */
      adjustSimulationModelGml( simulation, model, targetLink, parameterType, mergedObservations );

      /* Monitor. */
      monitor.worked( 100 );
    }
    catch( final Exception ex )
    {
      throw new CoreException( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString( "MultiCatchmentModelRunner_6" ), ex ) ); //$NON-NLS-1$
    }
    finally
    {
      /* Monitor. */
      monitor.done();

      /* Refresh the simulation folder. */
      refresh( simulation );
    }
  }

  /**
   * This function runs the linear sum generator.
   * 
   * @param prefix
   *          This prefix is used when writing the timeseries.
   * @param simulation
   *          The simulation.
   * @param control
   *          The na control.
   * @param model
   *          The na model.
   * @param generator
   *          The rainfall generator.
   * @param targetLink
   *          The target link.
   * @param parameterType
   *          The parameter type.
   * @param hash
   *          The hash.
   * @param monitor
   *          A progress monitor.
   */
  private void runGenerator( final String prefix, final RrmSimulation simulation, final NAControl control, final NaModell model, final ILinearSumGenerator generator, final QName targetLink, final String parameterType, final CatchmentTimeseriesHash hash, final IProgressMonitor monitor ) throws Exception
  {
    try
    {
      /* Monitor. */
      monitor.beginTask( String.format( Messages.getString( "MultiCatchmentModelRunner_7" ), generator.getDescription() ), 1000 ); //$NON-NLS-1$
      monitor.subTask( Messages.getString( "MultiCatchmentModelRunner_8" ) ); //$NON-NLS-1$

      /* This object can calculate some values. */
      final LinearSumCatchmentModelInfo linearInfo = new LinearSumCatchmentModelInfo( simulation, control, model, generator, targetLink, parameterType );

      /* Get the timestep and timestamp. */
      final Period timestep = linearInfo.getTimestep();
      final LocalTime timestamp = linearInfo.getTimestamp();

      /* HINT: The range is the adjusted simulation range. */
      final DateRange simulationRange = linearInfo.getSimulationRange();

      /* Intersect adjusted simulation range with validity range of generator. */
      final Interval simulationInterval = new Interval( new DateTime( simulationRange.getFrom() ), new DateTime( simulationRange.getTo() ) );

      /* Calculate range of current generator. */
      final Interval validityInterval = new Interval( new DateTime( generator.getValidFrom() ), new DateTime( generator.getValidTo() ) );

      /* Intersect complete simulation with this generator, only this range will be calculated. */
      final Interval interval = validityInterval.overlap( simulationInterval );

      // FIXME: The intersected interval does now NOT cover the extended range (3 steps before, 1 step after).
      // So the complete result does NOT cover the extended range.

      final DateRange range = new DateRange( interval.getStart().toDate(), interval.getEnd().toDate() );

      /* The catchment model runner should be executed with this generic info. */
      final ICatchmentModelInfo genericInfo = new GenericCatchmentModelInfo( simulation, control, model, generator, targetLink, parameterType, timestep, timestamp, range, range );

      /* Create the linear sum catchment model runner. */
      final LinearSumCatchmentModelRunner runner = new LinearSumCatchmentModelRunner( prefix );

      /* Calculate the catchment model. */
      runner.executeCatchmentModel( genericInfo, new SubProgressMonitor( monitor, 500 ) );

      /* Get the catchments. */
      final IFeatureBindingCollection<Catchment> catchments = model.getCatchments();
      for( final Catchment catchment : catchments )
      {
        /* The feature id of the catchment must be unique. */
        final String id = catchment.getId();

        /* Get the link of the timeseries. */
        ZmlLink link = null;
        if( parameterType.equals( ITimeseriesConstants.TYPE_RAINFALL ) )
          link = catchment.getPrecipitationLink();
        else if( parameterType.equals( ITimeseriesConstants.TYPE_EVAPORATION_LAND_BASED ) )
          link = catchment.getEvaporationLink();
        else if( parameterType.equals( ITimeseriesConstants.TYPE_MEAN_TEMPERATURE ) )
          link = catchment.getTemperatureLink();
        else
          throw new IllegalArgumentException( Messages.getString( "MultiCatchmentModelRunner_9" ) ); //$NON-NLS-1$

        /* Store the timeseries link. */
        hash.put( id, link.getTimeseriesLink() );
      }

      /* Monitor. */
      monitor.worked( 500 );
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }

  /**
   * This function saves the observations and adjusts the model.gml. For each catchment in the simulation.gml there must
   * be an observation in the observation hash. If for several catchments an equal observation exists, it will only be
   * saved for the first catchment and the created link will be set in all other catchments.
   * 
   * @param simulation
   *          The simulation.
   * @param targetLink
   *          The target link.
   * @param parameterType
   *          The parameter type.
   * @param observations
   *          The catchment->observation hash.
   */
  private void adjustSimulationModelGml( final RrmSimulation simulation, final NaModell model, final QName targetLink, final String parameterType, final Map<String, IObservation> observations ) throws Exception
  {
    /* Memory for the already used hash codes and their filenames. */
    final Map<String, String> usedHashCodes = new HashMap<>();

    /* Get the catchments. */
    final IFeatureBindingCollection<Catchment> catchments = model.getCatchments();
    for( final Catchment catchment : catchments )
    {
      /* Get the feature id. */
      final String id = catchment.getId();

      /* Get the observation for that catchment id. */
      final IObservation observation = observations.get( id );

      /* Get the hash code. */
      final String hashCode = observation.getMetadataList().getProperty( CatchmentTimeseriesHash.MD_HASH_CODE );
      if( hashCode != null && hashCode.length() > 0 && usedHashCodes.containsKey( hashCode ) )
      {
        /* HINT: If the hash code is an existing one, use the existing link for the catchment. */
        final String link = usedHashCodes.get( hashCode );

        /* Set the link. */
        CatchmentModelHelper.setLink( catchment, targetLink, link );

        continue;
      }

      /* HINT: If the hash code is null, create a new link for the catchment. */
      /* HINT: If the hash code is a new one, create a new link for the catchment. */
      final String folderName = CatchmentModelHelper.getTargetLinkFolderName( simulation, parameterType );
      final String link = CatchmentModelHelper.buildLink( simulation, null, folderName, parameterType, catchment );

      /* Set the link. */
      CatchmentModelHelper.setLink( catchment, targetLink, link );

      /* Save the observation. */
      final IFolder modelsFolder = simulation.getModelsFolder();
      final IFile observationFile = modelsFolder.getFile( new Path( link ) );
      ZmlFactory.writeToFile( observation, observationFile );

      /* HINT: If the hash code is a new one, save it along with its link. */
      if( hashCode != null && hashCode.length() > 0 )
        usedHashCodes.put( hashCode, link );
    }
  }
}