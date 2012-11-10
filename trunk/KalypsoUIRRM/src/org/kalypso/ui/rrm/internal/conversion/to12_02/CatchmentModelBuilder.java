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
package org.kalypso.ui.rrm.internal.conversion.to12_02;

import java.io.File;
import java.math.BigDecimal;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.xml.namespace.QName;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.joda.time.LocalTime;
import org.joda.time.MutablePeriod;
import org.joda.time.Period;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.binding.cm.ICatchment;
import org.kalypso.model.hydrology.binding.cm.ICatchmentModel;
import org.kalypso.model.hydrology.binding.cm.IFactorizedTimeseries;
import org.kalypso.model.hydrology.binding.cm.ILinearSumGenerator;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ogc.sensor.timeseries.TimeseriesUtils;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;

/**
 * Helper that guesses a CatchmentModel for each existing calculation case.
 * 
 * @author Gernot Belger
 * @author Holger Albert
 */
public class CatchmentModelBuilder
{
  private final Map<String, String> m_generatorIds = new HashMap<>();

  private final NaModell m_naModel;

  private final ICatchmentModel m_catchmentModel;

  private final File m_simulationDir;

  private final GlobalConversionData m_globalData;

  public CatchmentModelBuilder( final NaModell naModel, final ICatchmentModel catchmentModel, final File simulationDir, final GlobalConversionData globalData )
  {
    m_naModel = naModel;
    m_catchmentModel = catchmentModel;
    m_simulationDir = simulationDir;
    m_globalData = globalData;
  }

  public IStatus execute( final QName modelTimeseriesLink, final String parameterType )
  {
    final IStatusCollector log = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    /* Create and add new generator */
    final IFeatureBindingCollection<IRainfallGenerator> generators = m_catchmentModel.getGenerators();
    final ILinearSumGenerator generator = generators.addNew( ILinearSumGenerator.FEATURE_LINEAR_SUM_GENERATOR, ILinearSumGenerator.class );
    m_generatorIds.put( parameterType, generator.getId() );

    /* Set the properties. */
    generator.setParameterType( parameterType );
    generator.setDescription( m_simulationDir.getName() );
    generator.setComment( Messages.getString( "CatchmentModelBuilder_0" ) ); //$NON-NLS-1$
    generator.setAreaPath( new GMLXPath( NaModell.PROPERTY_LOCATION ) );
    generator.setAreaNamePath( new GMLXPath( NaModell.QN_NAME ) );
    generator.setAreaDescriptionPath( new GMLXPath( NaModell.QN_DESCRIPTION ) );

    /* Very big period, so everything is smaller. */
    final MutablePeriod smallestPeriod = Period.days( 10000 ).toMutablePeriod();

    /* Memory for the timestamps. */
    final Map<LocalTime, Integer> timestamps = new HashMap<>();

    /* Add catchments */
    final IFeatureBindingCollection<Catchment> catchments = m_naModel.getCatchments();
    for( final Catchment catchment : catchments )
    {
      try
      {
        final IStatus status = buildCatchment( generator, catchment, modelTimeseriesLink, parameterType, smallestPeriod, timestamps );
        log.add( status );
      }
      catch( final MalformedURLException e )
      {
        log.add( IStatus.WARNING, Messages.getString( "CatchmentModelBuilder.0" ), e ); //$NON-NLS-1$
      }
    }

    /* Use the smallest period of all involved timeseries as timestep for the generator. */
    final int timestepMinutes = smallestPeriod.toPeriod().toStandardMinutes().getMinutes();
    if( timestepMinutes > 0 )
      generator.setTimestep( timestepMinutes );

    /* Set the timestamp. */
    final LocalTime timestamp = findMostUsedTimestamp( timestamps );
    if( timestamp != null )
      generator.setTimestamp( timestamp );

    final String parameterName = TimeseriesUtils.getName( parameterType );
    final String message = String.format( Messages.getString( "CatchmentModelBuilder_1" ), parameterName ); //$NON-NLS-1$
    return log.asMultiStatusOrOK( message, message );
  }

  private IStatus buildCatchment( final ILinearSumGenerator generator, final Catchment modelCatchment, final QName modelTimeseriesLink, final String parameterType, final MutablePeriod smallestTimestep, final Map<LocalTime, Integer> timestamps ) throws MalformedURLException
  {
    // IMPORTANT: we use the simulation-models folder as context, because this is the right relative location
    // for the fixed timeseries links.
    final URL timeseriesContext = new File( m_simulationDir, RrmSimulation.FOLDER_MODELS ).toURI().toURL();
    final ZmlLink modelTargetLink = new ZmlLink( modelCatchment, modelTimeseriesLink, timeseriesContext );

    /* Create new catchment. */
    final IFeatureBindingCollection<ICatchment> catchments = generator.getCatchments();
    final ICatchment newCatchment = catchments.addNew( ICatchment.FEATURE_CATCHMENT );

    /* Link to model's catchment. */
    final String href = String.format( "%s#%s", RrmScenario.FILE_MODELL_GML, modelCatchment.getId() ); //$NON-NLS-1$
    newCatchment.setAreaLink( href );

    final TimeseriesIndex timeseriesIndex = m_globalData.getTimeseriesIndex();
    final Map<String, TimeseriesIndexEntry> oldMapping = m_globalData.getOldMapping( parameterType );

    /* Guess timeseries link... */
    final CatchmentTimeseriesGuesser timeseriesGuesser = new CatchmentTimeseriesGuesser( modelTargetLink, parameterType, timeseriesIndex, oldMapping );
    final IStatus guessStatus = timeseriesGuesser.execute();
    final String timeseriesPath = timeseriesGuesser.getResult();
    final Period timestep = timeseriesGuesser.getResultTimestep();
    final LocalTime timestamp = timeseriesGuesser.getResultTimestamp();

    if( !StringUtils.isBlank( timeseriesPath ) )
    {
      /* ...and set to the catchment. */
      final IFeatureBindingCollection<IFactorizedTimeseries> timeseriesList = newCatchment.getFactorizedTimeseries();
      final IFactorizedTimeseries timeseries = timeseriesList.addNew( IFactorizedTimeseries.FEATURE_FACTORIZED_TIMESERIES );
      timeseries.setFactor( new BigDecimal( 100 ) );
      timeseries.setTimeseriesLink( timeseriesPath );
    }

    if( timestep != null )
    {
      if( timestep.toStandardSeconds().getSeconds() < smallestTimestep.toPeriod().toStandardSeconds().getSeconds() )
        smallestTimestep.setPeriod( timestep );
    }

    if( timestamp != null )
    {
      final Integer number = timestamps.get( timestamp );
      if( number != null )
        timestamps.put( timestamp, new Integer( number.intValue() + 1 ) );
      else
        timestamps.put( timestamp, new Integer( 1 ) );
    }

    return guessStatus;
  }

  private LocalTime findMostUsedTimestamp( final Map<LocalTime, Integer> timestamps )
  {
    LocalTime timestamp = null;
    int number = 0;

    final Set<Entry<LocalTime, Integer>> entrySet = timestamps.entrySet();
    for( final Entry<LocalTime, Integer> entry : entrySet )
    {
      final LocalTime key = entry.getKey();
      final Integer value = entry.getValue();

      if( value.intValue() > number )
      {
        timestamp = key;
        number = value.intValue();
      }
    }

    return timestamp;
  }

  public String getGeneratorPath( final String parameterType )
  {
    final String id = m_generatorIds.get( parameterType );
    if( id == null )
      return null;

    return String.format( "%s#%s", RrmScenario.FILE_CATCHMENT_MODELS_GML, id ); //$NON-NLS-1$
  }
}