/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.rcm.internal.binding;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.joda.time.DateTimeZone;
import org.joda.time.LocalTime;
import org.joda.time.Period;
import org.joda.time.format.DateTimeFormatter;
import org.joda.time.format.ISODateTimeFormat;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.rcm.binding.AbstractRainfallGenerator;
import org.kalypso.model.rcm.binding.ICatchment;
import org.kalypso.model.rcm.binding.IFactorizedTimeseries;
import org.kalypso.model.rcm.binding.ILinearSumGenerator;
import org.kalypso.model.rcm.internal.KalypsoModelRcmActivator;
import org.kalypso.model.rcm.util.RainfallGeneratorUtilities;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.zml.core.filter.binding.IZmlFilter;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;

/**
 * The linear sum generator.
 *
 * @author Holger Albert
 */
public class LinearSumGenerator extends AbstractRainfallGenerator implements ILinearSumGenerator
{
  private final IFeatureBindingCollection<ICatchment> m_catchments;

  public LinearSumGenerator( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );

    m_catchments = new FeatureBindingCollection<ICatchment>( this, ICatchment.class, MEMBER_CATCHMENT );
  }

  /**
   * @param catchmentFeatures
   *          The catchment features will be taken from the generator itself, so they are not needed here. Leave them
   *          <code>null</code>.
   */
  @Override
  public IObservation[] createRainfall( final Feature[] catchmentFeatures, final DateRange range, final ILog log, IProgressMonitor monitor ) throws CoreException
  {
    /* Monitor. */
    if( monitor == null )
      monitor = new NullProgressMonitor();

    /* Get the catchments. */
    final List<ICatchment> catchments = getCatchments();

    /* HINT: Keep in mind, that the results must match the order of the catchments array. */
    final IObservation[] results = new IObservation[catchments.size()];

    try
    {
      /* Monitor. */
      monitor.beginTask( String.format( "Generiere Zeitreihen für %d Gebiete...", catchments.size() ), catchments.size() * 200 );
      monitor.subTask( "Generiere Zeitreihen..." );

      /* Generate one timeseries for each catchment. */
      for( int i = 0; i < catchments.size(); i++ )
      {
        /* Get the catchment. */
        final ICatchment catchment = catchments.get( i );

        // TODO: schöner als die Zahl wäre der Name/Beschreibung des Catchments (über die catchment property)
        // zusätzlich: auch die Gesamtanzahl wäre schön

        // FIXME: avoid generation of timeseries with same observation/weights combination

        /* Generate the message 1. */
        final String message1 = String.format( "Sammle gewichtete Zeitreihen zur Erzeugung von Zeitreihe %d...", i + 1 );

        /* Monitor. */
        monitor.subTask( message1 );

        /* Log. */
        if( log != null )
          log.log( new Status( IStatus.INFO, KalypsoModelRcmActivator.PLUGIN_ID, message1 ) );

        /* Memory for the factors and the observations of the catchments. */
        final List<Double> factors = new ArrayList<Double>();
        final List<IObservation> observations = new ArrayList<IObservation>();

        /* Get the factorized timeseries. */
        for( final IFactorizedTimeseries factorizedTimeseries : catchment.getFactorizedTimeseries() )
        {
          /* Get the factor. */
          final BigDecimal factor = factorizedTimeseries.getFactor();

          /* Get the timeseries link. */
          final GMLXPath linkPath = new GMLXPath( IFactorizedTimeseries.PROPERTY_TIMESERIES_LINK );

          /* Get the filters from the gml. */
          final IZmlFilter[] filters = getFilters().toArray( new IZmlFilter[] {} );

          /* Load the observation. */
          final IObservation[] readObservations = RainfallGeneratorUtilities.readObservations( new Feature[] { factorizedTimeseries }, linkPath, filters, range );
          final IObservation observation = readObservations[0];

          /* If the factor is valid, add the factor and its observation. */
          if( factor != null && factor.intValue() > 0 && factor.intValue() <= 100 )
          {
            factors.add( new Double( factor.doubleValue() / 100 ) );
            observations.add( observation );
          }
        }

        /* Generate the message 2. */
        final String message2 = String.format( "Erzeuge Zeitreihe %d mit %d gewichteten Zeitreihen...", i + 1, factors.size() );

        /* Monitor. */
        monitor.worked( 100 );
        monitor.subTask( message2 );

        /* Log. */
        if( log != null )
          log.log( new Status( IStatus.INFO, KalypsoModelRcmActivator.PLUGIN_ID, message2 ) );

        /* Set the resulting observation for this catchment. */
        results[i] = RainfallGeneratorUtilities.combineObses( observations.toArray( new IObservation[] {} ), convertDoubles( factors.toArray( new Double[] {} ) ), "catchmentGenerator://thiessen" );

        /* Monitor. */
        monitor.worked( 100 );
      }

      return results;
    }
    catch( final Exception ex )
    {
      /* Create the error status. */
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, ex.getLocalizedMessage(), ex );

      /* If there is a log, log to it. */
      if( log != null )
        log.log( status );

      throw new CoreException( status );
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }

  @Override
  public String getComment( )
  {
    return getProperty( PROPERTY_COMMENT, String.class );
  }

  @Override
  public void setComment( final String comment )
  {
    setProperty( PROPERTY_COMMENT, comment );
  }

  @Override
  public Integer getTimestep( )
  {
    return getProperty( PROPERTY_TIMESTEP, Integer.class );
  }

  @Override
  public void setTimestep( final Integer timestep )
  {
    setProperty( PROPERTY_TIMESTEP, timestep );
  }

  @Override
  public GMLXPath getAreaNamePath( )
  {
    return getPath( PROPERTY_AREA_NAME );
  }

  @Override
  public void setAreaNamePath( final GMLXPath path )
  {
    setPath( PROPERTY_AREA_NAME, path );
  }

  @Override
  public GMLXPath getAreaDescriptionPath( )
  {
    return getPath( PROPERTY_AREA_DESCRIPTION );
  }

  @Override
  public void setAreaDescriptionPath( final GMLXPath path )
  {
    setPath( PROPERTY_AREA_DESCRIPTION, path );
  }

  @Override
  public GMLXPath getAreaPath( )
  {
    return getPath( PROPERTY_AREA );
  }

  @Override
  public void setAreaPath( final GMLXPath path )
  {
    setPath( PROPERTY_AREA, path );
  }

  private GMLXPath getPath( final QName property )
  {
    final String value = getProperty( property, String.class );
    if( StringUtils.isBlank( value ) )
      return null;

    return new GMLXPath( value, getWorkspace().getNamespaceContext() );
  }

  private void setPath( final QName property, final GMLXPath path )
  {
    if( path == null )
      setProperty( property, null );
    else
      setProperty( property, path.toString() );
  }

  @Override
  public IFeatureBindingCollection<ICatchment> getCatchments( )
  {
    return m_catchments;
  }

  /**
   * This function converts an array of Doubles to an array of doubles.
   *
   * @param factors
   *          The array of Doubles.
   * @return A array of doubles.
   */
  private double[] convertDoubles( final Double[] factors )
  {
    final double[] weights = new double[factors.length];
    for( int j = 0; j < factors.length; j++ )
    {
      final Double factor = factors[j];
      weights[j] = factor.doubleValue();
    }

    return weights;
  }

  @Override
  public LocalTime getTimeStamp( )
  {
    final String timestampText = getProperty( PROPERTY_TIMESTAMP, String.class );
    if( StringUtils.isBlank( timestampText ) )
      return null;

    try
    {
      final DateTimeZone zone = DateTimeZone.forTimeZone( KalypsoCorePlugin.getDefault().getTimeZone() );
      final DateTimeFormatter parser = ISODateTimeFormat.localTimeParser().withZone( zone );
      final LocalTime utcTime = parser.parseLocalTime( timestampText );

      final int offset = zone.getOffset( utcTime.toDateTimeToday().getMillis() );

      final Period offsetPeriod = Period.millis( offset ).normalizedStandard();

      return utcTime.plus( offsetPeriod );
    }
    catch( final IllegalArgumentException e )
    {
      e.printStackTrace();
      return null;
    }
  }

  @Override
  public void setTimeStamp( final LocalTime timestamp )
  {
    // TODO: check: is zone correctly considered?
    if( timestamp == null )
      setProperty( PROPERTY_TIMESTAMP, null );
    else
      setProperty( PROPERTY_TIMESTAMP, timestamp.toString( "HH:mm" ) ); //$NON-NLS-1$
  }
}