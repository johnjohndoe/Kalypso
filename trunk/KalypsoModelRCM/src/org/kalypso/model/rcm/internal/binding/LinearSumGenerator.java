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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
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
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;

/**
 * The linear sum generator.
 * 
 * @author Holger Albert
 */
public class LinearSumGenerator extends AbstractRainfallGenerator implements ILinearSumGenerator
{
  public LinearSumGenerator( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
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
    final ICatchment[] catchments = getCatchments();

    /* HINT: Keep in mind, that the results must match the order of the catchments array. */
    final IObservation[] results = new IObservation[catchments.length];

    try
    {
      /* Monitor. */
      monitor.beginTask( String.format( "Generiere Zeitreihen für %d Gebiete...", catchments.length ), catchments.length * 200 );
      monitor.subTask( "Generiere Zeitreihen..." );

      /* Generate one timeseries for each catchment. */
      for( int i = 0; i < catchments.length; i++ )
      {
        /* Generate the message 1. */
        final String message1 = String.format( "Sammle gewichteten Zeitreihen zur Erzeugung von Zeitreihe %d...", i + 1 );

        /* Monitor. */
        monitor.subTask( message1 );

        /* Log. */
        if( log != null )
          log.log( new Status( IStatus.INFO, KalypsoModelRcmActivator.PLUGIN_ID, message1 ) );

        /* Get the catchment. */
        final ICatchment catchment = catchments[i];

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

          // TODO: get from gml
          final IZmlFilter[] filters = new IZmlFilter[] {};

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

  /**
   * @see org.kalypso.model.rcm.binding.ILinearSumGenerator#getComment()
   */
  @Override
  public String getComment( )
  {
    return getProperty( PROPERTY_COMMENT, String.class );
  }

  /**
   * @see org.kalypso.model.rcm.binding.ILinearSumGenerator#getAreaNameProperty()
   */
  @Override
  public String getAreaNameProperty( )
  {
    return getProperty( PROPERTY_AREA_NAME, String.class );
  }

  /**
   * @see org.kalypso.model.rcm.binding.ILinearSumGenerator#getAreaDescriptionProperty()
   */
  @Override
  public String getAreaDescriptionProperty( )
  {
    return getProperty( PROPERTY_AREA_DESCRIPTION, String.class );
  }

  /**
   * @see org.kalypso.model.rcm.binding.ILinearSumGenerator#getAreaProperty()
   */
  @Override
  public String getAreaProperty( )
  {
    return getProperty( PROPERTY_AREA, String.class );
  }

  /**
   * @see org.kalypso.model.rcm.binding.ILinearSumGenerator#getCatchments()
   */
  @Override
  public ICatchment[] getCatchments( )
  {
    /* Memory for the results. */
    final List<Catchment> results = new ArrayList<Catchment>();

    /* Get all catchments. */
    final FeatureList catchments = (FeatureList) getProperty( MEMBER_CATCHMENT );
    for( int i = 0; i < catchments.size(); i++ )
      results.add( (Catchment) catchments.get( i ) );

    return results.toArray( new Catchment[] {} );
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
}