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
package org.kalypso.model.hydrology.util.validation;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.rcm.binding.ICatchment;
import org.kalypso.model.rcm.binding.ILinearSumGenerator;
import org.kalypso.model.rcm.binding.IMultiGenerator;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * Validator for catchment models.
 * 
 * @author Holger Albert
 */
public class CatchmentModelValidator
{
  /**
   * The generator to validate.
   */
  private final IRainfallGenerator m_generator;

  /**
   * The constructor.
   * 
   * @param generator
   *          The generator to validate.
   */
  public CatchmentModelValidator( final IRainfallGenerator generator )
  {
    m_generator = generator;
  }

  /**
   * This function checks, if the results are up to date.<br/>
   * <br/>
   * It will return:
   * <ul>
   * <li>A ERROR status, if an error occured during validation.</li>
   * <li>A WARNING status, if the results are outdatet.</li>
   * <li>A OK status, if the results are up to date.</li>
   * </ul>
   * 
   * @return A status representing the validation results.
   */
  public IStatus areResultsUpToDate( )
  {
    try
    {
      /* Check the type of the generator. */
      if( !(m_generator instanceof IMultiGenerator) && !(m_generator instanceof ILinearSumGenerator) )
        throw new IllegalStateException( "Can only validate generators of the type IMultiGenerator or ILinearSumGenerator..." ); // $NON-NLS-1$

      /* Check the generator. */
      return checkGenerator( m_generator );
    }
    catch( final Exception ex )
    {
      return new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, String.format( "The validation of the results of the catchment model '%s' has failed.", m_generator.getDescription() ), ex );
    }
  }

  private IStatus checkGenerator( final IRainfallGenerator generator )
  {
    /* The status collector. */
    final IStatusCollector collector = new StatusCollector( ModelNA.PLUGIN_ID );

    /* Check the catchments of the model. */
    /* Everything must be refreshed. */
    // TODO

    /* Check the generator. */
    if( generator instanceof ILinearSumGenerator )
      checkLinearSumGenerator( (ILinearSumGenerator) generator, collector );
    else
      checkMultiGenerator( (IMultiGenerator) generator, collector );

    /* Check the stati. */
    if( !collector.isOK() )
      return collector.asMultiStatus( String.format( "The results of the catchment model '%s' are outdated.", generator.getDescription() ) );

    return collector.asMultiStatus( String.format( "The results of the catchment model '%s' are up to date.", generator.getDescription() ) );
  }

  private void checkLinearSumGenerator( final ILinearSumGenerator generator, final IStatusCollector collector )
  {
    /* Check the timeseries of the model, referenced by this linear sum generator. */
    final IStatus referencedTimeseriesStatus = checkReferencedTimeseries( generator );
    collector.add( referencedTimeseriesStatus );

    /* Has the linear sum generator changed itself? */
    // TODO
  }

  private IStatus checkReferencedTimeseries( final ILinearSumGenerator generator )
  {
    /* The status collector. */
    final IStatusCollector collector = new StatusCollector( ModelNA.PLUGIN_ID );

    /* Check the timeseries. */
    final IFeatureBindingCollection<ICatchment> catchments = generator.getCatchments();
    for( final ICatchment catchment : catchments )
    {
      // TODO
    }

    /* Check the collected stati. */
    if( !collector.isOK() )
      return collector.asMultiStatus( "The timeseries have changed." );

    return collector.asMultiStatus( "The timeseries have not changed." );
  }

  private void checkMultiGenerator( final IMultiGenerator generator, final IStatusCollector collector )
  {
    /* Check the linear sum generators, referenced by this multi generator. */
    final IStatus referencedLinearSumGeneratorsStatus = checkReferencedLinearSumGenerators( generator );
    collector.add( referencedLinearSumGeneratorsStatus );

    /* Has the multi generator changed itself? */
    // TODO
  }

  private IStatus checkReferencedLinearSumGenerators( final IMultiGenerator generator )
  {
    /* The status collector. */
    final IStatusCollector collector = new StatusCollector( ModelNA.PLUGIN_ID );

    /* Get the sub generators. */
    final IFeatureBindingCollection<IRainfallGenerator> subGenerators = generator.getSubGenerators();
    for( final IRainfallGenerator subGenerator : subGenerators )
    {
      final IStatus referencedLinearSumGeneratorStatus = checkReferencedLinearSumGenerator( (ILinearSumGenerator) subGenerator );
      collector.add( referencedLinearSumGeneratorStatus );
    }

    /* Check the collected stati. */
    if( !collector.isOK() )
      return collector.asMultiStatus( "The catchment models have changed." );

    return collector.asMultiStatus( "The catchment models have not changed." );
  }

  private IStatus checkReferencedLinearSumGenerator( final ILinearSumGenerator generator )
  {
    /* The status collector. */
    final StatusCollector collector = new StatusCollector( ModelNA.PLUGIN_ID );

    /* Check the linear sum generator. */
    checkLinearSumGenerator( generator, collector );

    /* Check the stati. */
    if( !collector.isOK() )
      return collector.asMultiStatus( String.format( "The catchment model '%s' has changed.", generator.getDescription() ) );

    return collector.asMultiStatus( String.format( "The catchment model '%s' has not changed.", generator.getDescription() ) );
  }
}