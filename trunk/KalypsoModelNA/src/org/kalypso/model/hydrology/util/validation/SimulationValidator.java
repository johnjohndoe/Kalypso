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
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.model.rcm.binding.IRainfallGenerator;

/**
 * Validator for simulations.
 * 
 * @author Holger Albert
 */
public class SimulationValidator
{
  /**
   * The simulation to validate.
   */
  private final RrmSimulation m_simulation;

  /**
   * The na control contains parameter of the simulation.
   */
  private final NAControl m_control;

  /**
   * The constructor.
   * 
   * @param simulation
   *          The simulation to validate.
   * @param control
   *          The na control contains parameter of the simulation.
   */
  public SimulationValidator( final RrmSimulation simulation, final NAControl control )
  {
    m_simulation = simulation;
    m_control = control;
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
      /* Check the simulation. */
      return checkSimulation( m_simulation, m_control );
    }
    catch( final Exception ex )
    {
      return new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, String.format( "The validation of the results of the simulation '%s' has failed.", m_control.getDescription() ), ex );
    }
  }

  private IStatus checkSimulation( final RrmSimulation simulation, final NAControl control )
  {
    /* The status collector. */
    final IStatusCollector collector = new StatusCollector( ModelNA.PLUGIN_ID );

    /* Check the catchment models, referenced by this simulation. */
    final IStatus catchmentModelsStatus = checkReferencedCatchmentModels( control );
    collector.add( catchmentModelsStatus );

    /* Is the simulation an unchanged one of the project import? */
    // TODO

    /* Other input data? */
    // TODO

    /* Has the simulation changed itself? */
    // TODO

    /* Check the stati. */
    if( !collector.isOK() )
      return collector.asMultiStatus( String.format( "The results of the simulation '%s' are outdated.", m_control.getDescription() ) );

    return collector.asMultiStatus( String.format( "The results of the simulation '%s' are up to date.", m_control.getDescription() ) );
  }

  private IStatus checkReferencedCatchmentModels( final NAControl control )
  {
    /* The status collector. */
    final IStatusCollector collector = new StatusCollector( ModelNA.PLUGIN_ID );

    /* Get all referenced catchment models. */
    final IRainfallGenerator generatorN = control.getGeneratorN();
    final IRainfallGenerator generatorT = control.getGeneratorT();
    final IRainfallGenerator generatorE = control.getGeneratorE();

    /* Create the validators. */
    final CatchmentModelValidator validatorN = new CatchmentModelValidator( generatorN );
    final CatchmentModelValidator validatorT = new CatchmentModelValidator( generatorT );
    final CatchmentModelValidator validatorE = new CatchmentModelValidator( generatorE );

    /* Validate the catchment models. */
    collector.add( validatorN.areResultsUpToDate() );
    collector.add( validatorT.areResultsUpToDate() );
    collector.add( validatorE.areResultsUpToDate() );

    /* Check the stati. */
    if( !collector.isOK() )
      return collector.asMultiStatus( "The catchment models have changed." );

    return collector.asMultiStatus( "The catchment models have not changed." );
  }
}