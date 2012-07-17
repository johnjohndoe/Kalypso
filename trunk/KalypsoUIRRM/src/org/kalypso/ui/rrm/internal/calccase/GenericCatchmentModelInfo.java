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

import javax.xml.namespace.QName;

import org.joda.time.LocalTime;
import org.joda.time.Period;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ogc.sensor.DateRange;

/**
 * The catchment model info object can be used to execute {@link AbstractCatchmentModelRunner}'s.
 * 
 * @author Holger Albert
 */
public class GenericCatchmentModelInfo implements ICatchmentModelInfo
{
  /**
   * The simulation.
   */
  private final RrmSimulation m_simulation;

  /**
   * The na control.
   */
  private final NAControl m_control;

  /**
   * The na model.
   */
  private final NaModell m_model;

  /**
   * The generator.
   */
  private final IRainfallGenerator m_generator;

  /**
   * The target link.
   */
  private final QName m_targetLink;

  /**
   * The parameter type.
   */
  private final String m_parameterType;

  /**
   * The timestep.
   */
  private final Period m_timestep;

  /**
   * The timestamp.
   */
  private final LocalTime m_timestamp;

  /**
   * The simulation range.
   */
  private final DateRange m_simulationRange;

  /**
   * The unadjusted simulation range.
   */
  private final DateRange m_unadjustedSimulationRange;

  /**
   * The constructor.
   * 
   * @param simulation
   *          The simulation.
   * @param control
   *          The na control.
   * @param model
   *          The na model.
   * @param generator
   *          The generator.
   * @param targetLink
   *          The target link.
   * @param parameterType
   *          The parameter type.
   * @param timestep
   *          The timestep.
   * @param timestamp
   *          The timestamp.
   * @param simulationRange
   *          The simulation range.
   * @param unadjustedSimulationRange
   *          The unadjusted simulation range.
   */
  public GenericCatchmentModelInfo( final RrmSimulation simulation, final NAControl control, final NaModell model, final IRainfallGenerator generator, final QName targetLink, final String parameterType, final Period timestep, final LocalTime timestamp, final DateRange simulationRange, final DateRange unadjustedSimulationRange )
  {
    m_simulation = simulation;
    m_control = control;
    m_model = model;
    m_generator = generator;
    m_targetLink = targetLink;
    m_parameterType = parameterType;
    m_timestep = timestep;
    m_timestamp = timestamp;
    m_simulationRange = simulationRange;
    m_unadjustedSimulationRange = unadjustedSimulationRange;
  }

  @Override
  public RrmSimulation getSimulation( )
  {
    return m_simulation;
  }

  @Override
  public NAControl getControl( )
  {
    return m_control;
  }

  @Override
  public NaModell getModel( )
  {
    return m_model;
  }

  @Override
  public IRainfallGenerator getGenerator( )
  {
    return m_generator;
  }

  @Override
  public QName getTargetLink( )
  {
    return m_targetLink;
  }

  @Override
  public String getParameterType( )
  {
    return m_parameterType;
  }

  @Override
  public Period getTimestep( )
  {
    return m_timestep;
  }

  @Override
  public LocalTime getTimestamp( )
  {
    return m_timestamp;
  }

  @Override
  public DateRange getSimulationRange( )
  {
    return m_simulationRange;
  }

  @Override
  public DateRange getUnadjustedSimulationRange( )
  {
    return m_unadjustedSimulationRange;
  }
}