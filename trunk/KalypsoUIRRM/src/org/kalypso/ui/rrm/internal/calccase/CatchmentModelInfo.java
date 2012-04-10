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
import org.kalypso.model.rcm.binding.ILinearSumGenerator;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ogc.sensor.DateRange;

/**
 * The catchment model info object can be used to execute {@link AbstractCatchmentModelRunner}'s.
 * 
 * @author Holger Albert
 */
public class CatchmentModelInfo
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
   * The rainfall generator.
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
   * The timestep. If null, it will be calculated.
   */
  private Period m_timestep;

  /**
   * The timestamp. If null, it will be calculated.
   */
  private LocalTime m_timestamp;

  /**
   * The range. If null, it will be calculated.
   */
  private DateRange m_range;

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
   *          The rainfall generator.
   * @param targetLink
   *          The target link.
   * @param parameterType
   *          The parameter type.
   * @param timestep
   *          The timestep. If null, it will be calculated
   * @param timestamp
   *          The timestamp. If null, it will be calculated
   * @param range
   *          The range. If null, it will be calculated
   */
  public CatchmentModelInfo( final RrmSimulation simulation, final NAControl control, final NaModell model, final IRainfallGenerator generator, final QName targetLink, final String parameterType, final Period timestep, final LocalTime timestamp, final DateRange range )
  {
    m_simulation = simulation;
    m_control = control;
    m_model = model;
    m_generator = generator;
    m_targetLink = targetLink;
    m_parameterType = parameterType;
    m_timestep = timestep;
    m_timestamp = timestamp;
    m_range = range;
  }

  public RrmSimulation getSimulation( )
  {
    return m_simulation;
  }

  public NAControl getControl( )
  {
    return m_control;
  }

  public NaModell getModel( )
  {
    return m_model;
  }

  public IRainfallGenerator getGenerator( )
  {
    return m_generator;
  }

  public QName getTargetLink( )
  {
    return m_targetLink;
  }

  public String getParameterType( )
  {
    return m_parameterType;
  }

  public Period getTimestep( )
  {
    if( m_timestep == null )
      m_timestep = calculateTimestep();

    return m_timestep;
  }

  private Period calculateTimestep( )
  {
    final int timestepMinutes = getTimestepMinutes();
    final Period minutes = Period.minutes( timestepMinutes );

    return minutes.normalizedStandard();
  }

  private int getTimestepMinutes( )
  {
    /* Cast. */
    final ILinearSumGenerator linearGenerator = (ILinearSumGenerator) m_generator;

    /* Get the timestep. */
    final Integer timestep = linearGenerator.getTimestep();
    if( timestep == null )
      return m_control.getMinutesOfTimestep();

    return timestep;
  }

  public LocalTime getTimestamp( )
  {
    if( m_timestamp == null )
      m_timestamp = calculateTimestamp();

    return m_timestamp;
  }

  private LocalTime calculateTimestamp( )
  {
    /* Cast. */
    final ILinearSumGenerator linearGenerator = (ILinearSumGenerator) m_generator;

    return linearGenerator.getTimestamp();
  }

  public DateRange getRange( )
  {
    if( m_range == null )
      m_range = calculateRange();

    return m_range;
  }

  private DateRange calculateRange( )
  {
    final NAControl control = m_control;
    final Period timestep = getTimestep();
    final LocalTime timestamp = getTimestamp();

    return CatchmentModelHelper.getRange( control, timestep, timestamp );
  }
}