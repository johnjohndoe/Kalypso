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
package org.kalypso.model.wspm.sobek.core.interfaces;

import java.util.GregorianCalendar;

import javax.xml.namespace.QName;

import org.kalypsodeegree.model.feature.Feature;

/**
 * @author kuch
 */
public interface ILastfall
{
  public static final QName QN_NAME = new QName( ISobekConstants.NS_SOBEK, "name" );//$NON-NLS-1$

  public static final QName QN_SIMULATION_BEGIN = new QName( ISobekConstants.NS_SOBEK, "simulationBegin" );//$NON-NLS-1$

  public static final QName QN_SIMULATION_END = new QName( ISobekConstants.NS_SOBEK, "simulationEnd" );//$NON-NLS-1$

  public static final QName QN_SIMULATION_PRE_TIME = new QName( ISobekConstants.NS_SOBEK, "preSimulationTime" );//$NON-NLS-1$

  public static final QName QN_SIMULATION_TIMESTEP = new QName( ISobekConstants.NS_SOBEK, "simulationTimestep" );//$NON-NLS-1$

  public static final QName QN_SIMULATION_TIMESTEP_MULTIPLIER = new QName( ISobekConstants.NS_SOBEK, "resultTimeStepAsMultiple" );//$NON-NLS-1$

  public Feature getFeature( );

  public GregorianCalendar getLastfallEnd( );

  public GregorianCalendar getLastfallStart( );

  public IModelMember getModelMember( );

  public String getName( );

  public Integer getPreSimulationTime( );

  public Integer getSimulationTimeStep( );

  public Integer getTimeStepMultiplier( );

  public String getValidatedLastfallDir( );

}
