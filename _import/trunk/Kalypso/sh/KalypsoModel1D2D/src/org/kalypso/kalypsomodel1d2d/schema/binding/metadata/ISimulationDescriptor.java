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
package org.kalypso.kalypsomodel1d2d.schema.binding.metadata;

import java.util.GregorianCalendar;

import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

/**
 * @author Patrice Congo
 * @author Dejan Antanaskovic
 * 
 */
public interface ISimulationDescriptor extends IFeatureWrapper2
{
  enum SIMULATIONTYPE
  {
    Steady,
    Qsteady,
    Unsteady
  }

  public String getScenarioName( );

  public void setScenarioName( String scenarioName );

  public boolean isRestarted( );

  public void setRestarted( boolean value );

  public boolean isAutoconverged( );

  public void setAutoconverged( boolean value );

  public SIMULATIONTYPE getSimulationType( );

  public void setSimulationType( SIMULATIONTYPE value );

  public GregorianCalendar getStartTime( );

  public void setStartTime( GregorianCalendar value );

  public GregorianCalendar getEndTime( );

  public void setEndTime( GregorianCalendar value );

  /**
   * To get the descriptor of the control model used in the simulation being described
   * 
   * @param an
   *            {@link IModelDescriptor} which describe the control model used in the simulation being described
   */
  public IModelDescriptor getControlModel( );

  /**
   * Sets the descriptor of the control model used for the simulation being described by this descriptor
   * 
   * @param modelDescriptor
   *            the model descriptor simulation control model
   * 
   */
  public void setControlModel( IModelDescriptor modelDescriptor );

  /**
   * To get the description of the calculation unit the describe simulation is for
   * 
   * @return {@link IModelDescriptor} of the calculation unit this
   * 
   * 
   */
  public IModelDescriptor getCalculationUnit( );

  /**
   * Sets the descriptor of the calculation unit model simulated by the simulation being described by this descriptor
   * 
   * @param modelDescriptor
   *            the model descriptor foe the simulated calculation unit
   * 
   */
  public void setCalculationUnit( IModelDescriptor modelDescriptor );

  /**
   * To get the list of descriptor for the result of the simulation being described
   * 
   * @return a {@link org.kalypso.kalypsosimulationmodel.core.FeatureWrapperCollection} containing the list of
   *         descriptors for the result of the simualtion being described.
   */
  public IFeatureWrapperCollection<IResultModelDescriptor> getResultModel( );

}
