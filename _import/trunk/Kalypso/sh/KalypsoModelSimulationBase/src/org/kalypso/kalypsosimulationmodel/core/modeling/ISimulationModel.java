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
package org.kalypso.kalypsosimulationmodel.core.modeling;

import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

/**
 * Interface for classes represeting a simBase:SimulationModel
 *  
 * @author Patrice Congo
 */
public interface ISimulationModel<
                    StaticM extends IStaticModel,
                    OpM extends IOperationalModel,
                    CntlM extends IControlModel,
                    ResM extends IResultModel,
                    EvalM extends IEvaluationModel,
                    AdmP extends IFeatureWrapper2,
                    OverM extends IFeatureWrapper2>
                    extends IModel
{
  /**
   * To get the static model of this simulation model
   */
  public StaticM getStaticModel();
  
  /**
   * To get the operational model of this simulation model
   * @return the operational model of this simulation model 
   */
  public OpM  getOperationalModel();
  
  /**
   * To get the control model of this simulation model
   * @return the control model of this simulation model
   */
  public CntlM getControlModel();
  
  /**
   * To get the evaluation model of this simulation model
   * @return the evaluation model of this simulation model
   */
  public EvalM getEvaluationModel();
  
  /**
   * To get the addidtional properties assosiated with this 
   * simulation model
   * @return the addidtional properties assosiated with this 
   *            simulation model 
   */
  public IFeatureWrapperCollection<AdmP> getAdditionalModelProps();
  
  /**
   * To get general overriding model associated with this
   * simulation model
   * @return general overriding model associated with this
   *            simulation model
   */
  public IFeatureWrapperCollection<OverM> getOverridingModel();
}
