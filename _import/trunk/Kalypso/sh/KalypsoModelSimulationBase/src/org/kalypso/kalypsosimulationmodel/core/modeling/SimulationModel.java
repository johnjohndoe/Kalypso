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

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.FeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

/**
 * Default, {@link AbstractFeatureBinder} based implementation of {@link ISimulationModel}
 * 
 * @author Patrice Congo
 *
 */
public class SimulationModel<
                StaticM extends IStaticModel,
                OpM extends IOperationalModel,
                CntlM extends IControlModel,
                ResM extends IResultModel,
                EvalM extends IEvaluationModel,
                AdmP extends IFeatureWrapper2,
                OverM extends IFeatureWrapper2>
                extends AbstractFeatureBinder
                implements ISimulationModel< 
                                          StaticM, OpM, 
                                          CntlM, ResM, 
                                          EvalM, AdmP, OverM>
                
{
  
  
  final private IFeatureWrapperCollection<AdmP> additionalProps;
  
  final private IFeatureWrapperCollection<OverM> overridingModels;

  final private Class<StaticM> staticModelCls;

  final private Class<OpM> operationalModelCls;

  final private Class<CntlM> controlModelCls;

  final private Class<ResM> resultModelCls;

  final private Class<EvalM> evalModelCls;

  final private Class<AdmP> additionalModelPropsCls;

  final private Class<OverM> overridingModelCls;
  
  /**
   * Creates Simulation model wrapping the specified feature and 
   * holding sumodel of the specified type
   * @param featureToBind the simulation model feature to bind
   * @param qnameToBind the q-name of the simulation feature to bind
   * @param staticModelCls the wrapper class type for the static submodel
   * @param operationalModelCls the wrapper class type for the operational submodel
   * @param controlModelCls the wrapper class type for the control sub model
   * @param resultModelCls the wrapper class type for the result sub model
   * @param evalModelCls the wrapper class for the result sub model
   * @param additionalModelPropsCls the wrapper class for additional model properties
   * @param overridingModelCls the wrapper class for overriding model
   * 
   */
  public SimulationModel(
              Feature featureToBind,
              QName qnameToBind,
              Class<StaticM> staticModelCls, 
              Class<OpM> operationalModelCls, 
              Class<CntlM> controlModelCls, 
              Class<ResM> resultModelCls, 
              Class<EvalM> evalModelCls, 
              Class<AdmP> additionalModelPropsCls, 
              Class<OverM> overridingModelCls)
  {
    super( featureToBind, qnameToBind );
    this.staticModelCls = staticModelCls;
    this.operationalModelCls = operationalModelCls;
    this.controlModelCls = controlModelCls;
    this.resultModelCls = resultModelCls;
    this.evalModelCls = evalModelCls;
    this.additionalModelPropsCls = additionalModelPropsCls;
    this.overridingModelCls = overridingModelCls;
    
    additionalProps = 
        new FeatureWrapperCollection<AdmP>(
                featureToBind,
                additionalModelPropsCls,
                KalypsoModelSimulationBaseConsts.SIM_BASE_P_ADDITIONAL_MODEL_PROPS );
    
    overridingModels = 
      new FeatureWrapperCollection<OverM>(
              featureToBind,
              overridingModelCls,
              KalypsoModelSimulationBaseConsts.SIM_BASE_P_OVERRIDING_MODEL );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.modeling.ISimulationModel#getAdditionalModelProps()
   */
  public IFeatureWrapperCollection<AdmP> getAdditionalModelProps( )
  {
    
    return additionalProps;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.modeling.ISimulationModel#getControlModel()
   */
  public CntlM getControlModel( )
  {
    
    final CntlM controlModel =
      getSubModel( 
        controlModelCls, 
        KalypsoModelSimulationBaseConsts.SIM_BASE_P_CONTROL_MODEL );
    return controlModel;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.modeling.ISimulationModel#getEvaluationModel()
   */
  public EvalM getEvaluationModel( )
  {
    final EvalM evaluationModel = 
      getSubModel( 
        evalModelCls, 
        KalypsoModelSimulationBaseConsts.SIM_BASE_P_EVALUATION_MODEL );
    return evaluationModel;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.modeling.ISimulationModel#getOperationalModel()
   */
  public OpM getOperationalModel( )
  {
    final OpM operationalModel = 
      getSubModel( 
        operationalModelCls, 
        KalypsoModelSimulationBaseConsts.SIM_BASE_P_OPERATIONAL_MODEL );
    return operationalModel;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.modeling.ISimulationModel#getOverridingModel()
   */
  public IFeatureWrapperCollection<OverM> getOverridingModel( )
  {
    return overridingModels;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.modeling.ISimulationModel#getStaticModel()
   */
  public StaticM getStaticModel( )
  {
    final StaticM staticModel=
      getSubModel( 
          staticModelCls, 
          KalypsoModelSimulationBaseConsts.SIM_BASE_P_STATIC_MODEL );
    return staticModel;
  }

  private <T extends IFeatureWrapper2> T getSubModel(
                                          Class<T> adapterTargetClass,
                                          QName propertyQName )
  {
//     final Feature feature = getFeature();
//     final Feature propFeature = FeatureHelper.resolveLink( feature, propertyQName );
//     if( propFeature == null )
//     {
//       return null;
//     }
//     else
//     {
//       T adaptedFeature = (T) propFeature.getAdapter( adapterTargetClass );
//       return adaptedFeature;
//     }
    
    return FeatureHelper.resolveLink( 
              getFeature(), 
              propertyQName, 
              adapterTargetClass );
       
  }
  
}
