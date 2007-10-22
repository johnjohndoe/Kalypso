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
package org.kalypso.kalypsosimulationmodel.core.flowresistance;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Primitive;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * {@link AbstractFeatureBinder} based default implementation of 
 * {@link IFlowResistanceModel} 
 * 
 * @author Patrice Congo
 *
 */
public class FlowResistanceModel 
                  extends AbstractFeatureBinder 
                  implements IFlowResistanceModel
{
  
  private static final  Logger logger = Logger.getLogger( FlowResistanceModel.class.toString() );
  
  private IFeatureWrapperCollection<IFlowResistanceConcept> flowResistanceConcepts;

  public FlowResistanceModel(Feature featureToBind, QName qnameToBind)
  {
    super( featureToBind, qnameToBind );
    flowResistanceConcepts = 
        new FeatureWrapperCollection<IFlowResistanceConcept>(
              featureToBind,
              IFlowResistanceConcept.class,
              KalypsoModelSimulationBaseConsts.SIM_BASE_P_FLOW_RESISTANCE_CONCEPT);
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.flowresistance.IFlowResistanceModel#getApplicableFlowResistanceConcepts(org.kalypsodeegree.model.geometry.GM_Primitive)
   */
  public List<IFlowResistanceConcept> getApplicableFlowResistanceConcepts( GM_Primitive zone )
  {
    return null;
  }

  //TODO Patric Congo this is limited to linked feature inside the bindfeature-workspace 
  private List<IFlowResistanceConcept> findApplicableFlowResistanceConcepts( GM_Primitive selectionZone )
  {
    Assert.throwIAEOnNullParam( selectionZone, "selectionZone" );
    
    FeatureList wrappedList = flowResistanceConcepts.getWrappedList();
    GMLWorkspace workspace = getWrappedFeature().getWorkspace();
    
    if(selectionZone instanceof GM_Point)
    {
      List<IFlowResistanceConcept> result =  new ArrayList<IFlowResistanceConcept>();
      List linkOrFeatures = wrappedList.query( ( ( GM_Point ) selectionZone ).getPosition(), null );
      IFlowResistanceConcept concept;
      for( Object linkOrFeature : linkOrFeatures )
      {
        Feature feature = FeatureHelper.getFeature( workspace, linkOrFeature );
        if(feature!=null)
        {
          concept = ( IFlowResistanceConcept ) 
                        feature.getAdapter( IFlowResistanceModel.class );
          if( concept != null )
          {
            GM_Primitive applicationZone = concept.getApplicationZone();
            if(applicationZone.contains( selectionZone ))
            {
              result.add( concept );
            }
          }
        }
        else
        {
          logger.info( "Could not found feature: link or feature="+feature );
        }
      }
      return result;
    }
    else if(selectionZone instanceof GM_Surface)
    {
      List<IFlowResistanceConcept> result =  new ArrayList<IFlowResistanceConcept>();
      List linkOrFeatures = wrappedList.query( ( ( GM_Surface ) selectionZone ).getEnvelope(), null );
      IFlowResistanceConcept concept;
      for( Object linkOrFeature : linkOrFeatures )
      {
        Feature feature = FeatureHelper.getFeature( workspace, linkOrFeature );
        if(feature!=null)
        {
          concept = ( IFlowResistanceConcept ) 
                        feature.getAdapter( IFlowResistanceModel.class );
          if( concept != null )
          {
            GM_Primitive applicationZone = concept.getApplicationZone();
            if( selectionZone.contains( applicationZone ) )
            {
              result.add( concept );
            }
          }
        }
        else
        {
          logger.info( "Could not found feature: link or feature="+feature );
        }
      }
      return result;
    }
    else
    {
      throw new IllegalArgumentException(
                  "Supports only point und surface zone:"+
                  "\n\tcurrent zone type = "+selectionZone.getClass()+
                  "\n\tcurrent zone value = "+selectionZone);
    }
  }
  
  
  
  /**
   * @see org.kalypso.kalypsosimulationmodel.core.flowresistance.IFlowResistanceModel#getFlowResistanceConcepts()
   */
  public IFeatureWrapperCollection<IFlowResistanceConcept> getFlowResistanceConcepts( )
  {
    return flowResistanceConcepts;
  }

}
