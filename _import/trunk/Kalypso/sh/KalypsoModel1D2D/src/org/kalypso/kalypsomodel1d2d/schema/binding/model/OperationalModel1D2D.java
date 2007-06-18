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
package org.kalypso.kalypsomodel1d2d.schema.binding.model;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.Util;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.FeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.flowresistance.IFlowResistanceConcept;
import org.kalypso.kalypsosimulationmodel.core.flowresistance.IFlowResistanceModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Primitive;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

/**
 * Default implementation of {@link IOperationalModel1D2D} based
 * on {@link AbstractFeatureBinder}
 * 
 * @author Patrice Congo
 *
 */
public class OperationalModel1D2D extends AbstractFeatureBinder implements IOperationalModel1D2D
{

  final private IFeatureWrapperCollection<IBoundaryCondition> boundaryConditions;
  
  public OperationalModel1D2D( Feature featureToBind)
  {
   this( 
       featureToBind, 
       Kalypso1D2DSchemaConstants.OP1D2D_F_OPERATIONAL_MODEL ); 
  }
  
  protected OperationalModel1D2D( Feature featureToBind, QName qnameToBind )
  {
    super( featureToBind, qnameToBind );
    boundaryConditions = 
      new FeatureWrapperCollection<IBoundaryCondition>( 
                featureToBind, 
                IBoundaryCondition.class, 
                Kalypso1D2DSchemaConstants.OP1D2D_PROP_BOUNDARY_CONDITION );
}

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IOperationalModel1D2D#getApplicableBoundaryConditions(org.kalypsodeegree.model.geometry.GM_Primitive)
   */
  public List<IBoundaryCondition> getApplicableBoundaryConditions( GM_Primitive zone )
  {
    
    return null;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IOperationalModel1D2D#getBoundaryConditions()
   */
  public IFeatureWrapperCollection<IBoundaryCondition> getBoundaryConditions( )
  {
    return boundaryConditions;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IOperationalModel1D2D#getInitialConditions()
   */
  public IFeatureWrapperCollection<IFeatureWrapper2> getInitialConditions( )
  {
    return null;
  }

//  public static final <T>List<T> findApplicableProperty( 
//                                  IFeatureWrapperCollection<T> fwCollection,
//                                  GM_Primitive selectionZone )
//  {
//    Assert.throwIAEOnNullParam( selectionZone, "selectionZone" );
//    
//    GMLWorkspace workspace = fwCollection.getWrappedFeature().getWorkspace();
//    FeatureList wrappedList = fwCollection.getWrappedList();
//    if(selectionZone instanceof GM_Point)
//    {
//      List<IFlowResistanceConcept> result =  new ArrayList<IFlowResistanceConcept>();
//      List linkOrFeatures = wrappedList.query( ( ( GM_Point ) selectionZone ).getPosition(), null );
//      IFlowResistanceConcept concept;
//      for( Object linkOrFeature : linkOrFeatures )
//      {
//        Feature feature = FeatureHelper.getFeature( workspace, linkOrFeature );
//        if(feature!=null)
//        {
//          concept = ( IFlowResistanceConcept ) 
//                        feature.getAdapter( IFlowResistanceModel.class );
//          if( concept != null )
//          {
//            GM_Primitive applicationZone = concept.getApplicationZone();
//            if(applicationZone.contains( selectionZone ))
//            {
//              result.add( concept );
//            }
//          }
//        }
//        else
//        {
//          logger.info( "Could not found feature: link or feature="+feature );
//        }
//      }
//      return result;
//    }
//    else if(selectionZone instanceof GM_Surface)
//    {
//      List<IFlowResistanceConcept> result =  new ArrayList<IFlowResistanceConcept>();
//      List linkOrFeatures = wrappedList.query( ( ( GM_Surface ) selectionZone ).getEnvelope(), null );
//      IFlowResistanceConcept concept;
//      for( Object linkOrFeature : linkOrFeatures )
//      {
//        Feature feature = FeatureHelper.getFeature( workspace, linkOrFeature );
//        if(feature!=null)
//        {
//          concept = ( IFlowResistanceConcept ) 
//                        feature.getAdapter( IFlowResistanceModel.class );
//          if( concept != null )
//          {
//            GM_Primitive applicationZone = concept.getApplicationZone();
//            if( selectionZone.contains( applicationZone ) )
//            {
//              result.add( concept );
//            }
//          }
//        }
//        else
//        {
//          logger.info( "Could not found feature: link or feature="+feature );
//        }
//      }
//      return result;
//    }
//    else
//    {
//      throw new IllegalArgumentException(
//                  "Supports only point und surface zone:"+
//                  "\n\tcurrent zone type = "+selectionZone.getClass()+
//                  "\n\tcurrent zone value = "+selectionZone);
//    }
//  }
  
}
