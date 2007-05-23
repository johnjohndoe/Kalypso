/**
 * 
 */
package org.kalypso.kalypsomodel1d2d.schema.binding;

import java.util.Collections;
import java.util.Hashtable;
import java.util.Map;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IAdapterFactory;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.BoundaryLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.CalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.CalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.CalculationUnit2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.EdgeInv;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.Element1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DContinuityLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DDiscretisationModel;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FEEdgeToCLineJunction1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FEEdgeToEdgeJunction1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FEJunction1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FEMiddleNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IBoundaryLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IEdgeInv;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DContinuityLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEEdgeToCLineJunction1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEEdgeToEdgeJunction1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEJunction1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEMiddleNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IJunctionContext1DTo2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IJunctionContext1DToCLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IRiverChannel1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.JunctionContext1DTo2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.JunctionContext1DToCLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.PolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.RiverChannel1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.BoundaryCondition;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IKingFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.KingFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.TeschkeFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.ControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IStaticModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.SimulationModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.StaticModel1D2D;
import org.kalypso.kalypsomodel1d2d.ui.map.merge.FERoughnessDisplayElement;
import org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz.SurfacePatchVisitableDisplayElement;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.modeling.ISimulationModel;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypsodeegree.graphics.displayelements.DisplayElementDecorator;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Adapter Factory for feature in the simBase namespace
 * 
 * @author Patrice Congo
 *
 */
@SuppressWarnings("unchecked")
public class KalypsoModel1D2DFeatureFactory implements IAdapterFactory
{
    private static final Logger logger = 
                        Logger.getLogger( 
                            KalypsoModel1D2DFeatureFactory.class.toString() );
    
    public static final void warnUnableToAdapt( 
                                Feature featureToAdapt, 
                                QName featureQName,
                                Class targetClass )
    {
      final String id = (featureToAdapt==null)?null:featureToAdapt.getId();
      String msg =
        String.format( 
            "Unable to provide log for:"+
              "\n\tfeatureType=%S"+
              "\n\tfeatureID=%s"+
              "\n\ttargetClass=%s", 
              featureQName,
              id,
              targetClass );
      logger.warning( msg  );
    }
    
	interface AdapterConstructor
	{
		/**
		 * Construct the Adapter of the specified class for the
		 * given feature
		 * 
		 * @param <T>
		 * @param feature
		 * @param cls
		 * @return
		 * @throws IllegalArgumentException if
		 * 	<ul>
		 * 		<li/>feature or cls is null
		 * 		<li/>feature cannnot be converted 
		 *  </ul>
		 */
		public Object constructAdapter(
							Feature feature,
							Class cls) 
							throws IllegalArgumentException;
	}
	
		
	private Map<Class, AdapterConstructor> constructors= 
											createConstructorMap();
	
	
	
	public KalypsoModel1D2DFeatureFactory()
	{
		//Empty
	}
	
	
	/**
	 * @see org.eclipse.core.runtime.IAdapterFactory#getAdapter(java.lang.Object, java.lang.Class)
	 */
	public Object getAdapter(Object adaptableObject, Class adapterType)
	{
		if(!(adaptableObject instanceof Feature))
		{
			throw new IllegalArgumentException(
					"Adapter Factory for feature only but"+
					" get to adapt:"+adaptableObject);
		}
		
		
		AdapterConstructor ctor=constructors.get(adapterType);
		if(ctor!=null)
		{
			return ctor.constructAdapter(
							(Feature)adaptableObject, 
							adapterType);
		}
		return null;
	}

	/**
	 * @see org.eclipse.core.runtime.IAdapterFactory#getAdapterList()
	 */
	public Class[] getAdapterList()
	{
      return constructors.keySet().toArray( new Class[constructors.size()] );
	}

	private final Map<Class, AdapterConstructor> createConstructorMap()
	{
		Map<Class, AdapterConstructor> cMap= 
				new Hashtable<Class, AdapterConstructor>();
		
		
		AdapterConstructor cTor;
		
        //IFE1D2DNode
		cTor = new AdapterConstructor()
		{
			public Object constructAdapter(
										Feature feature, 
										Class cls) 
										throws IllegalArgumentException
			{
                IFeatureType featureType = feature.getFeatureType();
				if(Kalypso1D2DSchemaConstants.WB1D2D_F_MIDDLE_NODE.equals( featureType.getQName() ))
                {
                  return new FEMiddleNode(feature);
                }
                else if(Kalypso1D2DSchemaConstants.WB1D2D_F_NODE.equals( featureType.getQName() ))
                {
                  return new FE1D2DNode(feature);
                }
                else
                {
                  return null;
                }
			}
		};
		cMap.put(IFE1D2DNode.class, cTor);
        cMap.put(IFEMiddleNode.class, cTor);
		        
        //IFE1D2DEdge
        cTor = new AdapterConstructor()
        {
            public Object constructAdapter(
                                        Feature feature, 
                                        Class cls) 
                                        throws IllegalArgumentException
            {
              QName featureQName = feature.getFeatureType().getQName();
              
              if(featureQName.equals( 
                  Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE_INV) )
              {
                return new EdgeInv(feature);
              }
              else if( featureQName.equals( 
                          Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE ) )
              {
                return new FE1D2DEdge(feature);
              }
              else
              {
                warnUnableToAdapt( feature, featureQName , IFE1D2DEdge.class );
                return null;
              }
            }
        };
        cMap.put(IFE1D2DEdge.class, cTor);
        cMap.put(IEdgeInv.class, cTor);
        
        //1d2d element
        // registered for IFE1D2DElement.class but generates the most specific type
        cTor = new AdapterConstructor()
        {
            
            public Object constructAdapter(
                                        Feature feature, 
                                        Class cls) 
                                        throws IllegalArgumentException
            {
              QName featureQName=feature.getFeatureType().getQName();
              
               if(featureQName.equals( Kalypso1D2DSchemaConstants.WB1D2D_F_POLY_ELEMENT ))
                  return new PolyElement(feature);

               if(featureQName.equals( 
                   Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT1D) )
               {
                 return new Element1D(feature);
               }
               else if(featureQName.equals( 
                   Kalypso1D2DSchemaConstants.WB1D2D_F_JUNCTION1D2D) )
               {
                 return new FEJunction1D2D(feature);
               }
               else if(featureQName.equals( 
                   Kalypso1D2DSchemaConstants.WB1D2D_F_JUNCTION1D2D_EDGE_EDGE) )
               {
                 return new FEEdgeToEdgeJunction1D2D(feature);
               }
               else if( featureQName.equals( 
                   Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2DContinuityLine) )
               {
                 return new FE1D2DContinuityLine(feature);
               }
               else if( featureQName.equals( 
                    Kalypso1D2DSchemaConstants.WB1D2D_F_BOUNDARY_LINE ))
               {
                 return new BoundaryLine( feature ); 
               }
               else
               {
                 warnUnableToAdapt( feature, featureQName, IFE1D2DElement.class );
                 return null;
               }
            }
        };
        // REMARK: do NOT register for the other classes as well
        // it is better to register them separate in order to make sure
        // that only the specific types are generated and null is returned if the types
        // does not fit (this is according to the adapter-contract)
        cMap.put( IFE1D2DElement.class, cTor );
        cMap.put( IBoundaryLine.class, cTor );
        
        // PolyElement
        cTor = new AdapterConstructor()
        {
            public Object constructAdapter(
                                        Feature feature, 
                                        Class cls) 
                                        throws IllegalArgumentException
            {
              QName featureQName=feature.getFeatureType().getQName();
              
               if(featureQName.equals( Kalypso1D2DSchemaConstants.WB1D2D_F_POLY_ELEMENT ))
                {
                  return new PolyElement(feature);
                }
                else
                {
                  warnUnableToAdapt( feature, featureQName, IPolyElement.class );
                  return null;
                }
            }
        };
        cMap.put(IPolyElement.class, cTor);
        
        //1d2d 1d-element
        cTor = new AdapterConstructor()
        {
          public Object constructAdapter(
              Feature feature, 
              Class cls) 
          throws IllegalArgumentException
          {
            QName featureQName=feature.getFeatureType().getQName();
            
            if(featureQName.equals( 
                Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT1D) )
            {
              return new Element1D(feature);     
            }
            else
            {
              warnUnableToAdapt( feature, featureQName, IElement1D.class );
              return null;
            }
          }
        };
        cMap.put( IElement1D.class, cTor );
        
        //1d2d conti-line-element
        cTor = new AdapterConstructor()
        {
          public Object constructAdapter(
              Feature feature, 
              Class cls) 
          throws IllegalArgumentException
          {
            QName featureQName=feature.getFeatureType().getQName();
            
            if(featureQName.equals( 
                Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2DContinuityLine) )
            {
              return new FE1D2DContinuityLine(feature);     
            }
            else
            {
              warnUnableToAdapt( feature, featureQName, IFE1D2DContinuityLine.class );
              return null;
            }
          }
        };
        cMap.put( IFE1D2DContinuityLine.class, cTor);
        
        //1d2d IFEJunction1D2D-element
        cTor = new AdapterConstructor()
        {
          public Object constructAdapter(
              Feature feature, 
              Class cls) 
          throws IllegalArgumentException
          {
            QName featureQName=feature.getFeatureType().getQName();
            
            if(featureQName.equals( 
                Kalypso1D2DSchemaConstants.WB1D2D_F_JUNCTION1D2D) )
            {
              return new FEJunction1D2D(feature);     
            }
            else
            {
              warnUnableToAdapt( feature, featureQName, IFEJunction1D2D.class);
              return null;
            }
          }
        };
        cMap.put( IFEJunction1D2D.class, cTor);
        
        //1d2d IFEEdgeToEdgeJunction1D2D
        cTor = new AdapterConstructor()
        {
          public Object constructAdapter(
              Feature feature, 
              Class cls) 
          throws IllegalArgumentException
          {
            QName featureQName=feature.getFeatureType().getQName();
            
            if(featureQName.equals( 
                Kalypso1D2DSchemaConstants.WB1D2D_F_JUNCTION1D2D_EDGE_EDGE) )
            {
              return new FEEdgeToEdgeJunction1D2D(feature);     
            }
            else
            {
              warnUnableToAdapt( feature, featureQName, IFEEdgeToEdgeJunction1D2D.class );
              return null;
            }
          }
        };
        cMap.put( IFEEdgeToEdgeJunction1D2D.class,cTor);
        
        //1d2d IFEEdgeToCLineJunction1D2D
        cTor = new AdapterConstructor()
        {
          public Object constructAdapter(
              Feature feature, 
              Class cls) 
          throws IllegalArgumentException
          {
            QName featureQName=feature.getFeatureType().getQName();
            
            if(featureQName.equals( 
                Kalypso1D2DSchemaConstants.WB1D2D_F_JUNCTION1D2D_EDGE_CLINE) )
            {
              return new FEEdgeToCLineJunction1D2D(feature);     
            }
            else
            {
              warnUnableToAdapt( 
                  feature, featureQName, IFEEdgeToCLineJunction1D2D.class );
              return null;
            }
          }
        };
        cMap.put( IFEEdgeToCLineJunction1D2D.class, cTor );
        
        //1d2d complex element
        cTor = new AdapterConstructor()
        {
            public Object constructAdapter(
                                        Feature feature, 
                                        Class cls) 
                                        throws IllegalArgumentException
            {
              QName featureQName=feature.getFeatureType().getQName();
              
                if(featureQName.equals( 
                    Kalypso1D2DSchemaConstants.WB1D2D_F_COMPLEX_ELE_2D) )
                {
                  return 
                    new FE1D2DComplexElement<IFE1D2DElement>(
                        feature,
                        Kalypso1D2DSchemaConstants.WB1D2D_F_COMPLEX_ELE_2D,
                        Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELE_2D,//elements
                        IFE1D2DElement.class);     
                }
                else if(featureQName.equals( 
                    Kalypso1D2DSchemaConstants.WB1D2D_F_JUNTCION_CONTEXT_1D_2D) )
                {
                  return new JunctionContext1DTo2D(feature);
                }
                else if(featureQName.equals( 
                      Kalypso1D2DSchemaConstants.WB1D2D_F_JUNTCION_CONTEXT_1D_CLINE) )
                {
                  return new JunctionContext1DToCLine(feature);
                }
                else if( featureQName.equals( 
                      Kalypso1D2DSchemaConstants.WB1D2D_F_CALC_UNIT_1D ))
                {
                  return new CalculationUnit1D( feature );
                }
                else if( featureQName.equals( 
                    Kalypso1D2DSchemaConstants.WB1D2D_F_CALC_UNIT_2D ))
                {
                  return new CalculationUnit2D( feature );
                }
                else if( featureQName.equals( 
                    Kalypso1D2DSchemaConstants.WB1D2D_F_CALC_UNIT_1D2D ))
                {
                    return new CalculationUnit1D2D( feature );
                }
                else
                {
                  warnUnableToAdapt( feature, featureQName, IFE1D2DComplexElement.class );
                  return null;
                }
            }
        };
        cMap.put( IFE1D2DComplexElement.class, cTor );
        cMap.put( IJunctionContext1DTo2D.class, cTor );
        cMap.put( IJunctionContext1DToCLine.class, cTor );
        cMap.put( ICalculationUnit.class, cTor );
        cMap.put( ICalculationUnit1D.class, cTor );
        cMap.put( ICalculationUnit2D.class, cTor );
 
        // RiverChannel1D
        cTor = new AdapterConstructor()
        {
            public Object constructAdapter(
                                        Feature feature, 
                                        Class cls) 
                                        throws IllegalArgumentException
            {
               return new RiverChannel1D( feature );     
            }
        };
        cMap.put(IRiverChannel1D.class, cTor);
        
////      JunctionContext
//        cTor = new AdapterConstructor()
//        {
//            public Object constructAdapter(
//                                        Feature feature, 
//                                        Class cls) 
//                                        throws IllegalArgumentException
//            {
//              QName featureQName = feature.getFeatureType().getQName();
//              if(featureQName.equals( 
//                    Kalypso1D2DSchemaConstants.WB1D2D_F_JUNTCION_CONTEXT_1D_2D) )
//              {
//                return new JunctionContext1DTo2D(feature);
//              }
//              else if(featureQName.equals( 
//                    Kalypso1D2DSchemaConstants.WB1D2D_F_JUNTCION_CONTEXT_1D_CLINE) )
//              {
//                return new JunctionContext1DToCLine(feature);
//              }
//              else
//              {
//                return null; 
//              }
//            }
//        };
//        cMap.put(IJunctionContext1DTo2D.class, cTor);
//        cMap.put( IJunctionContext1DToCLine.class, cTor );
        
 
        
        // DiscretisationModel  
        cTor = new AdapterConstructor()
        {
            public Object constructAdapter(
                                        Feature feature, 
                                        Class cls) 
                                        throws IllegalArgumentException
            {
              QName featureQName=feature.getFeatureType().getQName();
              
                if(featureQName.equals( 
                    Kalypso1D2DSchemaConstants.WB1D2D_F_DiscretisationModel) )
                {
                  return new FE1D2DDiscretisationModel(feature);     
                }
                else
                {
                  warnUnableToAdapt( 
                      feature, featureQName, IFEDiscretisationModel1d2d.class );
                  return null;
                }
            }
        };
        cMap.put( IFEDiscretisationModel1d2d.class, cTor);

//      StaticModel1D2D  
        cTor = new AdapterConstructor()
        {
            public Object constructAdapter(
                                        Feature feature, 
                                        Class cls) 
                                        throws IllegalArgumentException
            {
              QName featureQName=feature.getFeatureType().getQName();
              
                if(featureQName.equals( 
                    Kalypso1D2DSchemaConstants.WB1D2D_F_STATIC_MODEL ) )
                {
                  return new StaticModel1D2D(feature);     
                }
                else
                {
                  warnUnableToAdapt( 
                      feature, featureQName, IStaticModel1D2D.class );
                  return null;
                }
            }
        };
        cMap.put( IStaticModel1D2D.class, cTor );
        
//      ControlModel  
        cTor = new AdapterConstructor()
        {
            public Object constructAdapter(
                                        Feature feature, 
                                        Class cls) 
                                        throws IllegalArgumentException
            {
              QName featureQName=feature.getFeatureType().getQName();
              
                if(featureQName.equals( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_F_MODEL ) )
                {
                  return new ControlModel1D2D(feature);     
                }
                else
                {
                  warnUnableToAdapt( feature, featureQName, IControlModel1D2D.class );
                  return null;
                }
            }
        };
        cMap.put( IControlModel1D2D.class, cTor );
//      SimulationModel  
        cTor = new AdapterConstructor()
        {
            public Object constructAdapter(
                                        Feature feature, 
                                        Class cls) 
                                        throws IllegalArgumentException
            {
              QName featureQName=feature.getFeatureType().getQName();
              
              if(featureQName.equals( 
                  KalypsoModelSimulationBaseConsts.SIM_BASE_F_SIMULATION_MODEL ) )
              {
                return new SimulationModel1D2D( feature );     
              }
              else
              {
                warnUnableToAdapt( feature, featureQName, ISimulationModel.class );
                return null;
              }
            }
        };
        cMap.put( ISimulationModel.class, cTor);
        
//      IDisplayElement
        cTor= new AdapterConstructor()
        {
          public Object constructAdapter(
              Feature feature, 
              Class cls) 
          throws IllegalArgumentException
          {
            QName name = feature.getFeatureType().getQName();
            if(KalypsoModelSimulationBaseConsts.SIM_BASE_F_NATIVE_TERRAIN_ELE_WRAPPER.equals( name ))
            {
              return SurfacePatchVisitableDisplayElement.createDisplayElement( feature );
            }
            else if(Kalypso1D2DSchemaConstants.WB1D2D_F_STATIC_MODEL.equals( name ))
            {
              return FERoughnessDisplayElement.createDisplayElement( feature );
            }
            else
            {
              warnUnableToAdapt( feature, name, DisplayElementDecorator.class );
              return null;
            }
          }
        };
        cMap.put( DisplayElementDecorator.class, cTor);

        // IFlowRelation  
        cTor = new AdapterConstructor()
        {
            public Object constructAdapter(
                                        Feature feature, 
                                        Class cls) 
                                        throws IllegalArgumentException
            {
              // If a generel flow relation is to be adapted, return the konkrete type instead
              QName featureQName=feature.getFeatureType().getQName();
              
                if(featureQName.equals( 
                    IKingFlowRelation.QNAME) )
                {
                  return new KingFlowRelation(feature);
                }
                else if(featureQName.equals( 
                    ITeschkeFlowRelation.QNAME) )
                {
                  return new TeschkeFlowRelation(feature);
                }
                else if(featureQName.equals( 
                    IBoundaryCondition.QNAME) )
                {
                  return new BoundaryCondition(feature);
                }
                else
                {
                  warnUnableToAdapt( feature, featureQName, IFlowRelationship.class );
                  return null;
                }
            }
        };
        cMap.put( IFlowRelationship.class, cTor);

        
        // KingFlowRelation  
        cTor = new AdapterConstructor()
        {
            public Object constructAdapter(
                                        Feature feature, 
                                        Class cls) 
                                        throws IllegalArgumentException
            {
              QName featureQName=feature.getFeatureType().getQName();
              
                if(featureQName.equals( 
                    IKingFlowRelation.QNAME) )
                {
                  return new KingFlowRelation(feature);
                }
                else
                {                
                  warnUnableToAdapt( 
                      feature, featureQName,IKingFlowRelation.class );
                  return null;
                }
            }
        };
        cMap.put( IKingFlowRelation.class, cTor);
        
        // TeschkeFlowRelation  
        cTor = new AdapterConstructor()
        {
          public Object constructAdapter(
              Feature feature, 
              Class cls) 
          throws IllegalArgumentException
          {
            QName featureQName=feature.getFeatureType().getQName();
            
            if(featureQName.equals( 
                ITeschkeFlowRelation.QNAME) )
            {
              return new TeschkeFlowRelation(feature);
            }
            else
            {
              warnUnableToAdapt( feature, featureQName, ITeschkeFlowRelation.class );
              return null;
            }
          }
        };
        cMap.put( ITeschkeFlowRelation.class, cTor);
        
        // BoundaraCondition  
        cTor = new AdapterConstructor()
        {
          public Object constructAdapter(
              Feature feature, 
              Class cls) 
          throws IllegalArgumentException
          {
            QName featureQName=feature.getFeatureType().getQName();
            
            if(featureQName.equals( 
                IBoundaryCondition.QNAME) )
            {
              return new BoundaryCondition(feature);
            }
            else
            {
              warnUnableToAdapt( 
                  feature, featureQName, IBoundaryCondition.class );
              return null;
            }
          }
        };
        cMap.put( IBoundaryCondition.class, cTor);
        
		return Collections.unmodifiableMap(cMap);
	}
    
    
}
