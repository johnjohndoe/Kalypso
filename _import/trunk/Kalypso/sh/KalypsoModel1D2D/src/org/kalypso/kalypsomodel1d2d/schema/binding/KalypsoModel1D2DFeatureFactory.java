/**
 * 
 */
package org.kalypso.kalypsomodel1d2d.schema.binding;

import java.util.Collections;
import java.util.Hashtable;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IAdapterFactory;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.BoundaryCondition;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IKingFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.KingFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.TeschkeFlowRelation;
import org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz.SurfacePatchVisitableDisplayElement;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypsodeegree.graphics.displayelements.DisplayElementDecorator;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Adapter Factory for feature in the simBase namespace
 * 
 * @author Patrice Congo
 *
 */
public class KalypsoModel1D2DFeatureFactory implements IAdapterFactory
{
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
              QName featureQName=feature.getFeatureType().getQName();
              
              if(featureQName.equals( 
                  Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE_INV) )
              {
                return new EdgeInv(feature);
              }
              else
              {
                return new FE1D2DEdge(feature);
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
                 return new Element1D(feature);     

               if(featureQName.equals( 
                   Kalypso1D2DSchemaConstants.WB1D2D_F_JUNCTION1D2D) )
                 return new FEJunction1D2D(feature);     

               if(featureQName.equals( 
                   Kalypso1D2DSchemaConstants.WB1D2D_F_JUNCTION1D2D_EDGE_EDGE) )
                 return new FEEdgeToEdgeJunction1D2D(feature);     

               if(featureQName.equals( 
                   Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2DContinuityLine) )
                 return new FE1D2DContinuityLine(feature);     
               
               return null;
            }
        };
        // REMARK: do NOT register for the other classes as well
        // it is better to register them separate in order to make sure
        // that only the specific types are generated and null is returned if the types
        // does not fit (this is according to the adapter-contract)
        cMap.put(IFE1D2DElement.class, cTor);

        
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
              return null;
            }
          }
        };
        cMap.put(IElement1D.class, cTor);
        
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
              return null;
            }
          }
        };
        cMap.put(IFE1D2DContinuityLine.class, cTor);
        
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
              return null;
            }
          }
        };
        cMap.put(IFEJunction1D2D.class, cTor);
        
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
              return null;
            }
          }
        };
        cMap.put(IFEEdgeToEdgeJunction1D2D.class,cTor);
        
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
              return null;
            }
          }
        };
        cMap.put(IFEEdgeToCLineJunction1D2D.class,cTor);
        
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
                    new FE1D2DComplexElement(
                        feature,
                        Kalypso1D2DSchemaConstants.WB1D2D_F_COMPLEX_ELE_2D,
                        null,//Kalypso1D2DSchemaConstants.WB1D2D_PROP_FE1D2D_2D_ELE,//container
                        Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELE_2D//elements
                        );     
                }
                else
                {
                  return null;
                }
            }
        };
        cMap.put(IFE1D2DComplexElement.class, cTor);
 
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
                  return null;
                }
            }
        };
        cMap.put(IFEDiscretisationModel1d2d.class, cTor);
        
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
            else
            {
              return null;
            }
          }
        };
        cMap.put(DisplayElementDecorator.class, cTor);
        
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
                  return new KingFlowRelation(feature);
                
                return null;
            }
        };
        cMap.put(IKingFlowRelation.class, cTor);
        
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
              return new TeschkeFlowRelation(feature);
            
            return null;
          }
        };
        cMap.put(ITeschkeFlowRelation.class, cTor);
        
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
                IBoundaryCondition.QNAME) )
              return new BoundaryCondition(feature);
            
            return null;
          }
        };
        cMap.put(IBoundaryCondition.class, cTor);
        
		return Collections.unmodifiableMap(cMap);
	}
    
    
}
