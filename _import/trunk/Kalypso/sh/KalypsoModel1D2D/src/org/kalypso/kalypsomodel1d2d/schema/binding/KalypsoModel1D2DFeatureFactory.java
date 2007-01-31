/**
 * 
 */
package org.kalypso.kalypsomodel1d2d.schema.binding;

import java.util.Collections;
import java.util.Hashtable;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IAdapterFactory;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection;
import org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessClsCorrection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.RoughnessPolygon;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

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
	
		
	private final Class[] ADAPTER_LIST=
				{IFE1D2DNode.class};
	
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
		return ADAPTER_LIST;
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
				
				return new FE1D2DNode(feature);
			}
		};
		cMap.put(IFE1D2DNode.class, cTor);
		
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
                Object toInv=
                  feature.getProperty( 
                        Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGE_IN_INV );
                if(toInv instanceof String)
                {
                  GMLWorkspace workspace=feature.getWorkspace();
                  toInv=workspace.getFeature( (String )toInv);
                }
                FE1D2DEdge edge = new FE1D2DEdge((Feature)toInv);
                
                return new EdgeInv(edge);     
              }
              else
              {
                return new FE1D2DEdge(feature);
              }
            }
        };
        cMap.put(IFE1D2DEdge.class, cTor);
        
        //1d2d element
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
                  return new FE1D2D_2DElement(feature);
                }
            }
        };
        cMap.put(IFE1D2DElement.class, cTor);
		
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
                  return new FE1D2D_2DElement(feature);
                }
            }
        };
        cMap.put(IFE1D2DComplexElement.class, cTor);
        
		return Collections.unmodifiableMap(cMap);
	}
}
