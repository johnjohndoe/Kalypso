/**
 * 
 */
package org.kalypso.kalypsosimulationmodel.core;

import java.util.Collections;
import java.util.Hashtable;
import java.util.Map;

import org.eclipse.core.runtime.IAdapterFactory;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection;
import org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessClsCorrection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetwork;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetworkCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModelSystem;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.NativeTerrainElevationModelWrapper;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.RiverProfileNetwork;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.RiverProfileNetworkCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.RoughnessPolygon;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.TerrainElevationModelSystem;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.TerrainModel;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Adapter Factory for feature in the simBase namespace
 * 
 * @author Patrice Congo
 *
 */
public class KalypsoSimBaseFeatureFactory implements IAdapterFactory
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
	
	
	
	public KalypsoSimBaseFeatureFactory()
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

        //terrain model
        AdapterConstructor cTor= new AdapterConstructor()
        {
            public Object constructAdapter(
                                        Feature feature, 
                                        Class cls) 
                                        throws IllegalArgumentException
            {
                
                return new TerrainModel(feature);
            }
        };
        cMap.put(ITerrainModel.class, cTor);
        
		//IRoughnessCls
		cTor= new AdapterConstructor()
		{
			public Object constructAdapter(
										Feature feature, 
										Class cls) 
										throws IllegalArgumentException
			{
				
				return new RoughnessCls(feature);
			}
		};
		cMap.put(IRoughnessCls.class, cTor);
		
		//IRoughnessClsCorrection
		cTor= new AdapterConstructor()
		{
			public Object constructAdapter(
										Feature feature, 
										Class cls) 
										throws IllegalArgumentException
			{
				
				return new RoughnessClsCorrection(feature);
			}
		};
		cMap.put(IRoughnessClsCorrection.class, cTor);
		
		
			
		//IRoughnessPolygon
		cTor= new AdapterConstructor()
		{
			public Object constructAdapter(
										Feature feature, 
										Class cls) 
										throws IllegalArgumentException
			{
				
				return new RoughnessPolygon(feature);
			}
		};
		cMap.put(IRoughnessPolygon.class, cTor);

        //IRiverProfileNetworkCollection
		cTor= new AdapterConstructor()
		{
		  public Object constructAdapter(
		      Feature feature, 
		      Class cls) 
		  throws IllegalArgumentException
		  {
		    return new RiverProfileNetworkCollection(feature);
		  }
		};
		cMap.put(IRiverProfileNetworkCollection.class, cTor);
		
		//IRiverProfileNetwork
		cTor= new AdapterConstructor()
		{
		  public Object constructAdapter(
		      Feature feature, 
		      Class cls) 
		  throws IllegalArgumentException
		  {
		    return new RiverProfileNetwork(feature);
		  }
		};
		cMap.put(IRiverProfileNetwork.class, cTor);
        
//      ITerrainElevationModelSystem
//        cTor= new AdapterConstructor()
//        {
//          public Object constructAdapter(
//              Feature feature, 
//              Class cls) 
//          throws IllegalArgumentException
//          {
//            return new TerrainElevationModelSystem(feature);
//          }
//        };
//        cMap.put(ITerrainElevationModelSystem.class, cTor);
		
//      ITerrainElevationModel
        cTor= new AdapterConstructor()
        {
          public Object constructAdapter(
              Feature feature, 
              Class cls) 
          throws IllegalArgumentException
          {
            //TODO provide adapaterfac method for the elevation model
            if(GMLSchemaUtilities.substitutes( 
                        feature.getFeatureType(), 
                        KalypsoModelSimulationBaseConsts.SIM_BASE_F_NATIVE_TERRAIN_ELE_WRAPPER
                        ))
            {
              try
              {
                return new NativeTerrainElevationModelWrapper(feature);
              }
              catch( Throwable th )
              {
                throw new IllegalArgumentException(
                    "Could not create adapter for:"+
                      "\n\t featue="+feature+
                      "\n\t adaptaer type="+cls,
                    th);
              }
              
            }
            else if(GMLSchemaUtilities.substitutes( 
                feature.getFeatureType(), 
                KalypsoModelSimulationBaseConsts.SIM_BASE_F_TERRAIN_ELE_SYS
                ))
            {
              try
              {
                return new TerrainElevationModelSystem(feature);
              }
              catch( Throwable th )
              {
                throw new IllegalArgumentException(
                    "Could not create adapter for:"+
                      "\n\t featue="+feature+
                      "\n\t adaptaer type="+cls,
                    th);
              }
              
            }
            else
            {
              System.out.println("Could not adapt="+feature+" to"+cls);
              return null;//new TerrainElevationModel(feature);
            }
          }
        };
        cMap.put(ITerrainElevationModel.class, cTor);
        cMap.put(ITerrainElevationModelSystem.class, cTor);
        
        
        
//      ITerrainModel
        cTor= new AdapterConstructor()
        {
          public Object constructAdapter(
              Feature feature, 
              Class cls) 
          throws IllegalArgumentException
          {
            return new TerrainModel(feature);
          }
        };
        cMap.put(ITerrainModel.class, cTor);

		return Collections.unmodifiableMap(cMap);
	}
}
