/**
 * 
 */
package org.kalypso.kalypsosimulationmodel.core;

import java.util.Collections;
import java.util.Hashtable;
import java.util.Map;

import org.eclipse.core.runtime.IAdapterFactory;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection;
import org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessClsCorrection;
import org.kalypso.kalypsosimulationmodel.util.math.IPolynomial1D;
import org.kalypso.kalypsosimulationmodel.util.math.IPolynomial2D;
import org.kalypso.kalypsosimulationmodel.util.math.Polynomial1D;
import org.kalypso.kalypsosimulationmodel.util.math.Polynomial2D;
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
	
		
	private final Class[] ADAPTER_LIST=
				{IPolynomial1D.class,IPolynomial2D.class};
	
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
		return ADAPTER_LIST;
	}

	private final Map<Class, AdapterConstructor> createConstructorMap()
	{
		Map<Class, AdapterConstructor> cMap= 
				new Hashtable<Class, AdapterConstructor>();
		
		//polynomial 1d
		AdapterConstructor cTor= new AdapterConstructor()
		{
			public Object constructAdapter(
										Feature feature, 
										Class cls) 
										throws IllegalArgumentException
			{
				
				return new Polynomial1D(feature);
			}
		};
		cMap.put(IPolynomial1D.class, cTor);
		
		//Polynomial 2d
		cTor= new AdapterConstructor()
		{
			public Object constructAdapter(
										Feature feature, 
										Class cls) 
										throws IllegalArgumentException
			{
				
				return new Polynomial2D(feature);
			}
		};
		cMap.put(IPolynomial2D.class, cTor);
		
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
		
		
		return Collections.unmodifiableMap(cMap);
	}
}
