/**
 * 
 */
package org.kalypso.kalypsosimulationmodel.core;

import java.util.Map;

import org.eclipse.core.runtime.IAdapterFactory;
import org.kalypso.kalypsosimulationmodel.util.math.IPolynomial1D;
import org.kalypso.kalypsosimulationmodel.util.math.IPolynomial2D;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Adapter Factory for feature in the simBase namespace
 * 
 * @author Patrice Congo
 *
 */
public class KalypsoSimBaseFeatureFactory implements IAdapterFactory
{
	interface AdapterConstructor<T>
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
		public T constructAdapter(
							Feature feature,
							Class<T> cls) 
							throws IllegalArgumentException;
	}
	
	AdapterConstructor test= new AdapterConstructor<IPolynomial1D>()
	{
		public IPolynomial1D constructAdapter(Feature feature, Class<IPolynomial1D> cls) throws IllegalArgumentException
		{
			return null;
		}
	};
	
	private final Class[] ADAPTER_LIST=
				{IPolynomial1D.class,IPolynomial2D.class};
	
	private Map<Class, AdapterConstructor> constructors;
	
	
	
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
		
		
		
		return null;
	}

	/**
	 * @see org.eclipse.core.runtime.IAdapterFactory#getAdapterList()
	 */
	public Class[] getAdapterList()
	{
		return ADAPTER_LIST;
	}

}
