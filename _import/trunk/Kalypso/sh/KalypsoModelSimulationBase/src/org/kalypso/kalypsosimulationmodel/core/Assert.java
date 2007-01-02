package org.kalypso.kalypsosimulationmodel.core;

import javax.xml.namespace.QName;

import org.kalypsodeegree.model.feature.Feature;

/**
 * Provides assertion methods
 * 
 * @author Patrice Congo
 */
public class Assert
{
	private Assert()
	{
		//empty
	}
	
	/**
	 * Assert the given object for null value.
	 * This method throws concequently an illegal argument exception
	 * if the passed object is null
	 * 
	 * @param obj the object to be asserted
	 * @param message the exception message
	 * @throws IllegalArgumentException if the passed object is null
	 */
	public static final void throwIAEOnNull(
								Object obj, 
								String message)
								throws IllegalArgumentException
	{
		if(obj==null)
		{
			throw new IllegalArgumentException(message);
		}
	}
	
	public static final void throwIAEOnNotDirectInstanceOf(
										Feature feature, 
										QName expectedType)
										throws IllegalArgumentException
	{
		if(!Util.directInstanceOf(
							feature, 
							expectedType))
		{
			throw new IllegalArgumentException(
					"Feature must be of type "+
					expectedType+
					"; the current type is:"+feature.getFeatureType().getQName());
		}
	}
}
