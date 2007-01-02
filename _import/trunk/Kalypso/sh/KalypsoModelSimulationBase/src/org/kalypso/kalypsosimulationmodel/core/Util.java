
package org.kalypso.kalypsosimulationmodel.core;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypsodeegree.model.feature.Feature;

/**
 * Holds utility methods
 * 
 * @author Patrice Congo
 *
 */
public class Util
{
	
	/**
	 * Test whether the given feature is an elmenent of the type
	 * specified by the q-name.
	 * 
	 * @param feature the feature  instance, which type is to be access
	 * @param typeQname -- the required qname for the feature type
	 *  
	 * @return true if qname of the given feature match the one passed
	 *  otherwise false 
	 */
	public static final boolean directInstanceOf(
										Feature feature,
										QName typeQname)
	{
		if(feature==null || typeQname==null)
		{
			throw new IllegalArgumentException(
					"Argument feature and typeQname must not be null:"+
					"\tfeature="+feature+
					"\ttypeQname="+typeQname);
		}
		
		return typeQname.equals(feature.getFeatureType().getQName());
	}
	
	public static final List<Feature> toFeatureList(
									Collection<? extends IFeatureWrapper> c)
	{
		List<Feature> fl= new ArrayList<Feature>();
		if(c!=null)
		{
			Feature f;
			for(IFeatureWrapper fw:c)
			{
				f=fw.getWrappedFeature();
				if(f==null)
				{
					throw new IllegalArgumentException(
							"All feature wrapper must wrapp a non null feature:"+c);
				}
				fl.add(f);
			}
		}
		return fl;
	}
	
	
}
