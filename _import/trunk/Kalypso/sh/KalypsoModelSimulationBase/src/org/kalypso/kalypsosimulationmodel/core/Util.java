
package org.kalypso.kalypsosimulationmodel.core;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelRoughnessConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;

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
	
	public static final String getFirstName(Feature feature)
	{
		Object obj= feature.getProperty(
				KalypsoModelRoughnessConsts.GML_PROP_NAME);
		if(obj instanceof String)
		{
			return (String)obj;
		}
		else if(obj instanceof List)
		{
			if(((List)obj).size()>0)
			{
				return (String)((List)obj).get(0);
			}
			else
			{
				return null;
			}
		}
		else
		{
			return null;
		}
	}
	
	public static final List<String> getAllName(Feature feature)
	{
		Object obj= feature.getProperty(
				KalypsoModelRoughnessConsts.GML_PROP_NAME);
		if(obj instanceof String)
		{
			return Arrays.asList( new String[]{(String)obj});
		}
		else if(obj instanceof List)
		{
		
				ArrayList<String> names=
					new ArrayList<String>((List<String>)obj);
				return names;
		}
		else
		{
			return null;
		}
	}
	
	public static Feature createFeatureForListProp( 
									final FeatureList list, 
									final QName listProperty, 
									final QName newFeatureName ) 
									throws GMLSchemaException
	  {
	    
	    final Feature parentFeature = list.getParentFeature();
	    final GMLWorkspace workspace = parentFeature.getWorkspace();
	
	    final IRelationType parentRelation = list.getParentFeatureTypeProperty();
		final IFeatureType targetFeatureType = 
	    	parentRelation.getTargetFeatureType();
	
	    final IFeatureType newFeatureType;
	    if( newFeatureName == null )
	    {
	      newFeatureType = targetFeatureType;
	    }
	    else
	    {
	      newFeatureType = 
	    	  workspace.getGMLSchema().getFeatureType( newFeatureName );
	    }
	
	    if( 	newFeatureName != null && 
	    		!GMLSchemaUtilities.substitutes( 
	    							newFeatureType, 
	    							targetFeatureType.getQName() ) )
	    {
	      throw new GMLSchemaException( 
	    		  "Type of new feature (" + newFeatureName + 
	    		  ") does not substitutes target feature type of the list: " + 
	    		  targetFeatureType.getQName() );
	    }
	    
	    final Feature newFeature = 
	    		workspace.createFeature( parentFeature, parentRelation, newFeatureType );
	
	    return newFeature;
	  }
	
	/**
	 * 
	 * @param feature -- the feature which workspace of to search for other
	 * 			feature of the same type having the specified uri
	 * 
	 * @param uri the uri to lookup
	 * 
	 * @return true is  in the feature workspace there is a feature of the some 
	 * 			type with the given 
	 */
	public static final boolean isInFeatureWorkspace(
												Feature feature,
												QName propQName,
												String uri)
	{
		
		return false;
	}
}
