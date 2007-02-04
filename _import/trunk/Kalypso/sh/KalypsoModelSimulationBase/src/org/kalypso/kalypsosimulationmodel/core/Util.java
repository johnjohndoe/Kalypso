
package org.kalypso.kalypsosimulationmodel.core;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelRoughnessConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

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
	
	public static final Feature createFeatureWithId(
				QName newFeatureQName,
	            Feature parentFeature,
	            QName propQName,
	            String gmlID)
	            throws IllegalArgumentException
	{
			
		Assert.throwIAEOnNullParam(parentFeature,"parentFeature");
		Assert.throwIAEOnNullParam(propQName, "propQName");
		Assert.throwIAEOnNullParam(newFeatureQName, "newFeatureQName");
		gmlID=Assert.throwIAEOnNullOrEmpty( gmlID );
		
		GMLWorkspace workspace=parentFeature.getWorkspace();
		IGMLSchema schema=workspace.getGMLSchema();
		IFeatureType featureType=
		schema.getFeatureType(newFeatureQName);
		IPropertyType parentPT=
		parentFeature.getFeatureType().getProperty( propQName );
		if(!(parentPT instanceof      IRelationType))
		{
			throw new IllegalArgumentException(
			      "Property not a IRelationType="+parentPT+
			      " propQname="+propQName);
		}
	
		Feature created = FeatureFactory.createFeature( 
									parentFeature, 
									(IRelationType)parentPT, 
									gmlID, 
									featureType, 
									true );
       
		try
		{
			if(parentPT.isList())
			{
				workspace.addFeatureAsComposition( 
				    parentFeature, 
				    (IRelationType)parentPT, 
				    -1, 
				    created );
			}
			else
			{
				//TODO test this case
				parentFeature.setProperty( parentPT, created );
			}
		}
		catch( Exception e )
		{
		throw new RuntimeException("Could not add to the workspace",e);
		}
		
		return created;
	}
	
	
	
	/**
	   * Create a feature of the given type and link
	   * it to the given parentFeature as a property of the
	   * specified q-name
	   * @param parentFeature the parent feature
	   * @param propQName the q-name of the property linking the
	   *    parent and the newly created child
	   * @param featureQName the q-name denoting the type of the feature
	   */
	  public static final  Feature createFeatureAsProperty(
	      Feature parentFeature,
	      QName propQName, 
	      QName featureQName)
	      throws IllegalArgumentException
	  {
	    Assert.throwIAEOnNull(
	    propQName, "Argument propQName must not be null");
	    Assert.throwIAEOnNull(
	        parentFeature, 
	        "Argument roughnessCollection must not be null");
	    
	    try
	    {
	       Feature feature=
             FeatureHelper.addFeature( 
	          parentFeature, 
	          propQName, 
	          featureQName);
           
           return feature;
	    }
	    catch(GMLSchemaException ex)
	    {
	      throw new IllegalArgumentException(
	          "Property "+propQName+
	              " does not accept element of type"+
	          featureQName,
	          ex);
	    }   
	  }
	  
	  /**
	   * TODO complete and test this method
	   */
	  
	  /**
	   * Create a feature of the given type and link
	   * it to the given parentFeature as a property of the
	   * specified q-name
	   * @param parentFeature the parent feature
	   * @param propQName the q-name of the property linking the
	   *    parent and the newly created child
	   * @param featureQName the q-name denoting the type of the feature
	   * @param throws {@link IllegalArgumentException} if parentFeature
	   *    is null or propQName is null, or featureQName is null or 
	   *    featureID is null or empty or there is a feature in the workspace
	   *    with the same id
	   *    
	   */
	  public static final  Feature createFeatureAsProperty(
	            Feature parentFeature,
	            QName propQName, 
	            QName featureQName,
	            String featureID)
	            throws IllegalArgumentException
	  {
	    Assert.throwIAEOnNull(
	    propQName, "Argument propQName must not be null");
	    Assert.throwIAEOnNull(
	        parentFeature, 
	        "Argument roughnessCollection must not be null");
	    featureID=Assert.throwIAEOnNullOrEmpty( featureID );
	    
	    
	    
//	    try
//	    {
	      IGMLSchema schema = 
	        parentFeature.getFeatureType().getGMLSchema();
	      IFeatureType featureType=
	          schema.getFeatureType( featureQName );
	      IPropertyType propertyType=
	              featureType.getProperty( propQName );
	      if(!(propertyType instanceof IRelationType))
	      {
	        throw new RuntimeException("UPS I DID IT AGAIN");
	      }
	      return FeatureFactory.createFeature( 
	                        parentFeature, 
	                        (IRelationType)propertyType,//parentRelation, 
	                        featureID, 
	                        featureType, 
	                        true,//initializeWithDefaults, 
	                        1//depth 
	                        );
//	       return FeatureHelper.addFeature(
//	          parentFeature, 
//	          propQName, 
//	          featureQName);
//	    }
//	    catch(GMLSchemaException ex)
//	    {
//	      throw new IllegalArgumentException(
//	          "Property "+propQName+
//	              " does not accept element of type"+
//	          featureQName,
//	          ex);
//	    }   
	  }
}
