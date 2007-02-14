package org.kalypso.kalypsosimulationmodel.core;

import java.io.File;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.property.IPropertyType;
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
	
	/**
	 * Assert the given object for null value.
	 * This method throws concequently an illegal argument exception
	 * if the passed object is null
	 * 
	 * @param obj the object to be asserted
	 * @param message the exception message
	 * @throws IllegalArgumentException if the passed object is null
	 */
	public static final void throwIAEOnNullParam(
								Object param, 
								String paramName)
								throws IllegalArgumentException
	{
		if(param==null)
		{
			StringBuffer buf= new StringBuffer(128);
			buf.append("Parameter must not be null; param name:");
			buf.append(paramName);
			throw new IllegalArgumentException(buf.toString());
		}
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
	public static final void throwIAEOnFeaturePropNotList(
								Feature feature,
								QName propToTest,
								String message)
								throws IllegalArgumentException
	{
		IPropertyType type=
			feature.getFeatureType().getProperty(propToTest);
		if(!type.isList())
		{
			if(message==null)
			{
				StringBuffer buf= new StringBuffer();
				buf.append("Feature does not have list property of the given name");
				buf.append("\n\tFeature=");
				buf.append(feature);
				buf.append("\n\tProperty  QNAme=");
				buf.append(propToTest);
				message=buf.toString();
			}
			
			throw new IllegalArgumentException(message);
		}
	}
	
	public static final String throwIAEOnNullOrEmpty(
									String str)
									throws IllegalArgumentException
	{
		if(str==null)
		{
			throw new IllegalArgumentException("String must not be null");
		}
		str=str.trim();
		if(str.length()==0)
		{
			throw new IllegalArgumentException("String must not be empty");
		}
		return str;
	}
	
	
	
	public static final boolean isNullOrEmpty(String str)
	{
		if(str==null)
		{
			return true;
		}
		str=str.trim();
		if(str.length()==0)
		{
			return true;
		}
		return false;
	}
	
	public static final void throwIAEOnLessThan0(
						double d,
						String message)
						throws IllegalArgumentException
	{
		if(d<0)
		{
			if(message==null)
			{
				message="number must be greater or equals to 0";
			}
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
    
    public static final void throwIAEOnNulOrIsDirOrNotExistsOrNotReadable(
                                              File file)
                                               throws IllegalArgumentException
    {
      if(file==null)
      {
        throw new IllegalArgumentException("file must not be null");
      }
      
      if(file.isDirectory())
      {
        throw new IllegalArgumentException("File must not be a directory:"+file);      
      }
      
      if(!file.exists())
      {
        throw new IllegalArgumentException("file must exist:"+file);
      }
      
      if(!file.canRead())
      {
        throw new IllegalArgumentException("File cannot be read:"+file);
      }
      
    }
}
