package org.kalypso.kalypsosimulationmodel.core.roughness;

import java.util.Arrays;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelRoughnessConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * This class represents a wbr:roughnessClsCorrection element.
 * 
 * @author Patrice Congo
 */
public class RoughnessClsCorrection implements IRoughnessClsCorrection
{
	private final Feature feature;

	public RoughnessClsCorrection(Feature feature)
	{
		Assert.throwIAEOnNotDirectInstanceOf(
				feature, 
				KalypsoModelRoughnessConsts.WBR_F_ROUGHNESS_CORRECTION);
		
		this.feature=feature;
	}
	
	/**
	 * Create a {@link RoughnessClsCorrection} object based on a 
	 * wbr:RoughnessClsCorrection feature created as child 
	 * in the given parent and link to it by a property which type
	 * is specified by the given QName
	 * 
	 * @param parentFeature the parent feature 
	 * @param propName the link property Q-Name
	 */
	public RoughnessClsCorrection(
						Feature parentFeature,
						QName propQName)
	{
		Assert.throwIAEOnNull(
				propQName, Messages.getString("RoughnessClsCorrection.0")); //$NON-NLS-1$
		Assert.throwIAEOnNull(
				parentFeature, 
				Messages.getString("RoughnessClsCorrection.1")); //$NON-NLS-1$
		try
		{
			this.feature=
				FeatureHelper.addFeature(
					parentFeature, 
					propQName, 
					KalypsoModelRoughnessConsts.WBR_F_ROUGHNESS_CORRECTION);
			
			
		}
		catch(GMLSchemaException ex)
		{
			 
			throw new IllegalArgumentException(
					"Property "+propQName+ //$NON-NLS-1$
						Messages.getString("RoughnessClsCorrection.3")+ //$NON-NLS-1$
					KalypsoModelRoughnessConsts.WBR_F_ROUGHNESS,
					ex);
		}
	}
	
	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection#configure(double, double, double, double)
	 */
	public RoughnessCorConfigConsistency configure(
											double ksCor, 
											double axayCor,
											double dpCor, 
											double eddyCor,
											double marshCor)
	{
		RoughnessCorConfigConsistency check=
			RoughnessClsCorrection.validate(
					ksCor, axayCor, dpCor, eddyCor, marshCor);
		if(check!=RoughnessCorConfigConsistency.OK)
		{
			return check;
		}
		setKsCor(ksCor);
		setAxAyCor(axayCor);
		setDpCor(dpCor);
		setKsCor(ksCor);
		setMarshCor(marshCor);
		return RoughnessCorConfigConsistency.OK;
	}

	public static final RoughnessCorConfigConsistency validate(
												double ksCor, 
												double axayCor,
												double dpCor, 
												double eddyCor,
												double marshCor)
	{
		
		if(Double.isNaN(axayCor) || axayCor<0)
		{
			return RoughnessCorConfigConsistency.ILLEGAL_VALUE_AXAY_COR;
		}
		
		
		if(Double.isNaN(dpCor) || dpCor<0)
		{
			return RoughnessCorConfigConsistency.ILLEGAL_VALUE_DP_COR;
		}
		
		
		if(Double.isNaN(eddyCor) || eddyCor<0)
		{
			return RoughnessCorConfigConsistency.ILLEGAL_VALUE_EDDY_COR;
		}
		
		
		if(Double.isNaN(ksCor) || ksCor<0)
		{
			return RoughnessCorConfigConsistency.ILLEGAL_VALUE_KS_COR;
		}
		
		
		if(Double.isNaN(marshCor) || marshCor<0)
		{
			return RoughnessCorConfigConsistency.ILLEGAL_VALUE_MARSH_COR;
		}
		return RoughnessCorConfigConsistency.OK;
	} 
	
	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection#getAxAyCor()
	 */
	public double getAxAyCor()
	{
		return FeatureHelper.getAsDouble(
				feature, 
				KalypsoModelRoughnessConsts.WBR_PROP_AXAY_COR, 
				Double.NaN);
	}

	public double getMarshCor()
	{
		return FeatureHelper.getAsDouble(
				feature, 
				KalypsoModelRoughnessConsts.WBR_PROP_MARSH_COR, 
				Double.NaN);
	}
	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection#getDescription()
	 */
	public String getDescription()
	{
		Object obj= feature.getProperty(
				KalypsoModelRoughnessConsts.GML_PROP_DESCRIPTION);
		if(obj instanceof String)
		{
			return (String)obj;
		}
		else if(obj==null)
		{
			return null;
		}
		else
		{
			throw new RuntimeException(
					Messages.getString("RoughnessClsCorrection.4")+obj+ //$NON-NLS-1$
					"\n\tof type:"+obj.getClass()); //$NON-NLS-1$
		}
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection#getDpcor()
	 */
	public double getDpCor()
	{
		return FeatureHelper.getAsDouble(
				feature, 
				KalypsoModelRoughnessConsts.WBR_PROP_DP_COR, 
				Double.NaN);
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection#getEddyCor()
	 */
	public double getEddyCor()
	{
		return FeatureHelper.getAsDouble(
				feature, 
				KalypsoModelRoughnessConsts.WBR_PROP_EDDY_COR, 
				Double.NaN);
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection#getKsCor()
	 */
	public double getKsCor()
	{
		return FeatureHelper.getAsDouble(
				feature, 
				KalypsoModelRoughnessConsts.WBR_PROP_KS_COR, 
				Double.NaN);
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection#getURI()
	 */
	public String getURI()
	{
		return feature.getId();
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection#setAxAyCor(double)
	 */
	public void setAxAyCor(double axayCor) throws IllegalArgumentException
	{
		Assert.throwIAEOnLessThan0(axayCor, Messages.getString("RoughnessClsCorrection.6")); //$NON-NLS-1$
		feature.setProperty(
				KalypsoModelRoughnessConsts.WBR_PROP_AXAY_COR, 
				Double.valueOf(axayCor));
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection#setDescription(java.lang.String)
	 */
	public void setDescription(String descriptionText)
	{
		Assert.throwIAEOnNull(
				descriptionText, null);
		feature.setProperty(
				KalypsoModelRoughnessConsts.GML_PROP_DESCRIPTION, 
				descriptionText);
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection#setDpCor(double)
	 */
	public void setDpCor(double dpCor) throws IllegalArgumentException
	{
		Assert.throwIAEOnLessThan0(dpCor, Messages.getString("RoughnessClsCorrection.7")); //$NON-NLS-1$
		feature.setProperty(
				KalypsoModelRoughnessConsts.WBR_PROP_DP_COR, 
				Double.valueOf(dpCor));
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection#setEddy(double)
	 */
	public void setEddyCor(double eddyCor) throws IllegalArgumentException
	{
		Assert.throwIAEOnLessThan0(eddyCor, Messages.getString("RoughnessClsCorrection.8")); //$NON-NLS-1$
		feature.setProperty(
				KalypsoModelRoughnessConsts.WBR_PROP_EDDY_COR, 
				Double.valueOf(eddyCor));
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection#setKsCor(double)
	 */
	public void setKsCor(double ksCor) throws IllegalArgumentException
	{
		Assert.throwIAEOnLessThan0(ksCor, Messages.getString("RoughnessClsCorrection.9")); //$NON-NLS-1$
		feature.setProperty(
				KalypsoModelRoughnessConsts.WBR_PROP_KS_COR, 
				Double.valueOf(ksCor));
	}

	public void setMarshCor(double marshCor) throws IllegalArgumentException
	{
		Assert.throwIAEOnLessThan0(
				marshCor, Messages.getString("RoughnessClsCorrection.10")); //$NON-NLS-1$
		feature.setProperty(
				KalypsoModelRoughnessConsts.WBR_PROP_MARSH_COR, 
				Double.valueOf(marshCor));
	}
	
	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection#setURI(java.lang.String)
	 */
	public void setURI(String uri) throws IllegalArgumentException
	{
		
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection#validate()
	 */
	public RoughnessCorConfigConsistency validate()
	{
		double axayCor= getAxAyCor();
		double ksCor=getKsCor();
		double dpCor=getDpCor();
		double eddyCor=getDpCor();
		double marshCor=getMarshCor();
		
		return RoughnessClsCorrection.validate(
				ksCor, axayCor, dpCor, eddyCor, marshCor);
	}
	
	public String getName()
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
	
	public void setName(
			String name) 
			throws IllegalArgumentException
	{
		name=Assert.throwIAEOnNullOrEmpty(name);
		FeatureHelper.addProperty(
				feature,
				KalypsoModelRoughnessConsts.GML_PROP_NAME, 
				Arrays.asList(new String[]{name}) );
	}
	
	public Feature getWrappedFeature()
	{
		return feature;
	}
    /**
     * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper#getGmlID()
     */
    public String getGmlID( )
    {
      return feature.getId();
    }
    
	@Override
	public String toString()
	{
		StringBuffer buf= new StringBuffer(64);
		buf.append("RoughnessClsCorrection"); //$NON-NLS-1$
		String id=feature.getId();
		if(id!=null)
		{
			buf.append('.');
			buf.append(id);
		}
		buf.append('[');
		buf.append("axayCor="); //$NON-NLS-1$
		buf.append(getAxAyCor());
		
		
		buf.append(", dpCor="); //$NON-NLS-1$
		buf.append(getDpCor());
		
		
		buf.append(", eddyCor="); //$NON-NLS-1$
		buf.append(getEddyCor());
		
		buf.append(",ksCor="); //$NON-NLS-1$
		buf.append(getKsCor());
		
		
		buf.append(",marshCor="); //$NON-NLS-1$
		buf.append(getMarshCor());
		
		buf.append(' ');
		buf.append(']');
		
		return buf.toString();
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(obj instanceof IRoughnessClsCorrection)
		{
			IRoughnessClsCorrection cor=(IRoughnessClsCorrection)obj;
			if(Double.compare(cor.getAxAyCor(),getAxAyCor())!=0)
			{
				return false;
			}
			if(Double.compare(cor.getDpCor(),getDpCor())!=0)
			{
				return false;
			}
			if(Double.compare(cor.getEddyCor(),getEddyCor())!=0)
			{
				return false;
			}
			if(Double.compare(cor.getKsCor(),getKsCor())!=0)
			{
				return false;
			}
			if(Double.compare(cor.getMarshCor(),getMarshCor())!=0)
			{
				return false;
			}
			return true;
		}
		else
		{
			return super.equals(obj);
		}
	}
    
    
}
