package org.kalypso.kalypsosimulationmodel.core.roughness;

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
	
	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection#configure(double, double, double, double)
	 */
	public RoughnessCorConfigConsistency configure(double ks, double axay,
			double dp, double eddy)
	{
		return null;
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
					"Unexpected property value:"+obj+
					"\n\tof type:"+obj.getClass());
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
		Assert.throwIAEOnLessThan0(axayCor, "axay must be positiv");
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
		Assert.throwIAEOnLessThan0(dpCor, "dpCor must be positiv");
		feature.setProperty(
				KalypsoModelRoughnessConsts.WBR_PROP_DP_COR, 
				Double.valueOf(dpCor));
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection#setEddy(double)
	 */
	public void setEddyCor(double eddyCor) throws IllegalArgumentException
	{
		Assert.throwIAEOnLessThan0(eddyCor, "eddyCor must be positiv");
		feature.setProperty(
				KalypsoModelRoughnessConsts.WBR_PROP_EDDY_COR, 
				Double.valueOf(eddyCor));
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection#setKsCor(double)
	 */
	public void setKsCor(double ksCor) throws IllegalArgumentException
	{
		Assert.throwIAEOnLessThan0(ksCor, "ksCor must be positiv");
		feature.setProperty(
				KalypsoModelRoughnessConsts.WBR_PROP_KS_COR, 
				Double.valueOf(ksCor));
	}

	public void setMarshCor(double marshCor) throws IllegalArgumentException
	{
		Assert.throwIAEOnLessThan0(
				marshCor, "marshCor must be positiv");
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
		
		double d=getAxAyCor();
		if(Double.isNaN(d) || d<0)
		{
			return RoughnessCorConfigConsistency.ILLEGAL_VALUE_AXAY_COR;
		}
		
		d=getDpCor();
		if(Double.isNaN(d) || d<0)
		{
			return RoughnessCorConfigConsistency.ILLEGAL_VALUE_DP_COR;
		}
		
		d=getEddyCor();
		if(Double.isNaN(d) || d<0)
		{
			return RoughnessCorConfigConsistency.ILLEGAL_VALUE_EDDY_COR;
		}
		
		d=getKsCor();
		if(Double.isNaN(d) || d<0)
		{
			return RoughnessCorConfigConsistency.ILLEGAL_VALUE_KS_COR;
		}
		
		d=getMarshCor();
		if(Double.isNaN(d) || d<0)
		{
			return RoughnessCorConfigConsistency.ILLEGAL_VALUE_MARSH_COR;
		}
		return RoughnessCorConfigConsistency.OK;
	}
	
	public Feature getWrappedFeature()
	{
		return feature;
	}
}
