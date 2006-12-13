/**
 * 
 */
package org.kalypso.kalypsosimulationmodel.util.math;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * @author Patrice Congo
 *
 */
public class Polynom1D implements IPolynom1D
{
	final private Feature polFeature;
	
	public Polynom1D(Feature polFeature)
	{
		this.polFeature=polFeature;
	}
	
	public Polynom1D(GMLWorkspace workWorkspace)
	{
		IFeatureType featureType=
			workWorkspace.getGMLSchema().getFeatureType(
					KalypsoModelSimulationBaseConsts.SIM_BASE_PLYNOMIAL1D);
		this.polFeature=
			FeatureFactory.createFeature(
					workWorkspace.getRootFeature(),//parent, 
					"Polynom1d"+System.currentTimeMillis(),//TODO better ids , random? 
					featureType, 
					true);
	}
	
	
	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.util.math.IPolynom1D#computeResult(double)
	 */
	public double computeResult(double input)
	{
		//computation based on Hornerschemas
		double coefs[]= getCoefficients();
		
		int i=coefs.length;
		if(i==0)
		{
			throw new RuntimeException();
		}
		
		double result=coefs[i];
		i--;
		for(;i>=0;i--)
		{
			result=result*input+coefs[i];
		}
		return result;
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.util.math.IPolynom1D#getCoefficients()
	 */
	public double[] getCoefficients()
	{
		Object coefs=polFeature.getProperty(
				KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_COEFFICIENTS);
		if(coefs instanceof String)
		{
			String[] subStrings=((String)coefs).split("/s+");
			double doubles[]= new double[subStrings.length];
			for(int i=0; i<subStrings.length;i++)
			{
				doubles[i]=Double.parseDouble(subStrings[i]);
			}
			return doubles;
			
		}
		else
		{
			throw new RuntimeException();//TODO throw better exception
		}
		
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.util.math.IPolynom1D#getOrder()
	 */
	public int getOrder()
	{
		Object order=
			polFeature.getProperty(
				KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_ORDER);
		if(order instanceof Integer)
		{
			return ((Integer)order).intValue();
		}
		else
		{
		
			throw  new IllegalStateException(
					"Order property not a double for "+polFeature+
					"\t but a "+ (order==null?null:order.getClass())+
					"\t with value:"+order);
		}
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.util.math.IPolynom1D#setPolynomParameters(int, double[])
	 */
	public void setPolynomParameters(int order, double[] coefficients)
	{
		if(order<=0 )
		{
			throw new IllegalArgumentException();
		}
		
		if(coefficients==null)
		{
			throw new IllegalArgumentException();
		}
		
		if(order!=coefficients.length)
		{
			throw new IllegalArgumentException();
		}
		
		polFeature.setProperty(
				KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_ORDER, 
				String.valueOf(order));
		
		StringBuffer buf= 
				new StringBuffer(64);
		for(double coef:coefficients)
		{
			buf.append(String.valueOf(coef));
			buf.append(' ');
		}
		
		polFeature.setProperty(
				KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_COEFFICIENTS, 
				buf.toString());
	}

}
