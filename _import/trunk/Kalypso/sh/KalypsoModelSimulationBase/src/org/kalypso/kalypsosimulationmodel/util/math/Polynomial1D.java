/**
 * 
 */
package org.kalypso.kalypsosimulationmodel.util.math;

import java.lang.reflect.Array;
import java.math.BigInteger;
import java.util.Arrays;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * Default imlementation of the IPolynom1D interface
 * 
 * @author Patrice Congo
 *
 */
public class Polynomial1D implements IPolynomial1D
{
	final private Feature polFeature;
	
	public Polynomial1D(Feature polFeature)
	{
		this.polFeature=polFeature;
	}
	
	public Polynomial1D(GMLWorkspace workWorkspace)
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
		i--;//last element
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
			String[] subStrings=((String)coefs).split(" ");//"/s+");
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
	//TODO check for  the polynom with order 0 and a0=0
	
	public void setCoefficients(
						double[] coefficients) 
						throws IllegalArgumentException
	{
		if(coefficients==null)
		{
			throw new IllegalArgumentException();
		}
		if(coefficients.length==0)
		{
			throw new IllegalArgumentException(
					"a polynom must have at least ");
		}
		//allow
		if(coefficients[coefficients.length-1]==0 && coefficients.length==1)
		{
			throw new IllegalArgumentException(
					"Last element, except if element 0, must not be null to"+
					" respect the order matching with length only");
		}
		
		StringBuffer buf= new StringBuffer(128);
		for(double coef:coefficients)
		{
			buf.append(coef);
			buf.append(' ');
		}
		polFeature.setProperty(
				KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_COEFFICIENTS, 
				buf.toString());
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.util.math.IPolynom1D#getOrder()
	 */
	public int getOrder()
	{
		Object order=
			polFeature.getProperty(
				KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_ORDER);
		if(order instanceof BigInteger)
		{
			return ((BigInteger)order).intValue();
		}
		else if(order instanceof Integer)
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

	public void setOrder(int order) throws IllegalArgumentException
	{
		
		if(order<=0 )
		{
			throw new IllegalArgumentException();
		}
		
		polFeature.setProperty(
				KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_ORDER, 
				new Integer(order));
	}
	
	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.util.math.IPolynom1D#setPolynomParameters(int, double[])
	 */
	public void setPolynomParameters(
						int order, 
						double[] coefficients) 
						throws IllegalArgumentException
	{
//		if(order<=0 )
//		{
//			throw new IllegalArgumentException();
//		}
//		
//		if(coefficients==null)
//		{
//			throw new IllegalArgumentException();
//		}
//		
//		if(order!=coefficients.length)
//		{
//			throw new IllegalArgumentException();
//		}
		PolynomialConfigState checkHint=checkConsistency(order, coefficients);
		if(checkHint!=PolynomialConfigState.CONSISTENCY_OK)
		{
			StringBuffer buf= new StringBuffer("64");
			buf.append("Illegal order coefficient combination:");
			buf.append("\tcheckHint=");
			buf.append(checkHint);
			buf.append("\torder=");
			buf.append(order);
			buf.append("\tcoefficients=");
			buf.append(Arrays.toString(coefficients));
			throw new IllegalArgumentException(buf.toString());
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
	
	/**
	 * @see org.kalypso.kalypsosimulationmodel.util.math.IPolynomial1D#checkConsistency()
	 */
	public PolynomialConfigState checkConsistency()
	{
		int order=getOrder();
		double[] coefs=getCoefficients();
		try
		{
			return checkConsistency(order, coefs);
		}
		catch(Throwable th)
		{
			throw new RuntimeException(
					"Error while checking consistency",
					th);
		}
	}
	
	
	public static final PolynomialConfigState checkConsistency(
									int order,
									double[] coefs)
									throws IllegalArgumentException
	{
		if(coefs==null)
		{
			throw new IllegalArgumentException();
		}
		
		if(order<0)
		{
			return PolynomialConfigState.NEGATIVE_DEGREE;
		}
		else if(order!=coefs.length-1)
		{
			return PolynomialConfigState.ORDER_COEF_MISMATCH;
		}
		else if(coefs[order]==0)
		{
			if(order==0)
			{
				//allow 0*x^0
				return PolynomialConfigState.CONSISTENCY_OK;
			}
			else
			{
				return PolynomialConfigState.ZERO_MOST_SIGNIFICANT_COEFS;
			}
		}
		else
		{
			return PolynomialConfigState.CONSISTENCY_OK;
		}
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this==obj)
		{
			return true;
		}
		else if(obj instanceof IPolynomial1D)
		{
			final int THIS_ORDER=getOrder();
			if(THIS_ORDER!=((IPolynomial1D)obj).getOrder())
			{
				return false;
			}
			
			double thisCoefs[]=getCoefficients();
			double compCoefs[]=((IPolynomial1D)obj).getCoefficients();
			int i=thisCoefs.length;
			if(i!=compCoefs.length)
			{
				return false;
			}
			i--;//goto last element
			for(;i>=0;i--)
			{
				if(thisCoefs[i]!=compCoefs[i])
				{
					return false;
				}
			}
			return true;
			
		}
		else
		{
			return false;//super.equals(obj);
		}
	}

}
