package org.kalypso.kalypsosimulationmodel.util.math;

import java.math.BigInteger;

import javax.xml.namespace.QName;

import org.apache.xmlbeans.impl.xb.xmlconfig.Qnameconfig;
import org.kalypso.kalypsosimulationmodel.exception.IllegalFeatureState;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Patrice Congo
 *
 */
public class Polynomial2D implements IPolynomial2D
{
	final private Feature polFeature;
	
	public Polynomial2D(Feature polFeature)
	{
		if(polFeature==null)
		{
			throw new IllegalArgumentException(
					"Argument feature must not be null");
		}
		QName expectedQName=
			KalypsoModelSimulationBaseConsts.SIM_BASE_F_POLYNOMIAL2D;
		QName currentQName=polFeature.getFeatureType().getQName();
		if(!expectedQName.equals(currentQName))
		{
			StringBuffer buf= new StringBuffer(128);
			buf.append("Required type:");
			buf.append(expectedQName);
			buf.append("\n\tcurrent feature type=");
			buf.append(currentQName);
			
			throw new IllegalArgumentException(buf.toString());
		}
		polFeature.getFeatureType().getQName();
		this.polFeature=polFeature;
	}
	
	public PolynomialConfigState checkConsistency()
	{
		return null;
	}

	
	public static final PolynomialConfigState checkConsistency(
											int degreeX,
											int degreeY,
											double[] coefficients)
	{
		if(coefficients==null)
		{
			throw new IllegalArgumentException(
					"argument coefs must not be null");
		}
		
		if(degreeX<0)
		{
			return PolynomialConfigState.NEGATIVE_DEGREEX;
		}
		
		if(degreeY<0)
		{
			return PolynomialConfigState.NEGATIVE_DEGREEY;
		}
		
		if((degreeY+1)*(degreeX+1)!=coefficients.length)
		{
			return PolynomialConfigState.ORDER_COEF_MISMATCH;
		}
		//TODO check last x and last y
		if(coefficients[coefficients.length-1]==0.0)
		{
			return PolynomialConfigState.ZERO_MOST_SIGNIFICANT_COEFS;
		}
		return PolynomialConfigState.CONSISTENCY_OK;
	}
	
	public double evaluate(
						double inputX, 
						double inputY)
	{
		throw new RuntimeException("not supported");
	}

	public double[] getCoefficients() throws IllegalFeatureState
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
			throw new IllegalFeatureState(
					polFeature,
					KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_COEFFICIENTS,
					coefs);
		}
	}
	
	public void setCefficients(double[] coefficients) throws IllegalArgumentException
	{
		
	}

	public int getDegreeX()throws IllegalFeatureState
	{
		return getDegree(
				KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_DEGREEX);
	}
	
	public void setDegreeX(int degreeX)
	{
		setDegree(
				degreeX,
				KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_DEGREEX);
	}
	
	public int getDegreeY()throws IllegalFeatureState
	{
		return getDegree(
				KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_DEGREEY);
	}
	public void setDegreeY(int degreeY)
	{
		setDegree(
				degreeY,
				KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_DEGREEY);
	}
	
	private final void setDegree(
						int degree, 
						QName degreeQName) throws IllegalArgumentException
	{
		
		if(degree<=0 )
		{
			throw new IllegalArgumentException();
		}
		
		polFeature.setProperty(
						degreeQName, 
						new Integer(degree));
	}
	
	private final int getDegree(
						QName degreeQName)
						throws IllegalFeatureState 
	{
		Object dx=polFeature.getProperty(degreeQName);
		if(dx instanceof BigInteger)
		{
			return ((BigInteger)dx).intValue();
		}
		else
		{
			StringBuffer buf= new StringBuffer(128);
			buf.append(degreeQName);
			buf.append(" must be and integer but got:");
			buf.append(dx==null?null:dx.getClass());
			buf.append("with the value:");
			buf.append(dx);
			throw new IllegalFeatureState(
									buf.toString(),
									polFeature,
									degreeQName,
									dx);
			
		}
		
	}
	
	public void setPolynomParameters(
								int degreeX, 
								int degreeY, 
								double[] coefficients) 
								throws IllegalArgumentException
	{
		if(PolynomialConfigState.CONSISTENCY_OK!=
			checkConsistency(degreeX, degreeY, coefficients))
		{
			throw new IllegalArgumentException();
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

}
