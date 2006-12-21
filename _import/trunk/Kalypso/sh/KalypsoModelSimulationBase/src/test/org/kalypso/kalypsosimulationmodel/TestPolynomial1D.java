/**
 * 
 */
package test.org.kalypso.kalypsosimulationmodel;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Arrays;

import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypso.kalypsosimulationmodel.util.math.PolynomialConfigState;
import org.kalypso.kalypsosimulationmodel.util.math.IPolynomial1D;
import org.kalypso.kalypsosimulationmodel.util.math.Polynomial1D;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

import junit.framework.TestCase;

/**
 * @author Patrice Congo
 *
 */
public class TestPolynomial1D extends TestCase
{
	public static final IPolynomial1D EXPECTED_POL1D=
		new IPolynomial1D()
	{

		public PolynomialConfigState checkConsistency()
		{
			return null;
		}

		public double computeResult(double input)
		{
			return 0;
		}

		public double[] getCoefficients()
		{
			return new double[]{0,1,2,3,4};
		}
		
		public void setCoefficients(
						double[] coefficients) 
						throws IllegalArgumentException
		{
		
		}
		
		public int getOrder()
		{
			return 4;
		}

		public void setOrder(int order) throws IllegalArgumentException
		{
			
		}

		public void setPolynomParameters(int order, double[] coefficients) throws IllegalArgumentException
		{
			
		}
		
		@Override
		public boolean equals(Object obj)
		{
			if(obj instanceof IPolynomial1D)
			{
				int order=getOrder();
				if(order!=((IPolynomial1D)obj).getOrder())
				{
					return false;
				}
				double[] thisCoefs=getCoefficients();
				double[] compCoefs=((IPolynomial1D)obj).getCoefficients();
				for(int i=thisCoefs.length-1;i>=0;i--)
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
				return false;
			}
		}
		
		@Override
		public String toString()
		{
			
			return "EXPECTED_POLYNOM[o="+getOrder()+
					" coefs="+Arrays.toString(getCoefficients())+"]";
		}
	};
	
	public void testWorkspaceLoad()
	{
		
			//IFeatureProviderFactory fpf;
			
			GMLWorkspace workspace=null;
			
			try
			{
				workspace=
					GmlSerializer.createGMLWorkspace( 
									TestWorkspaces.URL_POLYNOMIAL1D, 
									null );
			}
			catch(Throwable th)
			{
				fail(TestUtils.getStackTraceAsString(th));
			}
			Feature pol1dFeature=workspace.getRootFeature();
			assertEquals(
					pol1dFeature.getFeatureType().getQName(), 
					KalypsoModelSimulationBaseConsts.SIM_BASE_PLYNOMIAL1D);
			Polynomial1D pol1d= new Polynomial1D(pol1dFeature);
			System.out.println(
					"POL1PROPS:"+Arrays.asList(pol1dFeature.getFeatureType().getProperties()));
			assertEquals(EXPECTED_POL1D,pol1d);
			
	}
	
	public void testCreation()
	{
		GMLWorkspace workspace= null;
		try
		{
			workspace=
				GmlSerializer.createGMLWorkspace( 
					TestWorkspaces.URL_EMPTY_GML, null );;
			Feature root=workspace.getRootFeature();	
			Polynomial1D pol1d= new Polynomial1D(workspace);
			try
			{
				pol1d.getOrder();
				fail("no order set therefore illegal state exception expected");
			}
			catch(IllegalStateException e)
			{
				//expected
			}
			
			try
			{
				pol1d.getCoefficients();
				fail("no coefficient set therefor exception expected");
			}
			catch(Throwable e)
			{
				//expected
			}
			final int SET_ORDER=2;
			pol1d.setOrder(SET_ORDER);
			assertEquals(SET_ORDER, pol1d.getOrder());
			
			final double[] coefs={0,1,2};
			pol1d.setCoefficients(coefs);
			double[] retCoefs=pol1d.getCoefficients();
			assertTrue(Arrays.equals(coefs, retCoefs));
			assertEquals(
					pol1d.checkConsistency(), 
					PolynomialConfigState.CONSISTENCY_OK);
			assertEquals(0.0, pol1d.computeResult(0));
			assertEquals(21.0, pol1d.computeResult(3));
			assertEquals(55.0, pol1d.computeResult(5));
			
			pol1d.setPolynomParameters(0, new double[]{0});
			try
			{
				pol1d.setPolynomParameters(1, new double[]{0});
				fail("Bad combination order=1 coefs=0; illegal exception must be thrown");
			}
			catch(IllegalArgumentException e)
			{
				//expected
			}
			
			try
			{
				pol1d.setPolynomParameters(1, new double[]{1,1});
				//0 at end is not okay
				pol1d.setPolynomParameters(1, new double[]{1,0});
				fail(
					"Bad combination order=1 coefs=1,0; since last element "+
					"is zero illegal exception must be thrown");
			}
			catch(IllegalArgumentException e)
			{
				//expected
			}
			//System.out.println("Root="+workspace.getRootFeature().getClass());
		}
		catch(Throwable th)
		{
		
			fail(TestUtils.getStackTraceAsString(th));
		}
		
	}
	
	
}
