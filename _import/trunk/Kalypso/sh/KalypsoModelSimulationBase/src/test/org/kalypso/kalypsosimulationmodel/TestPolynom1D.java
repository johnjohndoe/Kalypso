/**
 * 
 */
package test.org.kalypso.kalypsosimulationmodel;

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.Collections;

import org.deegree_impl.gml.GMLDocument_Impl;
import org.deegree_impl.gml.GMLFactory;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypso.kalypsosimulationmodel.util.math.IPolynom1D;
import org.kalypso.kalypsosimulationmodel.util.math.Polynom1D;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;
import org.kalypsodeegree_impl.model.feature.IFeatureProviderFactory;

import junit.framework.TestCase;

/**
 * @author Patrice Congo
 *
 */
public class TestPolynom1D extends TestCase
{
	public static final IPolynom1D EXPECTED_POL1D=
		new IPolynom1D()
	{

		public CONSISTENCY_CHECK checkConsistency()
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
			if(obj instanceof IPolynom1D)
			{
				int order=getOrder();
				if(order!=((IPolynom1D)obj).getOrder())
				{
					return false;
				}
				double[] thisCoefs=getCoefficients();
				double[] compCoefs=((IPolynom1D)obj).getCoefficients();
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
									TestWorkspaces.URL_POLYNOMS, null );
			}
			catch(Throwable th)
			{
				fail(th.toString());
			}
			Feature pol1dFeature=workspace.getRootFeature();
			assertEquals(
					pol1dFeature.getFeatureType().getQName(), 
					KalypsoModelSimulationBaseConsts.SIM_BASE_PLYNOMIAL1D);
			Polynom1D pol1d= new Polynom1D(pol1dFeature);
			
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
		}
		catch(Throwable th)
		{
		
			fail(th.toString());
		}
		
	}
}
