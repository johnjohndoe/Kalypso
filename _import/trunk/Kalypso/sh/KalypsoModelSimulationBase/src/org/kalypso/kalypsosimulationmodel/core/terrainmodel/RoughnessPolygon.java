/**
 * 
 */
package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import javax.xml.namespace.QName;


import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfaceInterpolation;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Dejan Antanaskovic, Patrice Congo
 *
 */
public class RoughnessPolygon implements IRoughnessPolygon 
{
	class GM_PolygonImpl implements GM_Polygon
	{
		private final GM_SurfacePatch surfacePatch;
		
		public GM_PolygonImpl(GM_SurfacePatch surfacePatch)
		{
			this.surfacePatch=surfacePatch;
		}

		/**
		 * @param gmo
		 * @return
		 * @see org.kalypsodeegree.model.geometry.GM_SurfacePatch#contains(org.kalypsodeegree.model.geometry.GM_Object)
		 */
		public boolean contains(GM_Object gmo)
		{
			return surfacePatch.contains(gmo);
		}

		/**
		 * @return
		 * @see org.kalypsodeegree.model.geometry.GM_SurfacePatch#getArea()
		 */
		public double getArea()
		{
			return surfacePatch.getArea();
		}

		/**
		 * @return
		 * @see org.kalypsodeegree.model.geometry.GM_SurfacePatch#getCentroid()
		 */
		public GM_Point getCentroid()
		{
			return surfacePatch.getCentroid();
		}

		/**
		 * @return
		 * @see org.kalypsodeegree.model.geometry.GM_SurfacePatch#getCoordinateSystem()
		 */
		public CS_CoordinateSystem getCoordinateSystem()
		{
			return surfacePatch.getCoordinateSystem();
		}

		/**
		 * @return
		 * @see org.kalypsodeegree.model.geometry.GM_GenericSurface#getEnvelope()
		 */
		public GM_Envelope getEnvelope()
		{
			return surfacePatch.getEnvelope();
		}

		/**
		 * @return
		 * @see org.kalypsodeegree.model.geometry.GM_SurfacePatch#getExteriorRing()
		 */
		public GM_Position[] getExteriorRing()
		{
			return surfacePatch.getExteriorRing();
		}

		/**
		 * @return
		 * @see org.kalypsodeegree.model.geometry.GM_SurfacePatch#getInteriorRings()
		 */
		public GM_Position[][] getInteriorRings()
		{
			return surfacePatch.getInteriorRings();
		}

		/**
		 * @return
		 * @see org.kalypsodeegree.model.geometry.GM_SurfacePatch#getInterpolation()
		 */
		public GM_SurfaceInterpolation getInterpolation()
		{
			return surfacePatch.getInterpolation();
		}

		/**
		 * @return
		 * @see org.kalypsodeegree.model.geometry.GM_GenericSurface#getPerimeter()
		 */
		public double getPerimeter()
		{
			return surfacePatch.getPerimeter();
		}

		/**
		 * @param gmo
		 * @return
		 * @see org.kalypsodeegree.model.geometry.GM_SurfacePatch#intersects(org.kalypsodeegree.model.geometry.GM_Object)
		 */
		public boolean intersects(GM_Object gmo)
		{
			return surfacePatch.intersects(gmo);
		}

		/**
		 * 
		 * @see org.kalypsodeegree.model.geometry.GM_GenericSurface#invalidate()
		 */
		public void invalidate()
		{
			surfacePatch.invalidate();
		}
		
	}
	
	private final Feature feature;
	
	
	/**
	 * Create a RoughnessPolygon object base on an existing
	 * feature
	 * @param feature
	 * @throws IllegalArgumentException if feature is 
	 * 				null and not of the appopriate type
	 */
	public RoughnessPolygon(Feature feature) 
	{
		Assert.throwIAEOnNull(
				feature, "Param feature must not be null");
		Assert.throwIAEOnNotDirectInstanceOf(
				feature, 
				KalypsoModelSimulationBaseConsts.SIM_BASE_F_ROUGHNESS_POLYGON);
		this.feature=feature;
	}
	
	/**
	 * Creates a new RoughnessPolygon for the passed 
	 * work space.
	 * if the work space has root element of the type
	 * RoughnessPolynomCollection than the a new Roughness
	 * polygon should be created and added to that one
	 * otherwise an illegal argument exception should be thrown
	 *  
	 * @param workspace
	 */
	public RoughnessPolygon(
				Feature parentFeature,
				QName linkPropQName) 
	{
		Assert.throwIAEOnNull(
				parentFeature, "Param parentFeature must not be null");
		Assert.throwIAEOnNull(
				linkPropQName, 
				"Parameter linkPropQName must not be null");
		this.feature=
			(Feature)parentFeature.getProperty(linkPropQName);
	}
	
	
	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#getLinestring()
	 */
	public GM_Polygon getPolygon() 
	{
		Object pol=feature.getProperty(
				KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_ROUGHNESS_POLYGON);
		if(pol instanceof GM_Polygon)
		{
			return (GM_Polygon)pol;
		}
		else if(pol instanceof GM_Surface)
		{
			if(((GM_Surface)pol).getNumberOfSurfacePatches()<=0)
			{
				return null;
			}
			else
			{
				try
				{
					GM_SurfacePatch sp=((GM_Surface)pol).getSurfacePatchAt(0);
					return new GM_PolygonImpl(sp);
				}
				catch(Throwable th)
				{
					return null;
				}
				
			}
		}
		else if(pol==null)
		{
			return null;
		}			
		else
		{
			throw new RuntimeException(
						"GM_Polynom expected ut got:"+
						"\n\ttype="+pol.getClass()+
						"\n\tvalue="+pol);
		}
		
		
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#getRoughnessID()
	 */
	public String getRoughnessID() 
	{
		Object id=feature.getProperty(
				KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_ROUGHNESS_ID);
		if(id instanceof String)
		{
			return (String)id;
		}
		else if(id==null)
		{
			return null;
		}			
		else
		{
			throw new RuntimeException(
						"String expected ut got:"+
						"\n\ttype="+id.getClass()+
						"\n\tvalue="+id);
		}
	}
	
	public void setPolygon(GM_Polygon polygon) 
	{
		Assert.throwIAEOnNull(
				polygon, "Paramerter polynom must not be null");
		feature.setProperty(
				KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_ROUGHNESS_POLYGON, 
				polygon);
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#setRougthnessID(java.lang.String)
	 */
	public void setRougthnessID(
						String id) 
						throws IllegalArgumentException 
	{
		Assert.throwIAEOnNullOrEmpty(id);
		feature.setProperty(
				KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_ROUGHNESS_ID, 
				id);

	}
	
	
	@Override
	public String toString() 
	{
		StringBuffer buf= new StringBuffer(128);
		buf.append("RoughnessPolygon");
		String id=feature.getId();
		if(id!=null)
		{
			buf.append('.');
			buf.append(id);
		}
		
		buf.append("[ roughnessID=");
		buf.append(getRoughnessID());
		buf.append(", polygon=");
		buf.append(getPolygon());
		buf.append(']');
		return buf.toString();
	}
	
	@Override
	public boolean equals(Object obj) 
	{
		if(obj instanceof IRoughnessPolygon)
		{
			String id1=getRoughnessID();
			String id2=((IRoughnessPolygon)obj).getRoughnessID();
			if(id1!=null && id2==null)
			{
				return false;
			}
			else if(id1==null && id2!=null)
			{
				return false;
			}
			else if(id1==id2)
			{
				//all null of the same or the same internalized
				//can match
			}
			else if(id1.equals(id2))
			{
				//can match
			}
			else
			{
				return false;
			}
			
			GM_Polygon pol1= getPolygon();
			GM_Polygon pol2=((IRoughnessPolygon)obj).getPolygon();
			if(pol1==pol2)
			{
				return true;
			}
			else if(pol1!=null && pol2==null)
			{
				return false;
			}
			else if(pol1==null && pol2!=null)
			{
				return false;
			}
			else
			{
				return pol1.equals(pol2);
			}
		}
		else
		{
			return super.equals(obj);
		}
	}
	
	public Feature getWrappedFeature()
	{
		return feature;
	}

}
