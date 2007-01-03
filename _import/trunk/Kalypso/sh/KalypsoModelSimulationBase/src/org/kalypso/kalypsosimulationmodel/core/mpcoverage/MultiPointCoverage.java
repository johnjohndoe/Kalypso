package org.kalypso.kalypsosimulationmodel.core.mpcoverage;

import java.util.ArrayList;
import java.util.List;

import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypso.kalypsosimulationmodel.schema.GmlImitationsConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * 
 * @author Patrice Congo
 */
public class MultiPointCoverage<RangSetCls extends IFeatureWrapper> implements IMultipointCoverage<RangSetCls>
{

	private final Feature mpFeature;
	private final FeatureRangeSet<RangSetCls> rangeSet;
	private final MultiPoint domainSet;
	//private final Class rangeSetClass;
	
	public MultiPointCoverage(
					Feature mpFeature,
					Class<RangSetCls> rangeSetClass)
					throws IllegalArgumentException
	{
		Assert.throwIAEOnNull(
				mpFeature, "Argument mpFeature must not be null");
		Assert.throwIAEOnNull(
				rangeSetClass, "Argument rangeSetClass must not be null");
		
		if(!Util.directInstanceOf(
					mpFeature, 
					GmlImitationsConsts.WBGML_F_MULTIPOINT_COVERAGE))
		{
			throw new IllegalArgumentException(
					"Feature of type "+
						GmlImitationsConsts.WBGML_F_MULTIPOINT_COVERAGE+
					" expected but got "+mpFeature.getFeatureType().getQName());
		}
		
		//this.rangeSetClass=rangeSetClass;
		this.mpFeature=mpFeature;		
		MultiPoint mp= 
			new MultiPoint(
					(Feature)mpFeature.getProperty(
							GmlImitationsConsts.WBGML_PROP_MULTIPOINT_DOMAIN)
//				mpFeature,
//				GmlImitationsConsts.WBGML_PROP_MULTIPOINT_DOMAIN
				);
		FeatureRangeSet<RangSetCls> fl=
			new FeatureRangeSet<RangSetCls>(
				(Feature)mpFeature.getProperty(
							GmlImitationsConsts.WBGML_PROP_RANGESET),
				rangeSetClass);
		if(mp.size()!=fl.size())
		{
			throw new IllegalArgumentException(
					"Size mismacth DomainSet.size="+mp.size()+
					" RangeSetSize.size="+fl.size());
		}
		this.domainSet=mp;
		this.rangeSet=fl;
	}

	public void addCoverageEntry(
					RangSetCls rangeValue, 
					GM_Point position) 
					throws IllegalArgumentException
	{
		Assert.throwIAEOnNull(
				rangeValue, 
				"Argument rangeValue must not be null");
		Assert.throwIAEOnNull(position, "Argument position must not be null");
		rangeSet.add(rangeValue);
		domainSet.add(position);
	}

	public List<GM_Point> getApplicablePosition(
									RangSetCls rangeValue) 
									throws IllegalArgumentException
	{
		Assert.throwIAEOnNull(
						rangeValue, 
						"rangeValue must not be null");
		final int SIZE=rangeSet.size();
		List<GM_Point> pointList= new ArrayList<GM_Point>(SIZE);
		for(int i=0;i<SIZE;i++)
		{
			if(rangeValue.equals(rangeSet.get(i)))
			{
				pointList.add(domainSet.get(i));
			}
		}
		return pointList;
	}

	public MultiPoint getDomain()
	{
		return domainSet;
	}

	public RangSetCls getRangeValue(
							GM_Point location) 
							throws IllegalArgumentException
	{
		Assert.throwIAEOnNull(
				location, "Argument location must not be null");
		int index=domainSet.indexOf(location);
		if(index>=0)
		{
			return rangeSet.get(index);
		}
		else
		{
			return null;
		}
	}

	public List<RangSetCls> getRangeValues(
								GM_Polygon region) 
								throws IllegalArgumentException
	{
		final int SIZE=domainSet.size();
		List<RangSetCls> rs= new ArrayList<RangSetCls>(SIZE);
		for(int i=0;i<SIZE;i++)
		{
			if(region.contains(domainSet.get(i)))
			{
				rs.add(rangeSet.get(i));
			}
		}
		return rs;
	}

	public void removeCoveredPosition(
							GM_Point position) throws IllegalArgumentException
	{
		Assert.throwIAEOnNull(
				position, "Argument position must not be null");
		for(
				int index=domainSet.size()-1;
				index>=0;
				index--)
			{
				if(position.equals(domainSet.get(index)))
				{
					rangeSet.remove(index);
					domainSet.remove(index);
				}
			}
	}

	public void removeRangeValue(
						RangSetCls rangeValue) throws IllegalArgumentException
	{
		Assert.throwIAEOnNull(
				rangeValue, "Argument range value must not be null");
		for(
			int index=rangeSet.size()-1;
			index>=0;
			index--)
		{
			if(rangeValue.equals(rangeSet.get(index)))
			{
				rangeSet.remove(index);
				domainSet.remove(index);
			}
		}
	}
	
	public Feature getWrappedFeature()
	{
		return mpFeature;
	}

}
