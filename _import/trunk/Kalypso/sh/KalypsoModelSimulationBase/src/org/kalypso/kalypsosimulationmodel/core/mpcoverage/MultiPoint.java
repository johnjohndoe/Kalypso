/**
 * 
 */
package org.kalypso.kalypsosimulationmodel.core.mpcoverage;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypso.kalypsosimulationmodel.schema.GmlImitationsConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Default implementation of {@link IMultiPoint}
 * 
 * @author Patrice Congo
 *
 */
public class MultiPoint implements IMultiPoint
{
	/**
	 * wrapped multipoint  feature 
	 */
	final private Feature multiPointFeature;
	
	/**
	 * holds the gm points in the multi point feature
	 */
	final ArrayList<GM_Point> points;
	
	/**
	 * Creates a multipoint object wrapped the given feature
	 * 
	 * @param multiPointFeature -- the multipoint feature to wrapp
	 */
	@SuppressWarnings("unchecked")
	public MultiPoint(Feature multiPointFeature)
	{
		if(multiPointFeature==null)
		{
			throw new IllegalArgumentException(
					"Argument multipoint feature must not be null");
		}
		
		if(!Util.directInstanceOf(
				multiPointFeature, 
				GmlImitationsConsts.WBGML_F_MULTIPOINT))
		{
			throw new IllegalArgumentException(
					"multiPoint  feature must be of type "+
					GmlImitationsConsts.WBGML_F_MULTIPOINT+
					"; the current type is:"+multiPointFeature.getFeatureType().getQName());
		}
		
		this.multiPointFeature=multiPointFeature;
		this.points=
			(ArrayList<GM_Point>)
				this.multiPointFeature.getProperty(
					GmlImitationsConsts.GML_PROP_POINT_MEMBER);
	}
	
	
	@SuppressWarnings("unchecked")
	public MultiPoint(
			Feature parentFeature, 
			QName propQName)
			throws IllegalArgumentException
	{
		if(parentFeature==null || propQName==null)
		{
			throw new IllegalArgumentException(
					"Argument parentFeature and propQName must not be null:"+
					"\n\tparentFeature="+parentFeature+
					"\n\tpropQName="+propQName);
		}
//		GMLWorkspace workspace= parentFeature.getWorkspace();
//		IFeatureType featureType=
//			workspace.getGMLSchema().getFeatureType(
//					GmlImitationsConsts.WBGML_F_MULTIPOINT);
//		
//		IFeatureType type=null;
//		parentFeature.getWorkspace().createFeature(parentFeature, type);
		
		try
		{
			this.multiPointFeature=
				FeatureHelper.addFeature(
					parentFeature, 
					propQName, 
					GmlImitationsConsts.WBGML_F_MULTIPOINT);
		}
		catch(GMLSchemaException ex)
		{
			 
			throw new IllegalArgumentException(
					"Property "+propQName+" does not accept element of type"+
					GmlImitationsConsts.WBGML_F_MULTIPOINT,
					ex);
		}
//		
//		this.multiPointFeature=
//			FeatureFactory.createFeature(
//					workspace.getRootFeature(),//parent, 
//					"MP"+System.currentTimeMillis(),//TODO better ids , random? 
//					featureType, 
//					true);
		
		
		
		this.points = (ArrayList<GM_Point>)
						this.multiPointFeature.getProperty(
							GmlImitationsConsts.GML_PROP_POINT_MEMBER);
		
	}
	
	/* (non-Javadoc)
	 * @see java.util.List#add(java.lang.Object)
	 */
	public boolean add(GM_Point o)
	{
		return points.add(o);
	}

	/* (non-Javadoc)
	 * @see java.util.List#add(int, java.lang.Object)
	 */
	public void add(int index, GM_Point element)
	{
		points.add(index, element);
	}

	/* (non-Javadoc)
	 * @see java.util.List#addAll(java.util.Collection)
	 */
	public boolean addAll(Collection<? extends GM_Point> c)
	{
		
		return points.addAll(c);
	}

	/* (non-Javadoc)
	 * @see java.util.List#addAll(int, java.util.Collection)
	 */
	public boolean addAll(int index, Collection<? extends GM_Point> c)
	{
		return points.addAll(index,c);
	}

	/* (non-Javadoc)
	 * @see java.util.List#clear()
	 */
	public void clear()
	{
		points.clear();

	}

	/* (non-Javadoc)
	 * @see java.util.List#contains(java.lang.Object)
	 */
	public boolean contains(Object o)
	{
		return points.contains(o);
	}

	/* (non-Javadoc)
	 * @see java.util.List#containsAll(java.util.Collection)
	 */
	public boolean containsAll(Collection<?> c)
	{
		return points.containsAll(c);
	}

	/* (non-Javadoc)
	 * @see java.util.List#get(int)
	 */
	public GM_Point get(int index)
	{
		return points.get(index);
	}

	/* (non-Javadoc)
	 * @see java.util.List#indexOf(java.lang.Object)
	 */
	public int indexOf(Object o)
	{
		return points.indexOf(o);
	}

	/* (non-Javadoc)
	 * @see java.util.List#isEmpty()
	 */
	public boolean isEmpty()
	{
		return points.isEmpty();
	}

	/* (non-Javadoc)
	 * @see java.util.List#iterator()
	 */
	public Iterator<GM_Point> iterator()
	{
		return points.iterator();
	}

	/* (non-Javadoc)
	 * @see java.util.List#lastIndexOf(java.lang.Object)
	 */
	public int lastIndexOf(Object o)
	{
		return points.lastIndexOf(o);
	}

	/* (non-Javadoc)
	 * @see java.util.List#listIterator()
	 */
	public ListIterator<GM_Point> listIterator()
	{
		return points.listIterator();
	}

	/* (non-Javadoc)
	 * @see java.util.List#listIterator(int)
	 */
	public ListIterator<GM_Point> listIterator(int index)
	{
		return points.listIterator(index);
	}

	/* (non-Javadoc)
	 * @see java.util.List#remove(java.lang.Object)
	 */
	public boolean remove(Object o)
	{
		return points.remove(o);
	}

	/* (non-Javadoc)
	 * @see java.util.List#remove(int)
	 */
	public GM_Point remove(int index)
	{
		return points.remove(index);
	}

	/* (non-Javadoc)
	 * @see java.util.List#removeAll(java.util.Collection)
	 */
	public boolean removeAll(Collection<?> c)
	{
		return points.removeAll(c);
	}

	/* (non-Javadoc)
	 * @see java.util.List#retainAll(java.util.Collection)
	 */
	public boolean retainAll(Collection<?> c)
	{
		return points.retainAll(c);
	}

	/* (non-Javadoc)
	 * @see java.util.List#set(int, java.lang.Object)
	 */
	public GM_Point set(int index, GM_Point element)
	{
		return points.set(index, element);
	}

	/* (non-Javadoc)
	 * @see java.util.List#size()
	 */
	public int size()
	{
		return points.size();
	}

	/* (non-Javadoc)
	 * @see java.util.List#subList(int, int)
	 */
	public List<GM_Point> subList(int fromIndex, int toIndex)
	{
		return points.subList(fromIndex, toIndex);
	}

	/* (non-Javadoc)
	 * @see java.util.List#toArray()
	 */
	public Object[] toArray()
	{
		return points.toArray();
	}

	/* (non-Javadoc)
	 * @see java.util.List#toArray(T[])
	 */
	public <T> T[] toArray(T[] a)
	{
		return points.toArray(a);
	}
	@Override
	public String toString()
	{
		StringBuffer buf= new StringBuffer(128);
		buf.append(GmlImitationsConsts.WBGML_F_MULTIPOINT.getPrefix());
		buf.append('.');
		buf.append(GmlImitationsConsts.WBGML_F_MULTIPOINT.getLocalPart());
		String id=multiPointFeature.getId();
		if(id!=null)
		{
			buf.append('.');
			buf.append(id);
		}
		buf.append(points);
		
		return buf.toString();
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(obj instanceof MultiPoint)
		{
			if(this==obj)
			{
				return true;
			}
			else
			{
				return points.equals(((MultiPoint)obj).points);
			}
			
		}
		if(obj instanceof IMultiPoint)
		{
			if(size()==((IMultiPoint)obj).size())
			{
				for(int i=size()-1;i>=0;i--)
				{
					if(!get(0).equals(((IMultiPoint)obj).get(0)))
					{
						return false;
					}
				}
				return true;
			}
			return false;
		}
		else
		{
			return super.equals(obj);
		}
	}
}
