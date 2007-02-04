package org.kalypso.kalypsosimulationmodel.core.mpcoverage;

import java.lang.reflect.Array;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import javax.xml.namespace.QName;


import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypso.kalypsosimulationmodel.schema.GmlImitationsConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Class representing the wbGml:RangeSetFeature element.
 * 
 * The feature contained in the range set need to be adaptable 
 * into a {@link RangeSetCls} object
 * 
 * @author Patrice Congo
 */
public class FeatureRangeSet<RangeSetCls extends IFeatureWrapper> 
				implements IFeatureRangeSet<RangeSetCls>
{
	/**
	 * The feature wrapped by this object
	 */
	private final Feature rsFeature;
	
	/**
	 * the list of features in the range set
	 */
	private final FeatureList rsFeatureList;
	
	/** class of the range set objects*/
	private final Class<RangeSetCls> rangeSetClass;
	
	/**
	 *Creates a new {@link FeatureRangeSet} wrapping the provided feature
	 *@param rsFeature the range set feature to wrapp 
	 */
	public FeatureRangeSet(Feature rsFeature, Class<RangeSetCls> rangeSetClass)
	{
		Assert.throwIAEOnNull(
				rangeSetClass, "Argument rangeSetClass must not be null");
		Assert.throwIAEOnNull(
				rsFeature, "Argument rsFeature must not be null");
		Assert.throwIAEOnNotDirectInstanceOf(
				rsFeature, GmlImitationsConsts.WBGML_F_FEATURERANGESET);
		
		this.rangeSetClass=rangeSetClass;
		this.rsFeature=rsFeature;
		this.rsFeatureList=
			(FeatureList)this.rsFeature.getProperty(
					GmlImitationsConsts.GML_PROP_FEATURE_MEMBER);
	}

	@SuppressWarnings("unchecked")
	public FeatureRangeSet(
			Feature parentFeature, 
			QName propQName,
			Class<RangeSetCls> rangeSetClass)
			throws IllegalArgumentException
	{
		if(parentFeature==null || propQName==null)
		{
			throw new IllegalArgumentException(
					"Argument parentFeature and propQName must not be null:"+
					"\n\tparentFeature="+parentFeature+
					"\n\tpropQName="+propQName);
		}
		
		try
		{
			this.rsFeature=
				FeatureHelper.addFeature(
					parentFeature, 
					propQName, 
					GmlImitationsConsts.WBGML_F_FEATURERANGESET);
		}
		catch(GMLSchemaException ex)
		{
			 
			throw new IllegalArgumentException(
					"Property "+propQName+" does not accept element of type"+
					GmlImitationsConsts.WBGML_F_FEATURERANGESET,
					ex);
		}
	
		this.rsFeatureList = (FeatureList)
						this.rsFeature.getProperty(
							GmlImitationsConsts.GML_PROP_FEATURE_MEMBER);
		this.rangeSetClass=rangeSetClass;
	}
	
	
	@SuppressWarnings("unchecked")
	public void add(int index, RangeSetCls element)
	{
		Assert.throwIAEOnNull(
						element, 
						"argument element must not be null");
		Feature f=element.getWrappedFeature();
		Assert.throwIAEOnNull(f, "wrapped feature must not be null");
		rsFeatureList.add(index, f);		
	}

	@SuppressWarnings("unchecked")
	public boolean add(RangeSetCls o)
	{
		Assert.throwIAEOnNull(o, "Argument o must not be null");
		Feature f= o.getWrappedFeature();
		Assert.throwIAEOnNull(f, "wrapped feature must not be null");
		
		return rsFeatureList.add(f);
	}

	@SuppressWarnings("unchecked")
	public boolean addAll(Collection<? extends RangeSetCls> c)
	{
		Assert.throwIAEOnNull(c, "Argument c must not be null");
		return rsFeatureList.addAll(Util.toFeatureList(c));
		
	}

	@SuppressWarnings("unchecked")
	public boolean addAll(
						int index, 
						Collection<? extends RangeSetCls> c)
	{
		Assert.throwIAEOnNull(c, "Argument c must not be null");
		return rsFeatureList.addAll(index,Util.toFeatureList(c));
	}

	public void clear()
	{
		rsFeatureList.clear();		
	}

	public boolean contains(Object o)
	{
		if(o instanceof IFeatureWrapper)
		{
			return rsFeatureList.contains(
						((IFeatureWrapper)o).getWrappedFeature());
		}
		else
		{
			return false;
		}
	}

	@SuppressWarnings("unchecked")
	public boolean containsAll(Collection<?> c)
	{
		return rsFeatureList.containsAll(c);
	}

	@SuppressWarnings("unchecked")
	public RangeSetCls get(int index)
	{
		Feature f=(Feature)rsFeatureList.get(index);
		RangeSetCls adapted=(RangeSetCls)f.getAdapter(rangeSetClass);
		return adapted;
	}

	public int indexOf(Object o)
	{
		if(o instanceof IFeatureWrapper)
		{
			return rsFeatureList.indexOf(
						((IFeatureWrapper)o).getWrappedFeature());
		}
		else
		{
			return -1;
		}
	}

	public boolean isEmpty()
	{
		return rsFeatureList.isEmpty();
	}

	public Iterator<RangeSetCls> iterator()
	{
		return new Iterator<RangeSetCls>()
		{
			private final Iterator it=
							rsFeatureList.iterator();
			public boolean hasNext()
			{
				return it.hasNext();
			}

			@SuppressWarnings("unchecked")
			public RangeSetCls next()
			{
				Feature f= (Feature)it.next();
				RangeSetCls wrapper= (RangeSetCls)f.getAdapter(rangeSetClass);
				if(wrapper==null)
				{
					throw new RuntimeException(
							"Feature "+f+" could not be adapted to "+rangeSetClass);
				}
				return wrapper;
			}

			public void remove()
			{
				it.remove();
			}
			
		};
	}

	public int lastIndexOf(Object o)
	{
		if(o instanceof IFeatureWrapper)
		{
			return rsFeatureList.lastIndexOf(
					((IFeatureWrapper)o).getWrappedFeature());
		}
		else
		{
			return -1;
		}
	}

	public ListIterator<RangeSetCls> listIterator()
	{
		return listIterator(0);
	}

	public ListIterator<RangeSetCls> listIterator(final int index)
	{
		return new ListIterator<RangeSetCls>()
		{
			private final ListIterator lit=rsFeatureList.listIterator(index);
			@SuppressWarnings("unchecked")
			public void add(RangeSetCls o)
			{
				lit.add(o.getWrappedFeature());
			}

			public boolean hasNext()
			{
				return lit.hasNext();
			}

			public boolean hasPrevious()
			{
				return lit.hasPrevious();
			}

			@SuppressWarnings("unchecked")
			public RangeSetCls next()
			{
				Feature f= (Feature)lit.next();
				Object wrapper=f.getAdapter(rangeSetClass);
				return (RangeSetCls)wrapper;
			}

			public int nextIndex()
			{
				return lit.nextIndex();
			}

			@SuppressWarnings("unchecked")
			public RangeSetCls previous()
			{
				Feature f= (Feature)lit.previous();
				Object wrapper=f.getAdapter(rangeSetClass);
				return (RangeSetCls)wrapper;
			}

			public int previousIndex()
			{
				return lit.previousIndex();
			}

			public void remove()
			{
				lit.remove();
			}

			@SuppressWarnings("unchecked")
			public void set(RangeSetCls o)
			{
				lit.set(o.getWrappedFeature());
			}
			
		};
	}

	@SuppressWarnings("unchecked")
	public RangeSetCls remove(int index)
	{
		Feature f= (Feature)rsFeatureList.remove(index);
		IFeatureWrapper wrapper=
				(IFeatureWrapper)f.getAdapter(rangeSetClass);
		return (RangeSetCls)wrapper;
	}

	public boolean remove(Object o)
	{
		if(o instanceof IFeatureWrapper)
		{
			return rsFeatureList.remove(
					((IFeatureWrapper)o).getWrappedFeature());
		}
		else
		{
			return false;
		}
	}

	public boolean removeAll(Collection<?> c)
	{
		boolean ret=false;
		for(Object o:c)
		{
			Assert.throwIAEOnNull(
					o, "Collection must not contain a null element");
			ret=ret||remove(o);
		}
		return ret;
	}

	public boolean retainAll(Collection<?> c)
	{
		throw new UnsupportedOperationException();
	}

	@SuppressWarnings("unchecked")
	public RangeSetCls set(int index, RangeSetCls element)
	{
		RangeSetCls r=get(index);
		Feature f=element.getWrappedFeature();
		Assert.throwIAEOnNull(f, "Wrapped feature must not be null");
		
		rsFeatureList.set(index, f);
		return r;
	}

	public int size()
	{
		return rsFeatureList.size();
	}

	public List<RangeSetCls> subList(int fromIndex, int toIndex)
	{
		return null;
	}

	public Object[] toArray()
	{
		int i=size(); 
		Object objs[]=new Object[i];
		Feature f;
		for(i--;i>=0;i--)
		{
			f=(Feature)rsFeatureList.get(i);
			objs[i]=f.getAdapter(rangeSetClass);
		}
		return objs;
	}

	@SuppressWarnings("unchecked")
	public <T> T[] toArray(T[] a)
	{
		final int  SIZE=size();
		Class compType=a.getClass().getComponentType();
		if(!compType.isAssignableFrom(rangeSetClass))
		{
			throw new ArrayStoreException();
		}
		
		if (a.length < SIZE)
		{
            a = (T[])Array.newInstance(compType, SIZE);
		}
		for(int i=SIZE-1;i>=0;i--)
		{
			a[i]=(T)get(i);
		}
		
		if (a.length > SIZE)
        {
            a[SIZE] = null;
        }
        return a;		
	}	
	
	@Override
	public boolean equals(Object obj)
	{
		if(obj instanceof FeatureRangeSet)
		{
			return rsFeatureList.equals(((FeatureRangeSet)obj).rsFeatureList);
		}
		else if(obj instanceof IFeatureRangeSet)
		{
			IFeatureRangeSet frs=(IFeatureRangeSet)obj;
			final int SIZE=size(); 
			if(SIZE!=frs.size())
			{
				return false;
			}
			for(int i=SIZE-1;i>=0;i--)
			{
				if(!get(i).equals(frs.get(0)))
				{
					return false;
				}
			}
			return true;
		}			
		else
		{
			return super.equals(obj);
		}
	}
	
	public Feature getWrappedFeature()
	{
		return rsFeature;
	}
    
    /**
     * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper#getGmlID()
     */
    public String getGmlID( )
    {
      return rsFeature.getId();
    }
}
