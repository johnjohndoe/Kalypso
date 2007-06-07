package org.kalypso.kalypsosimulationmodel.core;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

/**
 * Class representing the wbGml:RangeSetFeature element.
 * 
 * The feature contained in the range set need to be adaptable into a
 * {@link RangeSetCls} object
 * 
 * @author Patrice Congo
 */
@SuppressWarnings({"unchecked"})
public class FeatureWrapperCollection<FWCls extends IFeatureWrapper2> extends AbstractFeatureBinder
            implements IFeatureWrapperCollection<FWCls> 
{
	/**
	 * The feature wrapped by this object
	 */
	private final Feature featureCol;

	/**
	 * the list of the feature properties
	 */
	private final FeatureList featureList;

    /**
     * The {@link QName} of the list property of the feature(-collection) 
     */
	private final QName featureMemberProp;

	/**
	 * The class of the feature wrapper in this collection
	 */
	private final Class<FWCls> fwClass;

	/**
	 * Creates a new {@link FeatureWrapperCollection} wrapping the provided
	 * feature
	 * 
	 * @param featureCol
	 *            the feature or feature collection with a list property to wrapp
     * @param fwClass the base class representing the property feature in the list
     * @param featureMemberProp the list property linking the feature and its properties 
	 */
	public FeatureWrapperCollection(Feature featureCol, Class<FWCls> fwClass,
			QName featureMemberProp) {
      
      super( featureCol, featureCol.getFeatureType().getQName() );
      
		Assert.throwIAEOnNull(fwClass, "Parameter fwClass must not be null");

		Assert.throwIAEOnNull(featureCol,
				"Parameter featureCol must not be null");

		Assert.throwIAEOnNull(featureMemberProp,
				"Parameter featureMemberProp must not be null");

		this.fwClass = fwClass;
		this.featureCol = featureCol;
		this.featureMemberProp = featureMemberProp;
		this.featureList = 
				(FeatureList) this.featureCol.getProperty(featureMemberProp);
		  // TODO: the string creation here seems to be a performance problem
			Assert.throwIAEOnNull(
					this.featureList, 
					"could not create the feature list:"+
					"\n\tpropQNAme="+featureMemberProp);
		Assert.throwIAEOnFeaturePropNotList(featureCol, featureMemberProp, null);
	}

	@SuppressWarnings("unchecked")
	public FeatureWrapperCollection( 
					Feature parentFeature, 
					QName childQName,
					QName featureMemberProp, 
					Class<FWCls> fwClass)
					throws IllegalArgumentException 
	{
      super( createSubfeature( parentFeature, childQName, featureMemberProp ), childQName );
      
		Assert.throwIAEOnNull(parentFeature,
				"Parameter parentFeature must not be null");
		Assert.throwIAEOnNull(childQName,
				"Parameter childQName must not be null");

		Assert.throwIAEOnNull(featureMemberProp,
				"Parameter featureMemberProp must not be null");

		try 
		{
			this.featureCol = 
				FeatureHelper.addFeature(
						parentFeature,
						featureMemberProp, 
						childQName);
		} 
		catch (GMLSchemaException ex) 
		{

			throw new IllegalArgumentException(
					"Parent does not accept property of the specified type"
							+ "\n\tparent=" + parentFeature
							+ "\n\tpropertyType=" + featureMemberProp
							+ "\n\tchildType=" + childQName, ex);
		}

		this.featureList = 
          (FeatureList) this.featureCol.getProperty(featureMemberProp);
		this.featureMemberProp = featureMemberProp;

		this.fwClass = fwClass;
	}
    
    private static Feature createSubfeature(        
        Feature parentFeature, 
        QName childQName,
        QName featureMemberProp)
    {
        try 
        {
            return 
                FeatureHelper.addFeature(
                        parentFeature,
                        featureMemberProp, 
                        childQName);
        } 
        catch (GMLSchemaException ex) 
        {

            throw new IllegalArgumentException(
                    "Parent does not accept property of the specified type"
                            + "\n\tparent=" + parentFeature
                            + "\n\tpropertyType=" + featureMemberProp
                            + "\n\tchildType=" + childQName, ex);
        }

    }

	@SuppressWarnings("unchecked")
	public void add(int index, FWCls element) {
		Assert.throwIAEOnNull(element, "argument element must not be null");
		Feature f = element.getWrappedFeature();
		Assert.throwIAEOnNull(f, "wrapped feature must not be null");
		featureList.add(index, f);
	}

	@SuppressWarnings("unchecked")
	public boolean add(FWCls o) {
		Assert.throwIAEOnNull(o, "Argument o must not be null");
		Feature f = o.getWrappedFeature();
		Assert.throwIAEOnNull(f, "wrapped feature must not be null");

		return featureList.add(f);
	}

    public FWCls addNew( final QName newChildType ) {
      return addNew( newChildType, fwClass );
    }
    
	@SuppressWarnings("unchecked")
  public <T extends FWCls> T addNew( final QName newChildType, final Class<T> classToAdapt ) {
		Assert.throwIAEOnNull(newChildType, "newChildType must not null");
        Feature feature=null;
        try {
			 feature= Util.createFeatureForListProp(
                                       featureList,
			                           featureMemberProp, 
                                       newChildType);

             final T wrapper = (T) feature.getAdapter(classToAdapt);
			if (wrapper == null) 
            {
				throw new IllegalArgumentException("Feature not adaptable:"
						+ "\n\tfeatureType=" + newChildType
						+ "\n\tadapatble type=" + fwClass);
			}
            // Feature was already added by Util.create..., so dont add it again
			// featureList.add(feature);
			return wrapper;
		} catch (GMLSchemaException e) {
			throw new IllegalArgumentException(
                "feature:"+feature+ " class="+fwClass+ " featureQName="+newChildType,
                e);
		}
	}
    
    public FWCls addNew( final QName newChildType, final String newFeatureId )
    {
      return addNew( newChildType, newFeatureId, fwClass );
    }

    
    @SuppressWarnings("unchecked")
    public <T extends FWCls> T addNew( final QName newChildType, final String newFeatureId, final Class<T> classToAdapt ) 
    {
        Assert.throwIAEOnNull(newChildType, "newChildType must not null");
        
        Feature feature=null;
        try {
          
            feature=Util.createFeatureWithId( 
                          newChildType,//newFeatureQName, 
                          featureCol,//parentFeature, 
                          featureMemberProp,//propQName, 
                          newFeatureId//gmlID 
                          );
            
            final T wrapper = (T) feature.getAdapter(classToAdapt);
            if (wrapper == null) 
            {
                throw new IllegalArgumentException("Feature not adaptable:"
                        + "\n\tfeatureType=" + newChildType
                        + "\n\tadapatble type=" + fwClass);
            }
            return wrapper;
        } catch (Exception e) {
            throw new IllegalArgumentException(
                      "Feature="+feature+" class="+fwClass,e);
        }
    }

    public FWCls addNew( final int index, final QName newChildType )
    {
      return addNew( index, newChildType, fwClass );
    }
    
	@SuppressWarnings("unchecked")
  public <T extends FWCls> T addNew( final int index, final QName newChildType, final Class<T> classToAdapt ) {
		Assert.throwIAEOnNull(newChildType, "newChildType must not null");
		try {
			final Feature feature = Util.createFeatureForListProp(featureList,
					featureMemberProp, newChildType);
			final T wrapper = (T) feature.getAdapter(classToAdapt);
			if (wrapper == null) {
				throw new IllegalArgumentException("Feature not adaptable:"
						+ "\n\tfeatureType=" + newChildType
						+ "\n\tadapatble type=" + fwClass);
			}

			featureList.add(index, feature);

			return wrapper;
		} catch (GMLSchemaException e) {
			throw new IllegalArgumentException(e);
		}
	}

	@SuppressWarnings("unchecked")
	public boolean addAll(Collection<? extends FWCls> c) {
		Assert.throwIAEOnNull(c, "Argument c must not be null");
		return featureList.addAll(Util.toFeatureList(c));

	}

	@SuppressWarnings("unchecked")
	public boolean addAll(int index, Collection<? extends FWCls> c) {
		Assert.throwIAEOnNull(c, "Argument c must not be null");
		return featureList.addAll(index, Util.toFeatureList(c));
	}

	public void clear() {
		featureList.clear();
	}

	public boolean contains(Object o) {
		return indexOf(o) != -1;
	}

	@SuppressWarnings("unchecked")
	public boolean containsAll(Collection<?> c) {
		throw new UnsupportedOperationException();
		/* TODO: see comment at 'contains'
		/* return featureList.containsAll(c);
         */    
        /*TOASK 
		 * i do not understand that one.
         * and featureList.containsAll(c) will only work
         * if featureList contains real features or references  
		 */
	}

	@SuppressWarnings("unchecked")
	public FWCls get(int index) {
		final Feature f = FeatureHelper.getFeature(featureCol.getWorkspace(),
				featureList.get(index));

		FWCls adapted = (FWCls) f.getAdapter(fwClass);
		return adapted;
	}

	public int indexOf(Object o) {
		if (o instanceof IFeatureWrapper2) {
			// The following line does not work, because the feature list
			// may
			// contain strings (i.e. references to features)
			// return featureList.indexOf(((IFeatureWrapper) o)
			// .getWrappedFeature());

			// We do trust in the equals implementation of
			// AbstractFeatureBinder
            /*TODO 
             * may be we should put equals() into the iwrapper
             * interface because all ifeaturewrapper must not
             * extends AbtractFeatureBinder, 
             * and this will therefore work, i guess, only for
             * AbstractFeatureBinder childs. i will have a look
             * during the move IfeatureWrapper to kalypso deegree 
             * 
             */
			for (int i = 0; i < size(); i++) {
				final FWCls cls = get(i);
				if (cls.equals(o))
					return i;
			}
		}

		return -1;
	}

	public boolean isEmpty() {
		return featureList.isEmpty();
	}

	public Iterator<FWCls> iterator() {
		return new Iterator<FWCls>() {
			private final Iterator it = featureList.iterator();
			private final GMLWorkspace workspace=featureCol.getWorkspace();
			synchronized public boolean hasNext() {
				return it.hasNext();
			}

			@SuppressWarnings({ "unchecked", "synthetic-access" })
			synchronized public FWCls next() {
				final Object next = it.next();
                final Feature f = FeatureHelper.getFeature( workspace, next);

				FWCls wrapper = (FWCls) f.getAdapter(fwClass);
				if (wrapper == null) {
					throw new RuntimeException("Feature " + f
							+ " could not be adapted to " + fwClass);
				}
				return wrapper;
			}

			public void remove() {
				it.remove();
			}

		};
	}

	public int lastIndexOf(Object o) {
		if (o instanceof IFeatureWrapper2) {
			return featureList.lastIndexOf(((IFeatureWrapper2) o)
					.getWrappedFeature());
		} else {
			return -1;
		}
	}

	public ListIterator<FWCls> listIterator() {
		return listIterator(0);
	}

	public ListIterator<FWCls> listIterator(final int index) {
		return new ListIterator<FWCls>() {
			private final ListIterator lit = featureList.listIterator(index);

			@SuppressWarnings("unchecked")
			public void add(FWCls o) {
				lit.add(o.getWrappedFeature());
			}

			public boolean hasNext() {
				return lit.hasNext();
			}

			public boolean hasPrevious() {
				return lit.hasPrevious();
			}

			@SuppressWarnings("unchecked")
			public FWCls next() {
				final Feature f = FeatureHelper.getFeature(featureCol
						.getWorkspace(), lit.next());
				Object wrapper = f.getAdapter(fwClass);
				return (FWCls) wrapper;
			}

			public int nextIndex() {
				return lit.nextIndex();
			}

			@SuppressWarnings("unchecked")
			public FWCls previous() {
				Feature f = (Feature) lit.previous();
				Object wrapper = f.getAdapter(fwClass);
				return (FWCls) wrapper;
			}

			public int previousIndex() {
				return lit.previousIndex();
			}

			public void remove() {
				lit.remove();
			}

			@SuppressWarnings("unchecked")
			public void set(FWCls o) {
				lit.set(o.getWrappedFeature());
			}

		};
	}

	@SuppressWarnings("unchecked")
	public FWCls remove(int index) {
//		Feature f = (Feature) featureList.remove(index);
//		IFeatureWrapper2 wrapper = (IFeatureWrapper2) f.getAdapter(fwClass);
	  FWCls wrapper = FeatureHelper.getFeature( 
	      featureCol.getWorkspace(), 
	      featureList.remove(index), 
	      fwClass );
	  return wrapper;
	}

	public boolean remove(Object o) {
		if (o instanceof IFeatureWrapper2) {
			return featureList
					.remove(((IFeatureWrapper2) o).getWrappedFeature());
		}
        else if(o instanceof String)
        {
          return featureList.remove( o );
        }else {
			return featureList.remove( o );
		}
	}

	public boolean removeAll(Collection<?> c) {
		boolean ret = false;
		for (Object o : c) {
			Assert.throwIAEOnNull(o,
					"Collection must not contain a null element");
			ret = ret || remove(o);
		}
		return ret;
	}

	public boolean retainAll(Collection<?> c) {
		throw new UnsupportedOperationException();
	}

	@SuppressWarnings("unchecked")
	public FWCls set(int index, FWCls element) {
		FWCls r = get(index);
		Feature f = element.getWrappedFeature();
		Assert.throwIAEOnNull(f, "Wrapped feature must not be null");

		featureList.set(index, f);
		return r;
	}

	public int size() {
		return featureList.size();
	}

	public List<FWCls> subList(int fromIndex, int toIndex) {
		return null;
	}

	public Object[] toArray() 
	{
		int i = size();
		Object objs[] = new Object[i];
		Feature f;
		Object fObj;
		for (i--; i >= 0; i--) {
			fObj=featureList.get(i);
			if(fObj instanceof Feature)
			{
				f = (Feature) fObj;
			}
			else if(fObj instanceof String)
			{
				f=featureCol.getWorkspace().getFeature((String)fObj);
			}
			else
			{
				throw new RuntimeException("Type not known:"+fObj);
			}
			objs[i] = f.getAdapter(fwClass);
		}
		return objs;
	}

	@SuppressWarnings("unchecked")
	public <T> T[] toArray(T[] a) {
		final int SIZE = size();
		Class compType = a.getClass().getComponentType();
		if (!compType.isAssignableFrom(fwClass)) {
			throw new ArrayStoreException();
		}

		if (a.length < SIZE) {
			a = (T[]) Array.newInstance(compType, SIZE);
		}
		for (int i = SIZE - 1; i >= 0; i--) {
			a[i] = (T) get(i);
		}

		if (a.length > SIZE) {
			a[SIZE] = null;
		}
		return a;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof FeatureWrapperCollection) {
			return featureList
					.equals(((FeatureWrapperCollection) obj).featureList);
		} else if (obj instanceof IFeatureWrapperCollection) {
			IFeatureWrapperCollection frs = (IFeatureWrapperCollection) obj;
			final int SIZE = size();
			if (SIZE != frs.size()) {
				return false;
			}
			for (int i = SIZE - 1; i >= 0; i--) {
				if (!get(i).equals(frs.get(0))) {
					return false;
				}
			}
			return true;
		} else {
			return super.equals(obj);
		}
	}

	public Feature getWrappedFeature() {
		return featureCol;
	}
	
    /**
     * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper#getGmlID()
     */
    public String getGmlID( )
    {
      return featureCol.getId();
    }
    
	public FeatureList getWrappedList()
	{
		return featureList;
	}
    
    /**
     * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection#removeAllRefs(org.kalypsodeegree.model.feature.binding.IFeatureWrapper)
     */
	public boolean removeAllRefs( FWCls toRemove )
                                  throws IllegalArgumentException 
    {
      if(toRemove == null)
      {
        throw new IllegalArgumentException(
            "Parameter toRemove must not be null");
      }
      
      final String gmlID = toRemove.getGmlID();
      boolean removed = false;
      boolean currentRemove = false;
      do
      {      
        currentRemove = featureList.remove( gmlID );
        removed = removed || currentRemove;
      }
      while(currentRemove);
      return removed;
      
    }
    
    /**
     * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection#addRef(org.kalypsodeegree.model.feature.binding.IFeatureWrapper)
     */
	public boolean addRef(  FWCls toAdd )
                            throws IllegalArgumentException
    {
      final String gmlID = toAdd.getGmlID();
      if(featureList.contains( gmlID ))
      {
        return false;
      }
      return featureList.add(gmlID);
    }

    /**
     * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection#query(org.kalypsodeegree.model.geometry.GM_Surface, boolean, javax.xml.namespace.QName)
     */
    public List<FWCls> query( GM_Surface selectionSurface, boolean containedOnly, QName checkedGeometryPropertyName )
    {
      final List selectedFeature = featureList.query( selectionSurface.getEnvelope() , null );
      final List<FWCls> selFW = new ArrayList<FWCls>(selectedFeature.size());
      final GMLWorkspace workspace = featureCol.getWorkspace();
      
      for( Object linkOrFeature: selectedFeature )
      {
        final FWCls feature = FeatureHelper.getFeature( workspace, linkOrFeature, fwClass );
        if( feature != null )
        {
          final Object prop= 
            feature.getWrappedFeature().getProperty( checkedGeometryPropertyName );
          if( prop instanceof GM_Object )
          {
            if( containedOnly )
            {
              if( selectionSurface.contains( (GM_Object) prop ) )
              {
                  selFW.add( feature );
              }
              
            }
            else
            {
              if( selectionSurface.intersects( (GM_Object) prop ) )
              {
                  selFW.add( feature );
              }
            }
          }
        }
      }
      
      return selFW;
    }

    /**
     * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection#query(org.kalypsodeegree.model.geometry.GM_Envelope)
     */
    
    public List<FWCls> query( GM_Envelope envelope )
    {
      final List selectedFeature = featureList.query( envelope , null );
      final List<FWCls> selFW = new ArrayList<FWCls>(selectedFeature.size());
      final GMLWorkspace workspace = featureCol.getWorkspace();
      
      for( Object linkOrFeature: selectedFeature )
      {
        final FWCls feature = FeatureHelper.getFeature( workspace, linkOrFeature, fwClass );
        if( feature != null )
        {
          selFW.add( feature );
        }
      }      
      return selFW;
    }

    /**
     * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection#query(org.kalypsodeegree.model.geometry.GM_Position)
     */
    public List<FWCls> query( GM_Position position )
    {
      final List selectedFeature = featureList.query( position , null );
      final List<FWCls> selFW = new ArrayList<FWCls>(selectedFeature.size());
      final GMLWorkspace workspace = featureCol.getWorkspace();
      
      for( Object linkOrFeature: selectedFeature )
      {
        final FWCls feature = FeatureHelper.getFeature( workspace, linkOrFeature, fwClass );
        if( feature != null )
        {
          selFW.add( feature );
        }
      }
      
      return selFW;
    }
    
    
}
