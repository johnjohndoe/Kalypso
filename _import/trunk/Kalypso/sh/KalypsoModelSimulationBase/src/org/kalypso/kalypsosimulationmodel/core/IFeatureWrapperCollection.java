package org.kalypso.kalypsosimulationmodel.core;

import java.util.List;

import javax.xml.namespace.QName;

import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper;

/**
 * Interface to be implemented by classes that wrapped a feature collection to
 * provided a view as a {@link List} of {@link FWCls}
 * 
 * @author Patrice Congo
 * 
 */
public interface IFeatureWrapperCollection<FWCls extends IFeatureWrapper>
		extends IFeatureWrapper, List<FWCls> {
	/**
	 * Creates and Adds a new element of the specified type into the feature
	 * collection at the end of the feature collection
	 * 
	 * @param index
	 *            index at which the specified element is to be inserted.
	 * @param newChildType
	 *            the type of the element to add
	 * 
	 * @throws UnsupportedOperationException
	 *             if the <tt>add</tt> method is not supported by this list.
	 * @throws IllegalArgumentException
	 *             if some aspect of the specified newChildType prevents it from
	 *             being added to this list. E.g.
	 *             <ul>
	 *             <li/>newChildType is null <li/>the underlaying feature
	 *             collection does not accepts elements of the specified type
	 *             <li/>the type is not adaptable to the class {@link FWCls}
	 *             </ul>
	 */
	public FWCls addNew(QName newChildType);
    
    
    public FWCls addNew(QName newChildType, String newFeatureId) ;
    

	/**
	 * Creates and Adds a new element of the specified type into the feature
	 * collection at the specified position
	 * 
	 * @param index
	 *            index at which the specified element is to be inserted.
	 * @param newChildType
	 *            the type of the element to add
	 * 
	 * @throws UnsupportedOperationException
	 *             if the <tt>add</tt> method is not supported by this list.
	 * @throws IllegalArgumentException
	 *             if some aspect of the specified newChildType prevents it from
	 *             being added to this list. E.g.
	 *             <ul>
	 *             <li/>newChildType is null <li/>the underlaying feature
	 *             collection does not accepts elements of the specified type
	 *             <li/>the type is not adaptable to the class {@link FWCls}
	 *             </ul>
	 * @throws IndexOutOfBoundsException
	 *             if the index is out of range (index &lt; 0 || index &gt;
	 *             size()).
	 */
	public FWCls addNew(int index, QName newChildType);
	
	/**
	 * To get the {@link FeatureList} which is being wrapped
	 * 
	 * @return the wrapped feature list
	 */
	public FeatureList getWrappedList();
}
