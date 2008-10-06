package org.kalypsodeegree.model.feature.binding;

import java.util.List;

import javax.xml.namespace.QName;

import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;

/**
 * Interface to be implemented by classes that wrapped a feature collection to provided a view as a {@link List} of
 * {@link FWCls}
 * 
 * @author Patrice Congo
 */
public interface IFeatureWrapperCollection<FWCls extends IFeatureWrapper2> extends IFeatureWrapper2, List<FWCls>
{
  /**
   * Creates and Adds a new element of the specified type into the feature collection at the end of the feature
   * collection
   * 
   * @param index
   *            index at which the specified element is to be inserted.
   * @param newChildType
   *            the type of the element to add
   * @throws UnsupportedOperationException
   *             if the <tt>add</tt> method is not supported by this list.
   * @throws IllegalArgumentException
   *             if some aspect of the specified newChildType prevents it from being added to this list. E.g.
   *             <ul>
   *             <li/>newChildType is null <li/>the underlaying feature collection does not accepts elements of the
   *             specified type <li/>the type is not adaptable to the class {@link FWCls}
   *             </ul>
   */
  public FWCls addNew( final QName newChildType );

  public <T extends FWCls> T addNew( final QName newChildType, final Class<T> classToAdapt );

  public FWCls addNew( final QName newChildType, final String newFeatureId );

  public <T extends FWCls> T addNew( final QName newChildType, final String newFeatureId, final Class<T> classToAdapt );

  /**
   * Creates and Adds a new element of the specified type into the feature collection at the specified position
   * 
   * @param index
   *            index at which the specified element is to be inserted.
   * @param newChildType
   *            the type of the element to add
   * @throws UnsupportedOperationException
   *             if the <tt>add</tt> method is not supported by this list.
   * @throws IllegalArgumentException
   *             if some aspect of the specified newChildType prevents it from being added to this list. E.g.
   *             <ul>
   *             <li/>newChildType is null <li/>the underlaying feature collection does not accepts elements of the
   *             specified type <li/>the type is not adaptable to the class {@link FWCls}
   *             </ul>
   * @throws IndexOutOfBoundsException
   *             if the index is out of range (index &lt; 0 || index &gt; size()).
   */
  public <T extends FWCls> T addNew( final int index, final QName newChildType, final Class<T> classToAdapt );

  /**
   * To get the {@link FeatureList} which is being wrapped
   * 
   * @return the wrapped feature list
   */
  public FeatureList getWrappedList( );

  /**
   * Renove all reference to this feature from this list
   * 
   * @param toRemove
   *            a wrapper wrapping the feature which reference is to be remove from this list
   * @throws IllegalArgumentException
   *             if the argument toRemove is null
   */
  public void removeAllRefs( FWCls toRemove ) throws IllegalArgumentException;

  /**
   * Add this feature as reference to this list
   * 
   * @param toAdd
   *            a wrapper wrapping the feature to be added as list
   * @return true if the feature has been added
   * @throws IllegalArgumentException
   *             if the argument toAdd is null
   */
  public boolean addRef( FWCls toAdd ) throws IllegalArgumentException;

  /**
   * Answer all feature wrappers overlaping the selected zone
   * 
   * @param selectionSurface
   *            the selection surface
   * @param containedOnly
   *            control the selection of feature according to whether a feature (limited to a geometry specified by
   *            checkedGeometryPropertyName ) are contained in the selectionSurface or not:
   *            <ul>
   *            <li/> true to select only features that are contains in the area <li/> false to allow selection of all
   *            overlapping feature
   *            </ul>
   * @param checkedGeometryPropertyName
   *            the q-name of the feature property to check
   * @return a list of feature overlaping the given surface
   * @thorws {@link IllegalArgumentException} if selectionSurface is null
   */
  public List<FWCls> query( final GM_Surface< ? extends GM_SurfacePatch> selectionSurface, final boolean containedOnly );

  /**
   * Answer all feature wrappers overlaping the given envelope
   * 
   * @param envelope
   *            the envelope specifying the selection area
   * @return a list of feature overlaping the given surface
   * @thorws {@link IllegalArgumentException} if envelope is null
   */
  public List<FWCls> query( final GM_Envelope envelope );

  /**
   * Answer all feature wrappers containing the given position
   * 
   * @param selectionSurface
   *            the selection surface
   * @return a list of feature overlaping the given surface
   * @throws {@link IllegalArgumentException}
   *             if position is null
   */
  public List<FWCls> query( final GM_Position position );

  /**
   * Returns the combined bounding box off all contained objects.
   */
  public GM_Envelope getBoundingBox( );

  /**
   * To get the number of object of the specified class in this collection.
   * 
   * @return an int representing the number of object of the sepecified class in this collection
   */
  public int countFeatureWrappers( Class< ? > wrapperClass );

  /**
   * Clones the given object as member into this list
   */
  public void cloneInto( final FWCls toClone ) throws Exception;
}
