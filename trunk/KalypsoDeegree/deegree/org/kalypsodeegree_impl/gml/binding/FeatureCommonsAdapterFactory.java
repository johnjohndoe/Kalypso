/**
 * 
 */
package org.kalypsodeegree_impl.gml.binding;

import java.util.Collections;
import java.util.Hashtable;
import java.util.Map;

import org.eclipse.core.runtime.IAdapterFactory;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.gml.binding.commons.IStatus;
import org.kalypsodeegree_impl.gml.binding.commons.Status;
import org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D;
import org.kalypsodeegree_impl.gml.binding.math.IPolynomial2D;
import org.kalypsodeegree_impl.gml.binding.math.Polynomial1D;
import org.kalypsodeegree_impl.gml.binding.math.Polynomial2D;

/**
 * Adapter Factory for feature in the simBase namespace
 * 
 * @author Patrice Congo
 */
public class FeatureCommonsAdapterFactory implements IAdapterFactory
{
  private interface AdapterConstructor
  {
    /**
     * Construct the Adapter of the specified class for the given feature
     * 
     * @param <T>
     * @param feature
     * @param cls
     * @return
     * @throws IllegalArgumentException
     *             if
     *             <ul>
     *             <li/>feature or cls is null <li/>feature cannnot be converted
     *             </ul>
     */
    public Object constructAdapter( final Feature feature, final Class cls ) throws IllegalArgumentException;
  }

  private final Map<Class, AdapterConstructor> m_constructors = createConstructorMap();

  /**
   * @see org.eclipse.core.runtime.IAdapterFactory#getAdapter(java.lang.Object, java.lang.Class)
   */
  public Object getAdapter( final Object adaptableObject, final Class adapterType )
  {
    if( !(adaptableObject instanceof Feature) )
      throw new IllegalArgumentException( "Adapter Factory for feature only but" + " get to adapt:" + adaptableObject );

    final AdapterConstructor ctor = m_constructors.get( adapterType );
    if( ctor != null )
      return ctor.constructAdapter( (Feature) adaptableObject, adapterType );

    return null;
  }

  /**
   * @see org.eclipse.core.runtime.IAdapterFactory#getAdapterList()
   */
  public Class[] getAdapterList( )
  {
    return m_constructors.keySet().toArray( new Class[m_constructors.size()] );
  }

  private static final Map<Class, AdapterConstructor> createConstructorMap( )
  {
    final Map<Class, AdapterConstructor> cMap = new Hashtable<Class, AdapterConstructor>();

    // polynomial 1d
    cMap.put( IPolynomial1D.class, new AdapterConstructor()
    {
      public Object constructAdapter( Feature feature, Class cls ) throws IllegalArgumentException
      {
        return new Polynomial1D( feature );
      }
    } );

    // Polynomial 2d
    cMap.put( IPolynomial2D.class, new AdapterConstructor()
    {
      public Object constructAdapter( Feature feature, Class cls ) throws IllegalArgumentException
      {

        return new Polynomial2D( feature );
      }
    } );

    // Status
    cMap.put( IStatus.class, new AdapterConstructor()
    {
      public Object constructAdapter( Feature feature, Class cls ) throws IllegalArgumentException
      {

        return new Status( feature );
      }
    } );

    return Collections.unmodifiableMap( cMap );
  }
}
