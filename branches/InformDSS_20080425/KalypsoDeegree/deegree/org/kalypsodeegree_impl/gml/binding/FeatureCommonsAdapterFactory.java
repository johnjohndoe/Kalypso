/**
 *
 */
package org.kalypsodeegree_impl.gml.binding;

import java.util.Collections;
import java.util.Hashtable;
import java.util.Map;

import org.eclipse.core.runtime.IAdapterFactory;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.gml.binding.commons.CoverageCollection;
import org.kalypsodeegree_impl.gml.binding.commons.GeoStatus;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;
import org.kalypsodeegree_impl.gml.binding.commons.IGeoStatus;
import org.kalypsodeegree_impl.gml.binding.commons.IStatusCollection;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridCoverage;
import org.kalypsodeegree_impl.gml.binding.commons.StatusCollection;
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
     *             <li/>feature or cls is null <li/>feature cannot be converted
     *             </ul>
     */
    public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException;
  }

  private final Map<Class< ? >, AdapterConstructor> m_constructors = createConstructorMap();

  /**
   * @see org.eclipse.core.runtime.IAdapterFactory#getAdapter(java.lang.Object, java.lang.Class)
   */
  @SuppressWarnings("unchecked")
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
  public Class< ? >[] getAdapterList( )
  {
    return m_constructors.keySet().toArray( new Class[m_constructors.size()] );
  }

  private static final Map<Class< ? >, AdapterConstructor> createConstructorMap( )
  {
    final Map<Class< ? >, AdapterConstructor> cMap = new Hashtable<Class< ? >, AdapterConstructor>();

    // polynomial 1d
    cMap.put( IPolynomial1D.class, new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
      {
        return new Polynomial1D( feature );
      }
    } );

    // Polynomial 2d
    cMap.put( IPolynomial2D.class, new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
      {
        return new Polynomial2D( feature );
      }
    } );

    // Status
    cMap.put( IStatusCollection.class, new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
      {
        if( GMLSchemaUtilities.substitutes( feature.getFeatureType(), IStatusCollection.QNAME ) )
          return new StatusCollection( feature );

        return null;
      }
    } );

    cMap.put( IGeoStatus.class, new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
      {
        if( GMLSchemaUtilities.substitutes( feature.getFeatureType(), IGeoStatus.QNAME ) )
          return new GeoStatus( feature );

        return null;
      }
    } );

    // Coverages
    cMap.put( ICoverageCollection.class, new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
      {
        if( GMLSchemaUtilities.substitutes( feature.getFeatureType(), ICoverageCollection.QNAME ) )
          return new CoverageCollection( feature );

        return null;
      }
    } );

    cMap.put( ICoverage.class, new AdapterConstructor()
    {
      public Object constructAdapter( final Feature feature, final Class< ? > cls ) throws IllegalArgumentException
      {
        if( GMLSchemaUtilities.substitutes( feature.getFeatureType(), ICoverage.QNAME ) )
          return new RectifiedGridCoverage( feature );

        return null;
      }
    } );

    return Collections.unmodifiableMap( cMap );
  }

}
