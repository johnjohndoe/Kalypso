/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
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
