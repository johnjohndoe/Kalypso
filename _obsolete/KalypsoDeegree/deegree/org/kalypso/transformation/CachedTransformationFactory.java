/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.transformation;

import java.util.HashMap;
import java.util.Map;

import org.deegree.crs.exceptions.TransformationException;
import org.deegree.crs.transformations.CRSTransformation;
import org.deegree.crs.transformations.TransformationFactory;
import org.deegree.model.crs.CoordinateSystem;
import org.deegree.model.crs.UnknownCRSException;

/**
 * This is a factory for transformations, which uses the original one from Deegree 2, but caches the results.
 *
 * @author Holger Albert
 */
public class CachedTransformationFactory
{
  /**
   * The instance of this cached transformation factory.
   */
  private static CachedTransformationFactory m_myInstance = null;

  /**
   * The delegate transformation factory.
   */
  private TransformationFactory m_delegate;

  /**
   * The cache of all transformations, created so far.
   */
  private Map<String, CRSTransformation> m_cachedTransformations;

  /**
   * The constructor.
   */
  private CachedTransformationFactory( )
  {
    m_delegate = TransformationFactory.getInstance();
    m_cachedTransformations = new HashMap<String, CRSTransformation>();
  }

  /**
   * This function returns the instance of this factory.
   *
   * @return The instance of this factory.
   */
  public static CachedTransformationFactory getInstance( )
  {
    if( m_myInstance == null )
      m_myInstance = new CachedTransformationFactory();

    return m_myInstance;
  }

  /**
   * Creates a transformation between two coordinate systems. This method will examine the coordinate systems in order
   * to construct a transformation between them. This method may fail if no path between the coordinate systems is
   * found.
   *
   * @param source
   *            Input coordinate system name.
   * @param target
   *            Output coordinate system name.
   * @return A coordinate transformation from <code>source</code> to <code>target</code>.
   */
  public CRSTransformation createFromCoordinateSystems( String source, String target ) throws UnknownCRSException, TransformationException
  {
    return getCachedTransformation( source, target );
  }

  /**
   * This function retrieves a transformation by a source name and target name of a coordinate system from the cache. If
   * it does not exist in the cache, it is created and put into the cache.
   *
   * @param source
   *            Input coordinate system name.
   * @param target
   *            Output coordinate system name.
   * @return The transformation.
   */
  private CRSTransformation getCachedTransformation( String source, String target ) throws UnknownCRSException, TransformationException
  {
    /* Try to get it from the cache. */
    CRSTransformation transformation = m_cachedTransformations.get( source + " " + target );

    /* If still null, a new one is needed. */
    if( transformation == null )
    {
      /* Get an instance of the cached crs factory. */
      CachedCRSFactory factory = CachedCRSFactory.getInstance();

      /* Get the coordinate systems. */
      CoordinateSystem sourceCoordinateSystem = factory.create( source );
      CoordinateSystem targetCoordinateSystem = factory.create( target );

      /* Create it ... */
      transformation = m_delegate.createFromCoordinateSystems( sourceCoordinateSystem.getCRS(), targetCoordinateSystem.getCRS() );

      /* ... and cache it for later use. */
      m_cachedTransformations.put( source + " " + target, transformation );
    }

    return transformation;
  }
}