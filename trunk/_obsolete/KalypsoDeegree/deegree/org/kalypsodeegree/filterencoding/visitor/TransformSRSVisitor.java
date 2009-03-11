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
package org.kalypsodeegree.filterencoding.visitor;

import java.util.HashMap;
import java.util.Map;

import org.kalypso.transformation.GeoTransformer;
import org.kalypsodeegree.filterencoding.Operation;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.filterencoding.SpatialOperation;

/**
 * @author kuepfer
 */
public class TransformSRSVisitor implements FilterVisitor
{
  /** operation -> exception */
  private final Map<Operation, Throwable> m_exceptions = new HashMap<Operation, Throwable>();

  private GeoTransformer m_transformer;

  public TransformSRSVisitor( final String targetCRS )
  {
    try
    {
      m_transformer = new GeoTransformer( targetCRS );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypsodeegree.filterencoding.visitor.FilterVisitor#visit(org.kalypsodeegree.filterencoding.Operation)
   */
  public boolean visit( Operation operation )
  {
    try
    {
      if( operation instanceof SpatialOperation )
      {
        final SpatialOperation spatialOps = (SpatialOperation) operation;
        final GM_Object geometry = spatialOps.getGeometry();
        if( geometry != null )
        {
          final GM_Object newGeo = m_transformer.transform( geometry );
          spatialOps.setGeometry( newGeo );
        }
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      m_exceptions.put( operation, e );
    }

    return true;
  }
}