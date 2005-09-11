/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypsodeegree_impl.gml.schema.virtual;

import java.util.HashMap;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.AbstractFeatureType;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/*
 * 
 * @author doemming
 */
public class VirtualVelocityFeatureTypeProperty extends AbstractFeatureType implements VirtualFeatureTypeProperty
{

  private final static String DECORATED_NS = "http://elbe.wb.tu-harburg.de/2dModel";

  private final static String PROP_GEOM = DECORATED_NS + ":geometry";

  private final static String PROP_XVELOCITY = DECORATED_NS + ":xVelocity";

  private final static String PROP_YVELOCITY = DECORATED_NS + ":yVelocity";

  public VirtualVelocityFeatureTypeProperty()
  {
    super( "arrow_velocity", "virtual", new HashMap() );
  }

  /**
   * @see org.kalypsodeegree_impl.gml.schema.virtual.VirtualFeatureTypeProperty#getVirtuelValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypsodeegree.model.feature.GMLWorkspace)
   */
  public Object getVirtuelValue( Feature feature, GMLWorkspace workspace )
  {
    final GM_Point srcP = (GM_Point)feature.getProperty( PROP_GEOM );
    final Float xv = (Float)feature.getProperty( PROP_XVELOCITY );
    final Float yv = (Float)feature.getProperty( PROP_YVELOCITY );
    if( xv == null || srcP == null || yv == null )
      return null;
    double factor = 10;

    final GM_Point targetP = GeometryFactory.createGM_Point( srcP.getX() + factor * xv.floatValue(), srcP.getY()
        + factor * yv.floatValue(), srcP.getCoordinateSystem() );
    try
    {
      return GeometryUtilities.createArrowLineString( srcP, targetP );
      //      return GeometryUtilities.createArrowLineString( srcP, targetP, 0.6, 0.1 );
    }
    catch( GM_Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureTypeProperty#getType()
   */
  public String getType()
  {
    return GeometryUtilities.getLineStringClass().getName();
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureTypeProperty#isNullable()
   */
  public boolean isNullable()
  {
    return false;
  }

}