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
package org.kalypso.kalypsomodel1d2d.schema.binding.results;

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Thomas Jung
 */
public class NodeResult extends AbstractFeatureBinder
{
  public final static QName QNAME = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "NodeResult" );

  private static final QName QNAME_PROP_CALCID = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "calcId" );
  private static final QName QNAME_PROP_LOCATION = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "location" );
  private static final QName QNAME_PROP_DEPTH = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "depth" );
  private static final QName QNAME_PROP_WATERLEVEL = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "waterlevel" );
  
  public NodeResult( final Feature featureToBind )
  {
    super( featureToBind, QNAME );
  }

  public void setCalcId( final int id )
  {
    getFeature().setProperty( QNAME_PROP_CALCID, new Integer( id ) );
  }

  public void setLocation( final double x, final double y, final double z, final CS_CoordinateSystem crs )
  {
    final GM_Position position = GeometryFactory.createGM_Position( x, y, z );
    final GM_Point point = GeometryFactory.createGM_Point( position, crs );

    getFeature().setProperty( QNAME_PROP_LOCATION, point );
  }

  public void setResultValues( final double vx, final double vy, final double depth, final double waterlevel )
  {
    getFeature().setProperty( QNAME_PROP_DEPTH, depth );
    getFeature().setProperty( QNAME_PROP_WATERLEVEL, waterlevel );
    
    // TODO Auto-generated method stub
    
  }

}
