package org.deegree_impl.gml.schema.virtual;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.deegree.model.feature.Annotation;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureAssociationTypeProperty;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree.model.geometry.GM_Curve;
import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_LineString;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Position;
import org.deegree_impl.model.geometry.GeometryFactory;

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

public class VirtualFeatureAssociationTypeProperty implements VirtualFeatureTypeProperty
{

  private final String m_name;

  private final String m_type;

  private final Map m_annotations;

  private final String m_namespace;

  private String m_linkName;

  private String m_linkNamespace;

  /*
   * 
   * @author doemming
   */
  public VirtualFeatureAssociationTypeProperty( FeatureAssociationTypeProperty ftp )
  {
    m_linkName = ftp.getName();
    m_linkNamespace = ftp.getNamespace();
    m_name = "link_" + ftp.getName();
    m_type = GM_LineString.class.getName();
    m_annotations = new HashMap();
    m_namespace = "virtual";

  }

  /**
   * @see org.deegree.model.feature.FeatureTypeProperty#getName()
   */
  public String getName()
  {
    return m_name;
  }

  /**
   * @see org.deegree.model.feature.FeatureTypeProperty#getType()
   */
  public String getType()
  {
    return m_type;
  }

  /**
   * @see org.deegree.model.feature.FeatureTypeProperty#getAnnotation(java.lang.String)
   */
  public Annotation getAnnotation( String lang )
  {
    return (Annotation)m_annotations.get( lang );
  }

  /**
   * @see org.deegree.model.feature.FeatureTypeProperty#isNullable()
   */
  public boolean isNullable()
  {
    return true;
  }

  /**
   * @see org.deegree.model.feature.FeatureTypeProperty#getNamespace()
   */
  public String getNamespace()
  {
    return m_namespace;
  }

  /**
   * @see org.deegree.model.feature.FeatureTypeProperty#isGeometryProperty()
   */
  public boolean isGeometryProperty()
  {
    return true;
  }

  /**
   * @see org.deegree.model.feature.FeatureTypeProperty#getAnnotationMap()
   */
  public Map getAnnotationMap()
  {
    return m_annotations;
  }

  public Object getVirtuelValue( Feature feature, GMLWorkspace workspace )
  {
    if(workspace==null)
      return null;
    try
    {
      final GM_Object srcGeo = feature.getDefaultGeometryProperty();
      GM_Position src = srcGeo.getCentroid().getPosition();
      if( srcGeo == null )
        return null;
      final GetGeomDestinationFeatureVisitor visitor = new GetGeomDestinationFeatureVisitor(
          workspace, m_linkName, 2 );
      visitor.visit( feature );
      final GM_Object[] destGeo = visitor.getGeometryDestinations();
      final List curves = new ArrayList();
      for( int i = 0; i < destGeo.length; i++ )
      {
        final GM_Position[] pos = new GM_Position[]
        {
            src,
            destGeo[i].getCentroid().getPosition() };
        curves.add( GeometryFactory.createGM_Curve( pos, srcGeo.getCoordinateSystem() ) );
      }
      return GeometryFactory.createGM_MultiCurve( (GM_Curve[])curves.toArray( new GM_Curve[curves
          .size()] ) );
    }
    catch( GM_Exception e )
    {
      e.printStackTrace();
      return null;
    }

  }
}