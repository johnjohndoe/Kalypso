package org.deegree_impl.gml.schema.virtual;

import java.util.Map;

import org.deegree.model.feature.Annotation;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree.model.geometry.GM_Polygon;
import org.deegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.deegree_impl.model.cv.RectifiedGridDomain;
import org.opengis.cs.CS_CoordinateSystem;

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

public class VirtualRasterFeatureTypeProperty implements VirtualFeatureTypeProperty
{
  private final FeatureTypeProperty m_ftp;

  /*
   * 
   * @author Nadja
   */
  public VirtualRasterFeatureTypeProperty( FeatureTypeProperty ftp )
  {
    m_ftp = ftp;
  }

  /**
   * @see org.deegree_impl.gml.schema.virtual.VirtualFeatureTypeProperty#getVirtuelValue(org.deegree.model.feature.Feature,
   *      org.deegree.model.feature.GMLWorkspace)
   */
  public Object getVirtuelValue( Feature feature, GMLWorkspace workspace )
  {
    // TODO fetch cs get somewhere
    String targetSrs = "EPSG:31469";
    ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
    CS_CoordinateSystem cs = org.deegree_impl.model.cs.Adapters.getDefault().export(
        csFac.getCSByName( targetSrs ) );
    RectifiedGridDomain rgDomain = (RectifiedGridDomain)feature.getProperty( "rectifiedGridDomain" );
    try
    {
      return rgDomain.getGM_Surface( cs );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  /**
   * @see org.deegree.model.feature.FeatureTypeProperty#getName()
   */
  public String getName()
  {
    return "RasterBoundary_" + m_ftp.getName();
  }

  /**
   * @see org.deegree.model.feature.FeatureTypeProperty#getType()
   */
  public String getType()
  {
    return GM_Polygon.class.getName();
  }

  /**
   * @see org.deegree.model.feature.FeatureTypeProperty#getAnnotation(java.lang.String)
   */
  public Annotation getAnnotation( String lang )
  {
    return null;
  }

  /**
   * @see org.deegree.model.feature.FeatureTypeProperty#isNullable()
   */
  public boolean isNullable()
  {
    return false;
  }

  /**
   * @see org.deegree.model.feature.FeatureTypeProperty#getNamespace()
   */
  public String getNamespace()
  {
    return "virtual";
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
    return null;
  }

}