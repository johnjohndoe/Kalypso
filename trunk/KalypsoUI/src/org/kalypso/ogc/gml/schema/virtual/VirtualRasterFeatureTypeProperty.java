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
package org.kalypso.ogc.gml.schema.virtual;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.gml.schema.virtual.AbstractVirtualPropertyType;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * VirtualRasterFeatureTypeProperty
 * <p>
 * Diese VirtualProperty sollte unter KalypsoDeegree liegen, sie ben�tigt aber das lokal eingestellte Koordinatensystem
 * und wurde deshalb in KalypsoUI abgelegt
 * <p>
 * Bessere L�sung f�r das Raster w�re die Property RectifiedGridDomain des Rasters als GeometryProperty zu definieren,
 * dies ist momentan nicht m�glich bzw. mit gro�en Ver�nderungen verbunden
 * <p>
 * created by
 * 
 * @author Nadja Peiler (27.05.2005)
 */

/**
 * @author Nadja Peiler
 */
public class VirtualRasterFeatureTypeProperty extends AbstractVirtualPropertyType
{
  private GM_Object m_value;

  public VirtualRasterFeatureTypeProperty( IPropertyType ftp )
  {
    super( new QName( "virtual", "RasterBoundary_" + ftp.getQName().getLocalPart() ), 0, 1, GeometryUtilities.getPolygonClass() );
  }

  /**
   * @see org.kalypsodeegree_impl.gml.schema.virtual.VirtualFeatureTypeProperty#getVirtuelValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypsodeegree.model.feature.GMLWorkspace)
   */
  public Object getVirtuelValue( Feature feature, GMLWorkspace workspace )
  {
    if( m_value != null )
      return m_value;
    final RectifiedGridDomain rgDomain = (RectifiedGridDomain) feature.getProperty( "rectifiedGridDomain" );
    if( rgDomain == null )
      return null;
    try
    {
      m_value = rgDomain.getGM_Surface( KalypsoGisPlugin.getDefault().getCoordinatesSystem() );
      return m_value;
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }
}