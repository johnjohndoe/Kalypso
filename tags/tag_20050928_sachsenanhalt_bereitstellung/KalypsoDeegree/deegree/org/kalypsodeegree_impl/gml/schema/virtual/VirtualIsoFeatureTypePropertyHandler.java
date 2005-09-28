package org.kalypsodeegree_impl.gml.schema.virtual;

import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;

/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

public class VirtualIsoFeatureTypePropertyHandler implements VirtualFeatureTypePropertyHandler
{

  /*
   * 
   * @author doemming
   */
  public VirtualIsoFeatureTypePropertyHandler()
  {
    super();
  }

  /**
   * @see org.kalypsodeegree_impl.gml.schema.virtual.VirtualFeatureTypePropertyHandler#isDekoratorOf(org.kalypsodeegree.model.feature.FeatureType)
   */
  public boolean isDekoratorOf( FeatureType ft )
  {
    return "http://elbe.wb.tu-harburg.de/2dModel".equals( ft.getNamespace() ) && "femMesh".equals( ft.getName() );
  }

  /**
   * @see org.kalypsodeegree_impl.gml.schema.virtual.VirtualFeatureTypePropertyHandler#isDekoratorOf(org.kalypsodeegree.model.feature.FeatureTypeProperty)
   */
  public boolean isDekoratorOf( FeatureTypeProperty ftp )
  {
    return false;
  }

  /**
   * @see org.kalypsodeegree_impl.gml.schema.virtual.VirtualFeatureTypePropertyHandler#createVirtualFeatureTypeProperties(org.kalypsodeegree.model.feature.FeatureType)
   */
  public FeatureTypeProperty[] createVirtualFeatureTypeProperties( FeatureType ft )
  {
    return new FeatureTypeProperty[]
    { new VirtualIsoFeatureTypeProperty() };
  }

  /**
   * @see org.kalypsodeegree_impl.gml.schema.virtual.VirtualFeatureTypePropertyHandler#createVirtualFeatureTypeProperties(org.kalypsodeegree.model.feature.FeatureTypeProperty)
   */
  public FeatureTypeProperty[] createVirtualFeatureTypeProperties( FeatureTypeProperty ftp )
  {
    return null;
  }

}