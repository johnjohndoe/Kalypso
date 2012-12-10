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
package org.kalypso.model.wspm.tuhh.ui.export.shape;

import java.nio.charset.Charset;

import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.shape.IShapeData;
import org.kalypso.shape.ShapeType;
import org.kalypso.shape.dbf.IDBFValue;
import org.kalypso.shape.deegree.GM_Object2Shape;
import org.kalypso.shape.deegree.IShapeDataFactory;

public class ProfileLineDataFactory implements IShapeDataFactory
{
  private final IProfileFeature[] m_profiles;

  private final Charset m_charset;

  private final String m_coordinateSystem;

  private final IDBFValue[] m_fields;

  public ProfileLineDataFactory( final IProfileFeature[] profiles, final Charset charset, final String coordinateSystem, final IDBFValue[] fields )
  {
    m_profiles = profiles;
    m_charset = charset;
    m_coordinateSystem = coordinateSystem;
    m_fields = fields;
  }

  @Override
  public IShapeData createData( )
  {
    // TODO: let user choose type; one of POLYLINE(Z) or MULTIPOINT(Z) makes sense.
    final ShapeType shapeType = ShapeType.POLYLINEZ;
    final GM_Object2Shape shapeConverter = new GM_Object2Shape( shapeType, m_coordinateSystem );

    return new ProfileLineDataProvider( m_profiles, m_charset, shapeConverter, m_fields );
  }
}
