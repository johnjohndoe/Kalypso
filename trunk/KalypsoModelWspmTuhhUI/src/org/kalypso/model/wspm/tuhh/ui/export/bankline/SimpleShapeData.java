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
package org.kalypso.model.wspm.tuhh.ui.export.bankline;

import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import org.kalypso.shape.IShapeData;
import org.kalypso.shape.ShapeType;
import org.kalypso.shape.dbf.IDBFField;
import org.kalypso.shape.dbf.IDBFValue;
import org.kalypso.shape.geometry.ISHPGeometry;

/**
 * Simple implementation of {@link org.kalypso.shape.IShapeData}. TODO: move to common place.
 *
 * @author Gernot Belger
 */
public class SimpleShapeData implements IShapeData
{
  private final Collection<SimpleShapeValues> m_data = new ArrayList<>();

  private final Charset m_charset;

  private final String m_srs;

  private final ShapeType m_type;

  private final IDBFField[] m_fields;

  private final IDBFValue[] m_values;

  public SimpleShapeData( final Charset charset, final String srs, final ShapeType type, final IDBFField[] fields )
  {
    m_charset = charset;
    m_srs = srs;
    m_type = type;
    m_fields = fields;

    m_values = new IDBFValue[fields.length];
    for( int i = 0; i < fields.length; i++ )
      m_values[i] = new SimpleDbfValue( fields[i].getName(), fields[i] );
  }

  @Override
  public Charset getCharset( )
  {
    return m_charset;
  }

  @Override
  public String getCoordinateSystem( )
  {
    return m_srs;
  }

  @Override
  public ShapeType getShapeType( )
  {
    return m_type;
  }

  @Override
  public IDBFValue[] getFields( )
  {
    return m_values;
  }

  public void addRow( final ISHPGeometry geometry, final Object[] data )
  {
    final SimpleShapeValues row = new SimpleShapeValues( geometry, data, m_fields );
    m_data.add( row );
  }

  @Override
  public Iterator< ? > iterator( )
  {
    return m_data.iterator();
  }

  @Override
  public int size( )
  {
    return m_data.size();
  }

  @Override
  public ISHPGeometry getGeometry( final Object element )
  {
    final SimpleShapeValues row = (SimpleShapeValues) element;
    return row.getGeometry();
  }
}