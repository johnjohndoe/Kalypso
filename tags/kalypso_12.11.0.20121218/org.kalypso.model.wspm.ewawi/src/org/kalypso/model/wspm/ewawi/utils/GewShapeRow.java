/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.ewawi.utils;

import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.dbf.IDBFField;
import org.kalypso.shape.geometry.ISHPGeometry;

/**
 * @author Holger Albert
 */
public class GewShapeRow
{
  private final IDBFField[] m_fields;

  private final ISHPGeometry m_shape;

  private final Object[] m_values;

  public GewShapeRow( final IDBFField[] fields, final ISHPGeometry shape, final Object[] values )
  {
    m_fields = fields;
    m_shape = shape;
    m_values = values;
  }

  public ISHPGeometry getShape( )
  {
    return m_shape;
  }

  public Object getValue( final String field ) throws DBaseException
  {
    final int index = getIndex( field );
    if( index < 0 )
      throw new DBaseException( String.format( "Unknown field '%s'", field ) );

    return m_values[index];
  }

  public int getIndex( final String field )
  {
    for( int i = 0; i < m_fields.length; i++ )
    {
      final IDBFField dbfField = m_fields[i];
      if( dbfField.getName().equalsIgnoreCase( field ) )
        return i;
    }

    return -1;
  }
}