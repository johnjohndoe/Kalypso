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

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultLengthSectionColumn;
import org.kalypso.shape.ShapeDataException;
import org.kalypso.shape.dbf.DBFField;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.dbf.FieldType;
import org.kalypso.shape.dbf.IDBFValue;

/**
 * @author Gernot Belger
 */
public class WspmResultValue implements IDBFValue
{
  private final WspmResultLengthSectionColumn m_column;

  private final DBFField m_field;

  public WspmResultValue( final WspmResultLengthSectionColumn column )
  {
    m_column = column;

    final QName valueTypeName = column.getValueTypeName();
    final String label = String.format( "%5.5s", column ); //$NON-NLS-1$

    m_field = createField( label, valueTypeName );
  }

  private DBFField createField( final String label, final QName valueTypeName )
  {
    try
    {
      if( XmlTypes.XS_STRING.equals( valueTypeName ) )
        return new DBFField( label, FieldType.C, (short) 50, (short) 0 );

      // TODO: check
      if( XmlTypes.XS_DOUBLE.equals( valueTypeName ) )
        return new DBFField( label, FieldType.N, (short) 20, (short) 6 );
      if( XmlTypes.XS_FLOAT.equals( valueTypeName ) )
        return new DBFField( label, FieldType.N, (short) 10, (short) 3 );
      if( XmlTypes.XS_DECIMAL.equals( valueTypeName ) )
        return new DBFField( label, FieldType.N, (short) 10, (short) 3 );

      // TODO: check
      if( XmlTypes.XS_LONG.equals( valueTypeName ) )
        return new DBFField( label, FieldType.N, (short) 20, (short) 0 );
      if( XmlTypes.XS_INT.equals( valueTypeName ) )
        return new DBFField( label, FieldType.N, (short) 10, (short) 0 );
      if( XmlTypes.XS_INTEGER.equals( valueTypeName ) )
        return new DBFField( label, FieldType.N, (short) 10, (short) 0 );
      if( XmlTypes.XS_SHORT.equals( valueTypeName ) )
        return new DBFField( label, FieldType.N, (short) 6, (short) 0 );
      if( XmlTypes.XS_BYTE.equals( valueTypeName ) )
        return new DBFField( label, FieldType.N, (short) 4, (short) 0 );

      if( XmlTypes.XS_TIME.equals( valueTypeName ) )
        return new DBFField( label, FieldType.C, (short) 10, (short) 0 );
      if( XmlTypes.XS_DATETIME.equals( valueTypeName ) )
        return new DBFField( label, FieldType.C, (short) 20, (short) 0 );

      if( XmlTypes.XS_DATE.equals( valueTypeName ) )
        return new DBFField( label, FieldType.D, (short) 10, (short) 0 );

      if( XmlTypes.XS_BOOLEAN.equals( valueTypeName ) )
        return new DBFField( label, FieldType.L, (short) 1, (short) 0 );

      return new DBFField( label, FieldType.C, (short) 50, (short) 0 );
    }
    catch( final DBaseException e )
    {
      e.printStackTrace();
      return null;
    }
  }

  /**
   * @see org.kalypso.shape.dbf.IDBFValue#getField()
   */
  @Override
  public DBFField getField( ) throws ShapeDataException
  {
    if( m_field == null )
      throw new ShapeDataException();

    return m_field;
  }

  /**
   * @see org.kalypso.shape.dbf.IDBFValue#getValue(java.lang.Object)
   */
  @Override
  public Object getValue( final Object element )
  {
    final IProfileFeature profile = (IProfileFeature) element;
    return m_column.getValue( profile.getBigStation() );
  }

  /**
   * @see org.kalypso.shape.dbf.IDBFValue#getLabel()
   */
  @Override
  public String getLabel( )
  {
    final String componentLabel = m_column.getComponentLabel();
    final String label = m_column.getLabel();

    return String.format( "%s - %s", label, componentLabel ); //$NON-NLS-1$
  }

}
