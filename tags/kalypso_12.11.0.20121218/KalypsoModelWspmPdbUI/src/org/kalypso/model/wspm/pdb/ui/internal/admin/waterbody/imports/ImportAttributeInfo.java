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
package org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.imports;

import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.shape.dbf.DBFField;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.dbf.FieldType;
import org.kalypso.shape.dbf.IDBFField;

/**
 * @author Gernot Belger
 */
public class ImportAttributeInfo<T> extends AbstractModelObject
{
  public static final String PROPERTY_ENABLEMENT = "enablement"; //$NON-NLS-1$

  public static final String PROPERTY_DEFAULT_VALUE = "defaultValue"; //$NON-NLS-1$

  public static String PROPERTY_FIELD = "field"; //$NON-NLS-1$

  public static IDBFField FIELD_USE_DEFAULT;
  static
  {
    try
    {
      FIELD_USE_DEFAULT = new DBFField( Messages.getString( "ImportAttributeInfo.0" ), FieldType.C, (short) 1, (short) 0 ); //$NON-NLS-1$
    }
    catch( final DBaseException e )
    {
      e.printStackTrace();
    }
  }

  private final String m_property;

  private final boolean m_optional;

  private IDBFField m_field = FIELD_USE_DEFAULT;

  private T m_defaultValue;

  public ImportAttributeInfo( final String property, final boolean optional )
  {
    m_property = property;
    m_optional = optional;
  }

  public boolean getEnablement( )
  {
    return m_field == FIELD_USE_DEFAULT;
  }

  public T getDefaultValue( )
  {
    return m_defaultValue;
  }

  public void setDefaultValue( final T defaultValue )
  {
    final Object oldValue = m_defaultValue;

    m_defaultValue = defaultValue;

    firePropertyChange( PROPERTY_DEFAULT_VALUE, oldValue, m_defaultValue );
  }

  public void setField( final IDBFField field )
  {
    final IDBFField oldField = m_field;
    final boolean oldEnablement = getEnablement();

    m_field = field;

    firePropertyChange( PROPERTY_FIELD, oldField, field );
    firePropertyChange( PROPERTY_ENABLEMENT, oldEnablement, getEnablement() );
  }

  public IDBFField getField( )
  {
    return m_field;
  }

  public boolean isOptional( )
  {
    return m_optional;
  }

  public String getProperty( )
  {
    return m_property;
  }
}