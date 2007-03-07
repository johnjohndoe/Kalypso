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
package org.kalypso.ogc.gml.om.table;

import java.text.ParseException;

import org.apache.commons.lang.ObjectUtils;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.swt.widgets.TableItem;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.ITupleResultChangedListener.ValueChange;
import org.kalypsodeegree.model.XsdBaseTypeHandler;

public class TupleResultCellModifier implements ICellModifier
{
  private final TupleResultContentProvider m_provider;

  public TupleResultCellModifier( final TupleResultContentProvider provider )
  {
    m_provider = provider;
  }

  public boolean canModify( final Object element, String property )
  {
    return true;
  }

  public Object getValue( final Object element, final String property )
  {
    final IRecord record = (IRecord) element;
    final IComponent component = m_provider.getComponent( property );

    final Object value = record.getValue( component );
    if( value == null )
      return "";
    
    // TODO: use component definition to format string
    return "" + value;
  }

  public void modify( final Object element, final String property, final Object value )
  {
    final TableItem item = (TableItem) element;
    final IRecord record = (IRecord) item.getData();
    final IComponent component = modifyRecord( record, property, value );
    if( component == null )
      return;

    final ValueChange[] changes = new ValueChange[] { new ValueChange( record, component, value ) };
    m_provider.getResult().fireValuesChanged( changes );
  }

  /**
   * Does not inform any listeners.
   * <p>
   * Only changes the record if the new value is different frmo the current value.
   * 
   * @return the component whichs was modified, <code>null</code> if the record was not changed.
   */
  public IComponent modifyRecord( final IRecord record, final String property, final Object value )
  {
    final IComponent component = m_provider.getComponent( property );

    final ITypeRegistry<IMarshallingTypeHandler> typeRegistry = MarshallingTypeRegistrySingleton.getTypeRegistry();
    final IMarshallingTypeHandler handler = typeRegistry.getTypeHandlerForTypeName( component.getValueTypeName() );
    try
    {
      if( handler instanceof XsdBaseTypeHandler )
      {
        final Object valueToSet = value == null ? null : handler.parseType( value.toString() );

        final Object oldValue = record.getValue( component );
        if( ObjectUtils.equals( valueToSet, oldValue ) )
          return null;

        record.setValue( component, valueToSet );
      }
      else
      {
        System.out.println( "No type handler for component:" + component );
      }
    }
    catch( final ParseException e1 )
    {
      // ignore
    }

    return component;
  }
}