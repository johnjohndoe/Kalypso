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
package org.kalypso.ogc.gml.om.table.handlers;

import java.util.LinkedHashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.swt.SWT;
import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.annotation.ILanguageAnnontationProvider;
import org.kalypso.gmlschema.property.restriction.IRestriction;
import org.kalypso.gmlschema.property.restriction.RestrictionUtilities;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;

/**
 * Default implementation of {@link IComponentUiHandlerProvider}.<br/> Creates columns for all components of the given
 * observation according to its types.
 * 
 * @author Gernot Belger
 */
public class DefaultComponentUiHandlerProvider implements IComponentUiHandlerProvider
{
  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandlerProvider#createComponentHandler(org.kalypso.observation.result.TupleResult)
   */
  public Map<Integer, IComponentUiHandler> createComponentHandler( final TupleResult tupleResult )
  {
    final Map<Integer, IComponentUiHandler> result = new LinkedHashMap<Integer, IComponentUiHandler>();

    final IComponent[] components = tupleResult.getComponents();
    final int widthPercent = components.length == 0 ? 100 : (int) (100.0 / components.length);

    for( int i = 0; i < components.length; i++ )
    {
      final IComponent component = components[i];
      final IComponentUiHandler handlerForComponent = handlerForComponent( i, component, widthPercent );
      if( handlerForComponent != null )
        result.put( i, handlerForComponent );
    }

    return result;
  }

  private IComponentUiHandler handlerForComponent( final int index, final IComponent component, final int columnWidthPercent )
  {
    // Some default value
    final boolean editable = true;
    final boolean resizeable = true;
    final boolean moveable = true;
    final String columnLabel = component.getName();
    final int columnWidth = 100;

    final IRestriction[] restrictions = component.getRestrictions();
    if( ComponentUtilities.restrictionContainsEnumeration( restrictions ) )
    {
      final Map<Object, ILanguageAnnontationProvider> items = RestrictionUtilities.getEnumerationItems( restrictions );
      return new ComponentUiEnumerationHandler( index, editable, resizeable, moveable, columnLabel, SWT.NONE, columnWidth, columnWidthPercent, "", "", items );
    }

    final QName valueTypeName = component.getValueTypeName();

    if( valueTypeName.equals( new QName( NS.XSD_SCHEMA, "dateTime" ) ) )
      return new ComponentUiDateHandler( index, editable, resizeable, moveable, columnLabel, SWT.NONE, columnWidth, columnWidthPercent, "%1$tm %1$te,%1$tY", "", null );

    if( valueTypeName.equals( new QName( NS.XSD_SCHEMA, "double" ) ) )
      return new ComponentUiDoubleHandler( index, editable, resizeable, moveable, columnLabel, SWT.RIGHT, columnWidth, columnWidthPercent, "%f", "", null );

    if( valueTypeName.equals( new QName( NS.XSD_SCHEMA, "decimal" ) ) )
      return new ComponentUiDecimalHandler( index, editable, resizeable, moveable, columnLabel, SWT.RIGHT, columnWidth, columnWidthPercent, "%f", "", null );

    if( valueTypeName.equals( new QName( NS.XSD_SCHEMA, "integer" ) ) )
      return new ComponentUiIntegerHandler( index, editable, resizeable, moveable, columnLabel, SWT.RIGHT, columnWidth, columnWidthPercent, "%d", "", null );

    if( valueTypeName.equals( new QName( NS.XSD_SCHEMA, "string" ) ) )
      return new ComponentUiStringHandler( index, editable, resizeable, moveable, columnLabel, SWT.LEFT, columnWidth, columnWidthPercent, "%s", "", null );

    return null;
  }

}
