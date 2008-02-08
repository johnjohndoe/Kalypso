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
package org.kalypso.ogc.gml.featureview.control;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.table.handlers.ComponentUiHandlerFactory;
import org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler;
import org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandlerProvider;
import org.kalypso.template.featureview.ColumnDescriptor;
import org.kalypso.util.swt.SWTUtilities;

/**
 * @author Dirk Kuch
 * @author Gernot Belger
 */
public class TupleResultFeatureControlHandlerProvider implements IComponentUiHandlerProvider
{
  private final ColumnDescriptor[] m_descriptors;

  public TupleResultFeatureControlHandlerProvider( final ColumnDescriptor[] descriptors )
  {
    m_descriptors = descriptors;
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandlerProvider#createComponentHandler(org.kalypso.observation.result.TupleResult)
   */
  public Map<Integer, IComponentUiHandler> createComponentHandler( final TupleResult tupleResult )
  {
    final Map<Integer, IComponentUiHandler> result = new LinkedHashMap<Integer, IComponentUiHandler>( m_descriptors.length );

    final IComponent[] components = tupleResult.getComponents();

    final Map<String, IComponent> componentMap = new HashMap<String, IComponent>();
    for( final IComponent component : components )
      componentMap.put( component.getId(), component );

    for( final ColumnDescriptor cd : m_descriptors )
    {
      final String componentId = cd.getComponent();
      final IComponent component = componentMap.get( componentId );
      final int alignment = SWTUtilities.createStyleFromString( cd.getAlignment() );

      final int componentIndex = tupleResult.indexOfComponent( component );
      if( component != null )
      {
        final boolean editable = cd.isEditable();
        final boolean resizeable = cd.isResizeable();
        final boolean moveable = cd.isMoveable();
        final String label = cd.getLabel();
        final int width = cd.getWidth();
        final int widthPercent = cd.getWidthPercent();
        final String displayFormat = cd.getDisplayFormat();
        final String nullFormat = cd.getNullFormat();
        final String parseFormat = cd.getParseFormat();

        final String columnLabel = label == null ? component.getName() : label;

        final IComponentUiHandler handler = ComponentUiHandlerFactory.getHandler( componentIndex, component, editable, resizeable, moveable, columnLabel, alignment, width, widthPercent, displayFormat, nullFormat, parseFormat );
        result.put( componentIndex, handler );
      }

      final boolean optional = cd.isOptional();
      if( component == null && !optional )
      {
        /* Non-optional columns must exists: throw error message */
        final String msg = String.format( "Non-Optional component does not exist: %s. Remove from template or make optional.", componentId );
        throw new IllegalArgumentException( msg );
      }
    }

    return result;
  }

}
