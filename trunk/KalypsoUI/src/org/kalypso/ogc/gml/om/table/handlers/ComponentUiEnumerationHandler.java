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
package org.kalypso.ogc.gml.om.table.handlers;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import org.apache.commons.lang.NotImplementedException;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Table;
import org.kalypso.gmlschema.annotation.AnnotationAdapterFactory;
import org.kalypso.gmlschema.annotation.ILanguageAnnontationProvider;
import org.kalypso.gmlschema.property.restriction.EnumerationRestriction;
import org.kalypso.gmlschema.property.restriction.IRestriction;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.ogc.gml.om.table.celleditor.ComboBoxViewerCellEditor;

/**
 * Handles enumerated values, i.e. values which have enumeration-restrictions.
 * 
 * @author Dirk Kuch
 * @author Gernot Belger
 */
public class ComponentUiEnumerationHandler extends AbstractComponentUiHandler
{
  protected final Map<Object, ILanguageAnnontationProvider> m_items;

  public ComponentUiEnumerationHandler( final IComponent component, final boolean editable, final boolean resizeable, final boolean moveable, final String columnLabel, final int columnStyle, final int columnWidth, final int columnWidthPercent, final String displayFormat, final String nullFormat, final String parseFormat )
  {
    super( component, editable, resizeable, moveable, columnLabel, columnStyle, columnWidth, columnWidthPercent, displayFormat, nullFormat, parseFormat );

    m_items = getEnumerationItems( component.getRestrictions() );
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler#createCellEditor(org.eclipse.swt.widgets.Table)
   */
  public CellEditor createCellEditor( final Table table )
  {
    final Set<Object> set = m_items.keySet();

    final LabelProvider labelProvider = new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( Object element )
      {
        final String lang = AnnotationAdapterFactory.getPlatformLang();
        final ILanguageAnnontationProvider provider = m_items.get( element );

        if( provider != null )
          return provider.getAnnotation( lang ).getLabel().trim();

        return super.getText( element );
      }
    };

    return new ComboBoxViewerCellEditor( new ArrayContentProvider(), labelProvider, set, table, SWT.READ_ONLY | SWT.DROP_DOWN );
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler#formatValue(java.lang.Object)
   */
  public Object formatValue( final Object value )
  {
    return value;
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler#parseValue(java.lang.Object)
   */
  public Object parseValue( final Object value )
  {
    final String lang = AnnotationAdapterFactory.getPlatformLang();

    final Set<Entry<Object, ILanguageAnnontationProvider>> set = m_items.entrySet();
    for( final Entry<Object, ILanguageAnnontationProvider> entry : set )
    {
      final Object key = entry.getKey();

      final ILanguageAnnontationProvider provider = entry.getValue();
      final String label = provider.getAnnotation( lang ).getLabel().trim();

      if( label.equals( value ) )
        return key;

      else if( key.equals( value ) ) // because of "-9999" -> nodata
        return key;
    }

    throw new NotImplementedException();
  }

  private Map<Object, ILanguageAnnontationProvider> getEnumerationItems( final IRestriction[] restrictions )
  {
    final Map<Object, ILanguageAnnontationProvider> items = new LinkedHashMap<Object, ILanguageAnnontationProvider>();

    for( final IRestriction restriction : restrictions )
    {
      if( restriction instanceof EnumerationRestriction )
      {
        final EnumerationRestriction r = (EnumerationRestriction) restriction;

        items.putAll( r.getMapping() );
      }
    }

    return items;
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler#getStringRepresentation(java.lang.Object)
   */
  @Override
  public String getStringRepresentation( final Object value )
  {
    final ILanguageAnnontationProvider provider = ComponentUtilities.getLanguageProvider( getComponent().getRestrictions(), value );

    final String lang = AnnotationAdapterFactory.getPlatformLang();
    return provider.getAnnotation( lang ).getLabel().trim();
  }

}