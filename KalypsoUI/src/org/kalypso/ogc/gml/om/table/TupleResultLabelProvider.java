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
package org.kalypso.ogc.gml.om.table;

import java.util.HashMap;
import java.util.Map;

import javax.xml.datatype.XMLGregorianCalendar;

import org.eclipse.core.commands.common.EventManager;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.swt.graphics.Image;
import org.kalypso.gmlschema.adapter.AnnotationAdapterFactory;
import org.kalypso.gmlschema.adapter.ILanguageAnnontationProvider;
import org.kalypso.observation.phenomenon.DictionaryPhenomenon;
import org.kalypso.observation.phenomenon.IPhenomenon;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.template.featureview.ColumnDescriptor;
import org.kalypso.template.featureview.ColumnTypeDescriptor;

/**
 * @author Marc Schlienger
 */
public class TupleResultLabelProvider extends EventManager implements ITableLabelProvider
{
  /**
   * comp.getId() -> component-descriptor
   */
  private final Map<String, ColumnDescriptor> m_columnDescriptors;

  private final Map<String, ColumnTypeDescriptor> m_columnTypeDescriptors;

  public TupleResultLabelProvider( )
  {
    this( new HashMap<String, ColumnDescriptor>(), new HashMap<String, ColumnTypeDescriptor>() );
  }

  public TupleResultLabelProvider( final Map<String, ColumnDescriptor> columnDescriptors )
  {
    this( columnDescriptors, new HashMap<String, ColumnTypeDescriptor>() );
  }

  public TupleResultLabelProvider( final Map<String, ColumnDescriptor> columnDescriptors, final Map<String, ColumnTypeDescriptor> columnTypeDescriptors )
  {
    m_columnDescriptors = columnDescriptors;
    m_columnTypeDescriptors = columnTypeDescriptors;
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#addListener(org.eclipse.jface.viewers.ILabelProviderListener)
   */
  public void addListener( final ILabelProviderListener listener )
  {
    addListenerObject( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#dispose()
   */
  public void dispose( )
  {
    clearListeners();
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#isLabelProperty(java.lang.Object, java.lang.String)
   */
  public boolean isLabelProperty( final Object element, final String property )
  {
    // TODO: ask content provider if this is a valid property
    return true;
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#removeListener(org.eclipse.jface.viewers.ILabelProviderListener)
   */
  public void removeListener( final ILabelProviderListener listener )
  {
    removeListenerObject( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang.Object, int)
   */
  public Image getColumnImage( final Object element, final int columnIndex )
  {
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnText(java.lang.Object, int)
   */
  public String getColumnText( final Object element, final int columnIndex )
  {
    if( element instanceof IRecord )
    {
      final IRecord record = (IRecord) element;
      final IComponent comp = getComponent( record, columnIndex );

      Object value = record.getValue( comp );

      if( ComponentUtilities.restrictionContainsEnumeration( comp.getRestrictions() ) && (value != null) )
      {
        final ILanguageAnnontationProvider provider = ComponentUtilities.getLanguageProvider( comp.getRestrictions(), value );

        final String lang = AnnotationAdapterFactory.getPlatformLang();
        value = provider.getAnnotation( lang ).getLabel().trim();
      }

      /* Use descriptor to render text, if no descriptor is set, just 'toString' the value. */
      final ColumnDescriptor descriptor = m_columnDescriptors.get( comp.getId() );
      if( descriptor != null )
      {
        try
        {
          if( value == null )
          {
            final String nullFormat = descriptor.getNullFormat();
            if( nullFormat != null )
            {
              return nullFormat;
            }
          }

          final String formattedValue = TupleResultLabelProvider.formatValue( value, descriptor );
          if( formattedValue != null )
          {
            return formattedValue;
          }
        }
        catch( final Throwable t )
        {
          t.printStackTrace();
          return "<ERROR>";
        }
      }

      final String id;
      final IPhenomenon phenomenon = comp.getPhenomenon();
      if( phenomenon instanceof DictionaryPhenomenon )
      {
        id = ((DictionaryPhenomenon) phenomenon).getDictionaryUrn();
      }
      else
      {
        id = null;
      }

      final ColumnTypeDescriptor descr = m_columnTypeDescriptors.get( id );
      if( descr != null )
      {
        try
        {
          if( value == null )
          {
            final String nullFormat = descr.getNullFormat();
            if( nullFormat != null )
            {
              return nullFormat;
            }
          }

          final String formattedValue = TupleResultLabelProvider.formatValue( value, descr );
          if( formattedValue != null )
          {
            return formattedValue;
          }
        }
        catch( final Throwable t )
        {
          t.printStackTrace();
          return "<ERROR>";
        }
      }

      /* If no format string is present, just toString'it. */
      return value == null ? "" : value.toString();
    }

    return null;
  }

  private IComponent getComponent( final IRecord record, final int columnIndex )
  {
    final IComponent[] comps = record.getOwner().getComponents();
    if( (columnIndex < 0) || (columnIndex >= comps.length) )
    {
      return null;
    }

    final IComponent comp = comps[columnIndex];
    return comp;
  }

  public static String formatValue( final Object value, final ColumnDescriptor descriptor )
  {
    final String format = descriptor == null ? "%s" : descriptor.getDisplayFormat();
    final String displayFormat = format == null ? "%s" : format;

    // HACK: in order to format XMLGregorianCalendars convert them to GregorianCalendars
    // is there a better place to do this?
    if( value instanceof XMLGregorianCalendar )
    {
      return String.format( displayFormat, ((XMLGregorianCalendar) value).toGregorianCalendar() );
    }
    return String.format( displayFormat, value );
  }

  public static String formatValue( final Object value, final ColumnTypeDescriptor descriptor )
  {
    if( descriptor == null )
    {
      return null;
    }

    final String format = descriptor.getDisplayFormat();
    if( format == null )
    {
      return null;
    }

    // HACK: in order to format XMLGregorianCalendars convert them to GregorianCalendars
    // is there a better place to do this?
    if( value instanceof XMLGregorianCalendar )
    {
      return String.format( format, ((XMLGregorianCalendar) value).toGregorianCalendar() );
    }
    return String.format( format, value );
  }
}
