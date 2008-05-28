/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.featureview.modfier;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.ogc.gml.featureview.IFeatureModifier;
import org.kalypso.ogc.gml.gui.GuiTypeRegistrySingleton;
import org.kalypso.ogc.gml.gui.IGuiTypeHandler;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class StringModifier implements IFeatureModifier
{
  private final IValuePropertyType m_ftp;

  private final IGuiTypeHandler m_guiTypeHandler;

  private final String m_format;

  public StringModifier( final IValuePropertyType ftp, final String format )
  {
    m_ftp = ftp;
    m_format = format;

    // we need both registered type handler types
    final ITypeRegistry<IGuiTypeHandler> guiTypeRegistry = GuiTypeRegistrySingleton.getTypeRegistry();
    m_guiTypeHandler = guiTypeRegistry.getTypeHandlerFor( m_ftp );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#dispose()
   */
  public void dispose( )
  {
    // nix zu tun
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#getValue(org.kalypsodeegree.model.feature.Feature)
   */
  public Object getValue( final Feature f )
  {
    if( m_ftp == null )
      return null;

    final Object data = f.getProperty( m_ftp );

    return toText( data );
  }

  /**
   * @param data
   *            real property value
   */
  private String toText( final Object data )
  {
    final Object value;

    // REMARK: In order to also allow (basic) editing of lists we consider the edited
    // element to be the element with index = 0. Problem: all other elements of the list
    // get deleted when edited. This is ok for example for the gml:name property, where
    // we normally only want to edit the first entry.
    if( data instanceof List )
    {
      final List< ? > list = (List< ? >) data;
      if( list.isEmpty() )
        return ""; //$NON-NLS-1$

      value = list.get( 0 );
    }
    else
      value = data;

    if( m_guiTypeHandler != null )
      return m_guiTypeHandler.getText( value );

    return value == null ? "" : value.toString(); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#parseInput(org.kalypsodeegree.model.feature.Feature,
   *      java.lang.Object)
   */
  public Object parseInput( final Feature f, final Object editedStringValue )
  {
    Object result = null;
    final String text = editedStringValue == null ? "" : editedStringValue.toString(); //$NON-NLS-1$
    // if( text.length() == 0 )
    // return null;

    try
    {
      result = parseData( text );
    }
    catch( final NumberFormatException e )
    {
      e.printStackTrace();
    }
    catch( final ParseException e )
    {
      e.printStackTrace();
    }
    return result;
  }

  private Object parseData( final String text ) throws ParseException
  {
    if( m_guiTypeHandler != null )
    {
      final Object value = text.length() == 0 ? null : m_guiTypeHandler.parseText( text, null );

      // REMARK: In order to also allow (basic) editing of lists we consider the edited
      // element to be the element with index = 0. Problem: all other elements of the list
      // get deleted when edited. This is ok for example for the gml:name property, where
      // we normally only want to edit the first entry.
      // For other list-properties, where this behaviour is not intended, this string-modifier should not be used.
      if( m_ftp.isList() )
      {
        final List<Object> list = new ArrayList<Object>( 1 );
        list.add( value );
        return list;
      }

      return value;
    }

    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#createCellEditor(org.eclipse.swt.widgets.Composite)
   */
  public CellEditor createCellEditor( final Composite parent )
  {
    return new TextCellEditor( parent );
  }

  /**
   * @see org.eclipse.jface.viewers.ICellEditorValidator#isValid(java.lang.Object)
   */
  public String isValid( final Object editedStringValue )
  {
    try
    {
      if( editedStringValue == null )
        return null;
      final String text = editedStringValue.toString();
      if( text.length() == 0 )
        return null;
      parseData( text );
      return null;
    }
    catch( final Exception e )
    {
      return e.getLocalizedMessage();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#getFeatureTypeProperty()
   */
  public IPropertyType getFeatureTypeProperty( )
  {
    return m_ftp;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#getLabel(org.kalypsodeegree.model.feature.Feature)
   */
  public String getLabel( final Feature f )
  {
    final Object data = f.getProperty( m_ftp );

    // We should probably use a null-format string here
    if( data == null )
      return null;

    if( m_format != null && m_format.length() > 0 )
      return String.format( m_format, data );

    final Object value = getValue( f );

    // HACK
    final Object result;
    if( value instanceof List )
    {
      final List< ? > list = (List< ? >) value;
      if( (list).size() == 0 )
        result = null;
      else if( (list).size() == 1 )
        result = list.get( 0 );
      else
        result = list;
    }
    else
      result = value;

    if( result == null )
      return ""; //$NON-NLS-1$

    return result.toString();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#getImage(org.kalypsodeegree.model.feature.Feature)
   */
  public Image getImage( final Feature f )
  {
    if( m_guiTypeHandler != null )
      return m_guiTypeHandler.getImage( getValue( f ) );

    return null;
  }

  /**
   * Zwei Objekte sind gleich, wenn ihre String-Representation gleich sind.
   * 
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#equals(java.lang.Object, java.lang.Object)
   */
  public boolean equals( final Object newData, final Object oldData )
  {
    return toText( newData ).equals( toText( oldData ) );
  }
}