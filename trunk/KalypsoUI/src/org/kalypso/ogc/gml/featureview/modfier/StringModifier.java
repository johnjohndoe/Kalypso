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

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.ParseException;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.ogc.gml.featureview.IFeatureModifier;
import org.kalypso.ogc.gml.gui.GuiTypeRegistrySingleton;
import org.kalypso.ogc.gml.gui.IGuiTypeHandler;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author belger
 */
public class StringModifier implements IFeatureModifier
{
  // TODO put a default dateformat into the preferences and special formats into the gml-applications-schemas !

  // Es gibt jetzt GuiTypeHandler für Datums-Typen.
  //
  // private final static DateFormat DATE_FORMATTER = new SimpleDateFormat( "dd.MM.yyyy HH:mm" );
  // static
  // {
  // DATE_FORMATTER.setTimeZone( KalypsoGisPlugin.getDefault().getDisplayTimeZone() );
  // }

  private final NumberFormat NUMBER_FORMAT;

  private final IValuePropertyType m_ftp;

  private final IGuiTypeHandler m_guiTypeHandler;

  private final IMarshallingTypeHandler m_marshallingTypeHandler;

  public StringModifier( final IValuePropertyType ftp )
  {
    NUMBER_FORMAT = getNumberFormat( ftp );
    m_ftp = ftp;

    // we need both registered type handler types
    final ITypeRegistry<IGuiTypeHandler> guiTypeRegistry = GuiTypeRegistrySingleton.getTypeRegistry();
    m_guiTypeHandler = guiTypeRegistry.getTypeHandlerFor( m_ftp );

    final ITypeRegistry<IMarshallingTypeHandler> marshallingTypeRegistry = MarshallingTypeRegistrySingleton.getTypeRegistry();
    m_marshallingTypeHandler = marshallingTypeRegistry.getTypeHandlerFor( m_ftp );
  }

  public NumberFormat getNumberFormat( final IPropertyType ftp )
  {
    // HACK: TODO: either put this into the IGuiTypeHandler or
    // maybe even in the appinfo of the schema
    final String namespace = ftp.getQName().getNamespaceURI();
    final String name = ftp.getQName().getLocalPart();
    final DecimalFormat expFormat = new DecimalFormat( "0.000E0" );
    // ##0.000E0
    final NumberFormat normalFormat = NumberFormat.getInstance();

    if( "http://www.tuhh.de/kalypsoNA".equals( namespace ) ) // NAMODELL
    {
      if( "flaech".equals( name ) )
        return expFormat;
    }
    if( "http://www.tuhh.de/hydrotop".equals( namespace ) ) // NAMODELL-Hydrotope
    {
      if( "m_perkm".equals( name ) || "area".equals( name ) )
        return expFormat;
    }
    return normalFormat;
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
   *          real property value
   */
  private String toText( final Object data )
  {
    // Es gibt jetzt GuiTypeHandler für Datums-Typen.
    //
    // if( data instanceof XMLGregorianCalendar )
    // {
    // final XMLGregorianCalendar cal = (XMLGregorianCalendar) data;
    // final Date date = DateUtilities.toDate( cal );
    // return DATE_FORMATTER.format( date );
    // }

    if( m_guiTypeHandler != null )
      return m_guiTypeHandler.getText( data );

    return data == null ? "" : data.toString();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#parseInput(org.kalypsodeegree.model.feature.Feature,
   *      java.lang.Object)
   */
  public Object parseInput( final Feature f, final Object editedStringValue )
  {
    Object result = null;
    final String text = editedStringValue == null ? "" : editedStringValue.toString();
    if( text.length() == 0 )
      return null;

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
    // Es gibt jetzt GuiTypeHandler für Datums-Typen.
    //
    // final Class clazz = m_ftp.getValueClass();
    // if( clazz == XMLGregorianCalendar.class )
    // {
    // final Date date = DATE_FORMATTER.parse( text );
    // try
    // {
    // return DateUtilities.toXMLGregorianCalendar( date );
    // }
    // catch( Exception e )
    // {
    // e.printStackTrace();
    // throw new ParseException( e.getMessage(), 0 );
    // }
    // }

    if( m_guiTypeHandler != null )
      return m_guiTypeHandler.fromText( text );

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
    final Object value = getValue( f );
    return value == null ? "" : value.toString();
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