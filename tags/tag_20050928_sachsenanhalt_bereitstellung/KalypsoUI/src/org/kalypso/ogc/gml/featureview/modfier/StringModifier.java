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

import java.text.DateFormat;
import java.text.NumberFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.ogc.gml.featureview.IFeatureModifier;
import org.kalypso.ogc.gml.gui.GuiTypeRegistrySingleton;
import org.kalypso.ogc.gml.gui.IGuiTypeHandler;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree_impl.extension.IMarshallingTypeHandler;
import org.kalypsodeegree_impl.extension.ITypeRegistry;
import org.kalypsodeegree_impl.extension.MarshallingTypeRegistrySingleton;

/**
 * @author belger
 */
public class StringModifier implements IFeatureModifier
{
  private final DateFormat DATE_FORMATTER = new SimpleDateFormat( "dd.MM.yyyy HH:mm" );

  private final NumberFormat NUMBER_FORMAT = NumberFormat.getInstance();

  private final FeatureTypeProperty m_ftp;

  private final IGuiTypeHandler m_guiTypeHandler;

  private final IMarshallingTypeHandler m_marshallingTypeHandler;

  public StringModifier( final FeatureTypeProperty ftp )
  {
    m_ftp = ftp;

    // wen need both registered type handler types
    final ITypeRegistry guiTypeRegistry = GuiTypeRegistrySingleton.getTypeRegistry();
    m_guiTypeHandler = (IGuiTypeHandler)guiTypeRegistry.getTypeHandlerForClassName( m_ftp.getType() );
    
    final ITypeRegistry marshallingTypeRegistry = MarshallingTypeRegistrySingleton.getTypeRegistry();
    m_marshallingTypeHandler = (IMarshallingTypeHandler)marshallingTypeRegistry.getTypeHandlerForClassName( m_ftp.getType() );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#dispose()
   */
  public void dispose()
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

    final Object data = f.getProperty( m_ftp.getName() );

    return toText( data );
  }
  
  private String toText( final Object data )
  {
    if( m_guiTypeHandler != null )
      return m_guiTypeHandler.getText( data );

    if( data instanceof Date )
      return DATE_FORMATTER.format( data );
    else if( data instanceof Number )
      return NUMBER_FORMAT.format( data );

    return data == null ? "" : data.toString();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#parseInput(org.kalypsodeegree.model.feature.Feature,
   *      java.lang.Object)
   */
  public Object parseInput( final Feature f, final Object value )
  {
    Object result = null;
    final String text = value == null ? "" : value.toString();
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
    final String typeName = m_ftp.getType();
    if( m_guiTypeHandler != null )
      return m_marshallingTypeHandler.parseType( text );
    else if( typeName.equals( "java.lang.String" ) )
      return text;
    else if( typeName.equals( "java.lang.Boolean" ) )
      return new Boolean( text );
    else if( typeName.equals( "java.util.Date" ) )
      return DATE_FORMATTER.parse( text );
    else if( typeName.equals( "java.lang.Double" ) )
      return new Double( NUMBER_FORMAT.parse( text ).doubleValue() );
    else if( typeName.equals( "java.lang.Integer" ) )
      return new Integer( NUMBER_FORMAT.parse( text ).intValue() );
    else if( typeName.equals( "java.lang.Float" ) )
      return new Float( NUMBER_FORMAT.parse( text ).floatValue() );
    else if( typeName.equals( "java.lang.Long" ) )
      return new Long( NUMBER_FORMAT.parse( text ).longValue() );
    
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
  public String isValid( final Object value )
  {
    try
    {
      if( value == null )
        return null;
      final String text = value.toString();
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
  public FeatureTypeProperty getFeatureTypeProperty()
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