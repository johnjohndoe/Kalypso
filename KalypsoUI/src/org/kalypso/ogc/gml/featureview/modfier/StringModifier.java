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
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;

/**
 * @author belger
 */
public class StringModifier implements IFeatureModifier
{
  private final DateFormat DATE_FORMATTER = new SimpleDateFormat( "dd.MM.yyyy HH:mm" );
  
  private final NumberFormat NUMBER_FORMAT = NumberFormat.getInstance();
  
  private final FeatureTypeProperty m_ftp;

  public StringModifier( final FeatureTypeProperty ftp, final String format )
  {
    m_ftp = ftp;
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
    final String text = value == null ? "" : value.toString();
    if( text.length() == 0 )
      return null;

    try
    {
      return parseData( text );
    }
    catch( final NumberFormatException e )
    {
      e.printStackTrace();
    }
    catch( final ParseException e )
    {
      e.printStackTrace();
    }

    return null;
  }

  private Object parseData( final String text ) throws ParseException
  {
    final String typeName = m_ftp.getType();
    if( typeName.equals( "java.lang.String" ) )
      return text;
    if( typeName.equals( "java.lang.Double" ) )
      return new Double( text.replace( ',', '.' ) );
    if( typeName.equals( "java.lang.Integer" ) )
      return new Integer( text );
    if( typeName.equals( "java.lang.Float" ) )
      return new Float( text.replace( ',', '.' ) );
    if( typeName.equals( "java.lang.Long" ) )
      return new Long( text );
    if( typeName.equals( "java.lang.Boolean" ) )
      return new Boolean( text );
    if( typeName.equals( "java.util.Date" ) )
      return DATE_FORMATTER.parse( text );

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
  public String getLabel( Feature f )
  {
    final Object value = getValue( f );
    return value == null ? "" : value.toString();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#getImage(org.kalypsodeegree.model.feature.Feature)
   */
  public Image getImage( Feature f )
  {
    return null;
  }

}