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

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ColorCellEditor;
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
 * @author belger
 */
public class ColorModifier implements IFeatureModifier
{
  private final IValuePropertyType m_ftp;

  private final IGuiTypeHandler m_guiTypeHandler;

  public ColorModifier( final IValuePropertyType ftp )
  {
    m_ftp = ftp;

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

    final Object rgbValue = f.getProperty( m_ftp );
    return rgbValue;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#parseInput(org.kalypsodeegree.model.feature.Feature,
   *      java.lang.Object)
   */
  public Object parseInput( final Feature f, final Object rgbValue )
  {
    return rgbValue;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#createCellEditor(org.eclipse.swt.widgets.Composite)
   */
  public CellEditor createCellEditor( final Composite parent )
  {
    return new ColorCellEditor( parent );
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
      m_guiTypeHandler.fromText( text );
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
    return value == null ? "" : m_guiTypeHandler.getText( value ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#getImage(org.kalypsodeegree.model.feature.Feature)
   */
  public Image getImage( final Feature f )
  {
    final Object value = getValue( f );
    return m_guiTypeHandler.getImage( value );
  }

  /**
   * Zwei Objekte sind gleich, wenn ihre String-Representation gleich sind.
   * 
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#equals(java.lang.Object, java.lang.Object)
   */
  public boolean equals( final Object newData, final Object oldData )
  {
    return m_guiTypeHandler.getText( newData ).equals( m_guiTypeHandler.getText( oldData ) );
  }
}