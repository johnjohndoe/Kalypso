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
package org.kalypso.ui.views.properties;

import org.eclipse.ui.views.properties.IPropertyDescriptor;
import org.eclipse.ui.views.properties.IPropertySource2;
import org.eclipse.ui.views.properties.PropertyDescriptor;
import org.eclipse.ui.views.properties.TextPropertyDescriptor;
import org.kalypso.commons.i18n.I10nString;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoTheme;

/**
 * Property source for {@link org.kalypso.ogc.gml.IKalypsoTheme}s.
 * 
 * @author Gernot Belger
 */
public class ThemePropertySource implements IPropertySource2
{
  public static final String DESC_NAME = "property.descriptor.theme.basic.name"; //$NON-NLS-1$

  public static final String DESC_TYPE = "property.descriptor.theme.basic.type"; //$NON-NLS-1$

  private final IKalypsoTheme m_theme;

  private final IPropertyDescriptor[] m_descriptors;

  public ThemePropertySource( final IKalypsoTheme theme )
  {
    m_theme = theme;
    m_descriptors = initDescriptors();
  }

  private static IPropertyDescriptor[] initDescriptors( )
  {
    final TextPropertyDescriptor nameDesc = new TextPropertyDescriptor( DESC_NAME, Messages.getString("org.kalypso.ui.views.properties.ThemePropertySource.2") ); //$NON-NLS-1$
    nameDesc.setCategory( Messages.getString("org.kalypso.ui.views.properties.ThemePropertySource.3") ); //$NON-NLS-1$
    final PropertyDescriptor typeDesc = new PropertyDescriptor( DESC_TYPE, Messages.getString("org.kalypso.ui.views.properties.ThemePropertySource.4") ); //$NON-NLS-1$
    typeDesc.setCategory( Messages.getString("org.kalypso.ui.views.properties.ThemePropertySource.5") ); //$NON-NLS-1$

    return new IPropertyDescriptor[] { nameDesc, typeDesc };
  }

  /**
   * @see org.eclipse.ui.views.properties.IPropertySource2#isPropertyResettable(java.lang.Object)
   */
  public boolean isPropertyResettable( final Object id )
  {
    return false;
  }

  /**
   * @see org.eclipse.ui.views.properties.IPropertySource2#isPropertySet(java.lang.Object)
   */
  public boolean isPropertySet( final Object id )
  {
    return false;
  }

  /**
   * @see org.eclipse.ui.views.properties.IPropertySource#getEditableValue()
   */
  public Object getEditableValue( )
  {
    return m_theme;
  }

  /**
   * @see org.eclipse.ui.views.properties.IPropertySource#getPropertyDescriptors()
   */
  public IPropertyDescriptor[] getPropertyDescriptors( )
  {
    return m_descriptors;
  }

  /**
   * @see org.eclipse.ui.views.properties.IPropertySource#getPropertyValue(java.lang.Object)
   */
  public Object getPropertyValue( final Object id )
  {
    if( DESC_NAME.equals( id ) )
      return m_theme.getLabel();

    if( DESC_TYPE.equals( id ) )
      return m_theme.getType();

    throw new UnsupportedOperationException( Messages.getString("org.kalypso.ui.views.properties.ThemePropertySource.6") + id ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.ui.views.properties.IPropertySource#resetPropertyValue(java.lang.Object)
   */
  public void resetPropertyValue( final Object id )
  {
    throw new UnsupportedOperationException( Messages.getString("org.kalypso.ui.views.properties.ThemePropertySource.7") + id ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.ui.views.properties.IPropertySource#setPropertyValue(java.lang.Object, java.lang.Object)
   */
  public void setPropertyValue( final Object id, final Object value )
  {
    if( DESC_NAME.equals( id ) )
    {
      m_theme.setName( new I10nString( (String) value ) );
      return;
    }

    throw new UnsupportedOperationException( Messages.getString("org.kalypso.ui.views.properties.ThemePropertySource.8") + id ); //$NON-NLS-1$
  }

}
