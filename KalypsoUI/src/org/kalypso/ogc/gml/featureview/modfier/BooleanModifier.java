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

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureTypeProperty;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.CheckboxCellEditor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.ogc.gml.featureview.IFeatureModifier;
import org.kalypso.ui.ImageProvider;

/**
 * @author belger
 */
public class BooleanModifier implements IFeatureModifier
{
  private Image m_checkedImage = null;
  private Image m_uncheckedImage = null;
  
  private final FeatureTypeProperty m_ftp;
  
  public BooleanModifier( final FeatureTypeProperty ftp )
  {
    m_ftp = ftp;
    
    if( !"java.lang.Boolean".equals( ftp.getType() ) )
      throw new IllegalArgumentException( "Only Booleans accepted by this Modifier" );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#getValue(org.deegree.model.feature.Feature)
   */
  public Object getValue( final Feature f )
  {
    final Object property = f.getProperty( m_ftp.getName() );
    if( property == null )
      return Boolean.FALSE;
    
    return property;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#parseInput(org.deegree.model.feature.Feature, java.lang.Object)
   */
  public Object parseInput( final Feature f, final Object value )
  {
    return value;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#createCellEditor(org.eclipse.swt.widgets.Composite)
   */
  public CellEditor createCellEditor( Composite parent )
  {
    return new CheckboxCellEditor( parent );
  }

  /**
   * @see org.eclipse.jface.viewers.ICellEditorValidator#isValid(java.lang.Object)
   */
  public String isValid( final Object value )
  {
    if( value instanceof Boolean )
      return null;

    return "Only Boolean values accepted";
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#getFeatureTypeProperty()
   */
  public FeatureTypeProperty getFeatureTypeProperty()
  {
    return m_ftp;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#getLabel(org.deegree.model.feature.Feature)
   */
  public String getLabel( final Feature f )
  {
    return "";
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#getImage(org.deegree.model.feature.Feature)
   */
  public Image getImage( final Feature f )
  {
    final Boolean b = (Boolean)getValue( f );
    if( b == null || !b.booleanValue() )
    {
      if( m_uncheckedImage == null )
      {
         final ImageDescriptor id = ImageProvider.IMAGE_UTIL_UNCHECKED;
        m_uncheckedImage = id.createImage();
      }
      
      return m_uncheckedImage;
    }
    
    if( m_checkedImage == null )
    {
      final ImageDescriptor id = ImageProvider.IMAGE_UTIL_CHECKED;
      m_checkedImage = id.createImage();
    }
    
    return m_checkedImage;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#dispose()
   */
  public void dispose()
  {
    m_checkedImage.dispose();
    m_uncheckedImage.dispose();
  }

}