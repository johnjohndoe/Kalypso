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
package org.kalypso.ogc.gml.gui;

import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBElement;
import javax.xml.namespace.QName;

import org.eclipse.jface.viewers.LabelProvider;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.PropertyUtils;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.IFeatureModifier;
import org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog;
import org.kalypso.ogc.gml.featureview.dialog.NotImplementedFeatureDialog;
import org.kalypso.ogc.gml.featureview.modfier.BooleanModifier;
import org.kalypso.ogc.gml.featureview.modfier.StringModifier;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.template.featureview.Checkbox;
import org.kalypso.template.featureview.Combo;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.ObjectFactory;
import org.kalypso.template.featureview.Text;
import org.kalypso.template.featureview.Combo.Entry;
import org.kalypsodeegree.model.XsdBaseTypeHandler;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot
 */
public class XsdBaseGuiTypeHandler extends LabelProvider implements IGuiTypeHandler
{
  private final XsdBaseTypeHandler m_handler;

  public XsdBaseGuiTypeHandler( final XsdBaseTypeHandler handler )
  {
    m_handler = handler;
  }

  /**
   * @see org.kalypso.ogc.gml.gui.IGuiTypeHandler#createFeatureDialog(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType)
   */
  public IFeatureDialog createFeatureDialog( Feature feature, IPropertyType ftp )
  {
    return new NotImplementedFeatureDialog();
  }

  /**
   * @see org.kalypso.ogc.gml.gui.IGuiTypeHandler#createFeatureviewControl(javax.xml.namespace.QName,
   *      org.kalypso.template.featureview.ObjectFactory)
   */
  public JAXBElement< ? extends ControlType> createFeatureviewControl( final IPropertyType property, final ObjectFactory factory )
  {
    // if we get a ClassCastException here, something is very wrong
    final IValuePropertyType vpt = (IValuePropertyType) property;

    final Class valueClass = getValueClass();
    final QName qname = property.getQName();

    // Booleans get a check box
    if( Boolean.class == valueClass )
    {
      final Checkbox checkbox = factory.createCheckbox();
      checkbox.setStyle( "SWT.NONE" );
      checkbox.setEditable( true );
      checkbox.setProperty( qname );

      return factory.createCheckbox( checkbox );
    }

    // Enumeration will get a Combo-Box
    final Map<String, String> comboEntries = PropertyUtils.createComboEntries( vpt );
    if( comboEntries.size() > 0 )
    {
      final Combo combo = factory.createCombo();
      combo.setStyle( "SWT.DROP_DOWN | SWT.READ_ONLY" );
      combo.setProperty( qname );

      final List<Entry> entries = combo.getEntry();
      for( final Map.Entry<String, String> comboEntry : comboEntries.entrySet() )
      {
        final Entry entry = factory.createComboEntry();
        entry.setLabel( comboEntry.getKey() );
        entry.setValue( comboEntry.getValue() );

        entries.add( entry );
      }

      return factory.createCombo( combo );
    }

    // everything else will be edited in a text field
    final Text editor = factory.createText();
    editor.setStyle( "SWT.NONE" );
    editor.setEditable( true );
    editor.setProperty( qname );

    return factory.createText( editor );
  }

  /**
   * @see org.kalypso.ogc.gml.gui.IGuiTypeHandler#createFeatureModifier(org.kalypso.gmlschema.property.IPropertyType,
   *      org.kalypso.ogc.gml.selection.IFeatureSelectionManager,
   *      org.kalypso.ogc.gml.featureview.IFeatureChangeListener)
   */
  public IFeatureModifier createFeatureModifier( final IPropertyType ftp, final IFeatureSelectionManager selectionManager, final IFeatureChangeListener fcl )
  {
    // if we get a ClassCastExxception here, something is very wrong
    final IValuePropertyType vpt = (IValuePropertyType) ftp;

    final Class valueClass = getValueClass();

    if( Boolean.class == valueClass )
      return new BooleanModifier( vpt );

    return new StringModifier( vpt );
  }

  /**
   * @see org.kalypso.gmlschema.types.ITypeHandler#getValueClass()
   */
  public Class getValueClass( )
  {
    return m_handler.getValueClass();
  }

  /**
   * @see org.kalypso.gmlschema.types.ITypeHandler#getTypeName()
   */
  public QName getTypeName( )
  {
    return m_handler.getTypeName();
  }

  /**
   * @see org.kalypso.gmlschema.types.ITypeHandler#isGeometry()
   */
  public boolean isGeometry( )
  {
    return m_handler.isGeometry();
  }

}
