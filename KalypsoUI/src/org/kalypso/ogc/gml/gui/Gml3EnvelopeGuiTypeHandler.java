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

import javax.xml.bind.JAXBElement;
import javax.xml.namespace.QName;

import org.eclipse.jface.viewers.LabelProvider;
import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.IFeatureModifier;
import org.kalypso.ogc.gml.featureview.dialog.EnvelopeFeatureDialog;
import org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog;
import org.kalypso.ogc.gml.featureview.modfier.BooleanModifier;
import org.kalypso.ogc.gml.featureview.modfier.StringModifier;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.template.featureview.Button;
import org.kalypso.template.featureview.CompositeType;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.GridDataType;
import org.kalypso.template.featureview.GridLayout;
import org.kalypso.template.featureview.ObjectFactory;
import org.kalypso.template.featureview.Text;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Gui type handler for gml:envelopes's.
 * 
 * @author Holger Albert
 */
public class Gml3EnvelopeGuiTypeHandler extends LabelProvider implements IGuiTypeHandler
{
  public Gml3EnvelopeGuiTypeHandler( )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.gui.IGuiTypeHandler#createFeatureDialog(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType)
   */
  public IFeatureDialog createFeatureDialog( final Feature feature, final IPropertyType ftp )
  {
    return new EnvelopeFeatureDialog( feature, (IValuePropertyType) ftp );
  }

  /**
   * @see org.kalypso.ogc.gml.gui.IGuiTypeHandler#createFeatureviewControl(javax.xml.namespace.QName,
   *      org.kalypso.template.featureview.ObjectFactory)
   */
  public JAXBElement< ? extends ControlType> createFeatureviewControl( final IPropertyType property, final ObjectFactory factory )
  {
    // // if we get a ClassCastException here, something is very wrong
    // final IValuePropertyType vpt = (IValuePropertyType) property;
    //
    // // Enumeration will get a Combo-Box
    // final Map<String, String> comboEntries = PropertyUtils.createComboEntries( vpt );
    // if( comboEntries.size() > 0 )
    // return super.createFeatureviewControl( property, factory );

    final QName qname = property.getQName();

    final CompositeType composite = factory.createCompositeType();

    final GridLayout layout = factory.createGridLayout();
    layout.setNumColumns( 2 );
    layout.setMakeColumnsEqualWidth( false );
    layout.setMarginWidth( 1 );
    composite.setLayout( factory.createGridLayout( layout ) );
    composite.setStyle( "SWT.NONE" );

    // Text
    final Text text = factory.createText();
    text.setStyle( "SWT.BORDER" );
    text.setEditable( true );
    text.setProperty( qname );

    final GridDataType textData = factory.createGridDataType();
    textData.setHorizontalAlignment( "GridData.FILL" );
    textData.setGrabExcessHorizontalSpace( true );
    text.setLayoutData( factory.createGridData( textData ) );

    // Knopf
    final Button button = factory.createButton();
    final GridDataType buttonData = factory.createGridDataType();
    button.setStyle( "SWT.PUSH" );
    button.setProperty( qname );

    buttonData.setHorizontalAlignment( "GridData.BEGINNING" );
    button.setLayoutData( factory.createGridData( buttonData ) );

    final List<JAXBElement< ? extends ControlType>> control = composite.getControl();
    control.add( factory.createText( text ) );
    control.add( factory.createButton( button ) );

    return factory.createComposite( composite );
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
    return GM_Envelope.class;
  }

  /**
   * @see org.kalypso.gmlschema.types.ITypeHandler#getTypeName()
   */
  public QName getTypeName( )
  {
    return new QName( NS.GML3, "BoundingShapeType" );
  }

  /**
   * @see org.kalypso.gmlschema.types.ITypeHandler#isGeometry()
   */
  public boolean isGeometry( )
  {
    return false;
  }

  /**
   * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
   */
  @Override
  public String getText( final Object element )
  {
    if( element == null )
      return "";

    GM_Envelope envelope = (GM_Envelope) element;

    final String result = new Double( envelope.getMin().getX() ).toString() + ";" + new Double( envelope.getMin().getY() ).toString() + ";" + new Double( envelope.getMax().getX() ).toString() + ";"
        + new Double( envelope.getMax().getY() ).toString();

    return result;
  }

  /**
   * @see org.kalypso.ogc.gml.gui.IGuiTypeHandler#fromText(java.lang.String)
   */
  public Object parseText( String text, String formatHint )
  {
    /* Erstellen des Envelopes. */

    /* Werte anhand von ; trennen. */
    String[] str_values = text.split( ";" );

    Double[] dbl_values = new Double[4];

    /* Es muss vier Werte geben. */
    for( int i = 0; i < 4; i++ )
    {
      /* Ist kein Wert vorhanden, so speichere 0.0. */
      if( str_values.length - 1 < i )
      {
        dbl_values[i] = new Double( "0.0" );
      }
      else
      {
        dbl_values[i] = new Double( str_values[i] );
      }
    }

    final GM_Envelope envelope = GeometryFactory.createGM_Envelope( dbl_values[0], dbl_values[1], dbl_values[2], dbl_values[3] );

    return envelope;
  }
}
