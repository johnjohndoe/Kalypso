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

import java.rmi.RemoteException;
import java.util.List;

import javax.xml.bind.JAXBElement;
import javax.xml.namespace.QName;

import org.eclipse.jface.viewers.LabelProvider;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.IFeatureModifier;
import org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog;
import org.kalypso.ogc.gml.featureview.dialog.PointFeatureDialog;
import org.kalypso.ogc.gml.featureview.maker.FeatureviewHelper;
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
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.cs.Adapters;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.kalypsodeegree_impl.model.cs.CoordinateSystem;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Gui type handler for gml:envelopes's.
 * 
 * @author Holger Albert
 */
public class Gml3PointGuiTypeHandler extends LabelProvider implements IGuiTypeHandler
{
  public Gml3PointGuiTypeHandler( )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.gui.IGuiTypeHandler#createFeatureDialog(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType)
   */
  public IFeatureDialog createFeatureDialog( final Feature feature, final IPropertyType ftp )
  {
    return new PointFeatureDialog( feature, (IValuePropertyType) ftp );
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
    textData.setWidthHint( FeatureviewHelper.STANDARD_TEXT_FIELD_WIDTH_HINT );
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
    return GM_Point.class;
  }

  /**
   * @see org.kalypso.gmlschema.types.ITypeHandler#getTypeName()
   */
  public QName getTypeName( )
  {
    /* This corresponds to the qname, it in defined in GeometryUtilities. */
    return GeometryUtilities.QN_POINT_PROPERTY;
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

    GM_Point point = (GM_Point) element;
    GM_Position pos = point.getPosition();

    double[] dbl_values = pos.getAsArray();

    String result = "";

    try
    {
      final CS_CoordinateSystem coordinateSystem = point.getCoordinateSystem();
      result = coordinateSystem == null ? "" : coordinateSystem.getName();
    }
    catch( RemoteException e )
    {
      e.printStackTrace();
    }

    for( int i = 0; i < dbl_values.length; i++ )
    {
      result = result + ";" + new Double( dbl_values[i] ).toString();
    }

    return result;
  }

  /**
   * @see org.kalypso.ogc.gml.gui.IGuiTypeHandler#fromText(java.lang.String)
   */
  public Object fromText( String text )
  {
    final Adapters m_csAdapter = org.kalypsodeegree_impl.model.cs.Adapters.getDefault();

    /* Erstellen des Points. */
    GM_Position pos = null;
    CS_CoordinateSystem crs = null;

    /* Werte anhand von ; trennen. */
    String[] str_values = text.split( ";" );

    double[] dbl_values = null;

    /* Sind mind. zwei Eintr‰ge vorhanden (CS und ein double-Wert)? */
    if( str_values.length > 1 )
    {
      /* Der erste Eintrag wird als das CS angenommen. */
      CoordinateSystem cs = new ConvenienceCSFactoryFull().getCSByName( str_values[0] );

      /* Sollte es das CS nicht geben, stelle das Default CS ein. */
      if( cs != null )
      {
        crs = m_csAdapter.export( cs );

        /* Der erste Eintrag war ein CS. */
        dbl_values = new double[str_values.length - 1];

        for( int i = 1; i < str_values.length; i++ )
        {
          dbl_values[i - 1] = new Double( str_values[i] );
        }
      }
      else
      {
        crs = KalypsoGisPlugin.getDefault().getCoordinatesSystem();

        /* Der erste Eintrag war kein CS. */
        dbl_values = new double[str_values.length];

        for( int i = 0; i < str_values.length; i++ )
        {
          dbl_values[i] = new Double( str_values[i] );
        }
      }

      pos = GeometryFactory.createGM_Position( dbl_values );
    }
    else
    {
      /* Falls es nur einen Wert gibt, wird er als CS angenommen. */
      CoordinateSystem cs = new ConvenienceCSFactoryFull().getCSByName( str_values[0] );

      /* Sollte es das CS nicht geben, stelle das Default CS ein. */
      if( cs != null )
      {
        crs = m_csAdapter.export( cs );
        pos = GeometryFactory.createGM_Position( new double[] { 0.0 } );
      }
      else
      {
        crs = KalypsoGisPlugin.getDefault().getCoordinatesSystem();
        pos = GeometryFactory.createGM_Position( new double[] { new Double( str_values[0] ) } );
      }

    }

    GM_Point point = GeometryFactory.createGM_Point( pos, crs );

    return point;
  }
}