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

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBElement;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.PropertyUtils;
import org.kalypso.ogc.gml.featureview.dialog.CalendarFeatureDialog;
import org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog;
import org.kalypso.ogc.gml.featureview.maker.FeatureviewHelper;
import org.kalypso.template.featureview.Button;
import org.kalypso.template.featureview.CompositeType;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.GridDataType;
import org.kalypso.template.featureview.GridLayout;
import org.kalypso.template.featureview.ObjectFactory;
import org.kalypso.template.featureview.Text;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandler;

import com.sun.org.apache.xerces.internal.jaxp.datatype.XMLGregorianCalendarImpl;

/**
 * Gui type handler for xs:date's.
 * 
 * @author Holger Albert
 */
public class XsdDateGuiTypeHandler extends XsdBaseGuiTypeHandler
{
  public static final DateFormat DF_Date = DateFormat.getDateInstance( DateFormat.MEDIUM );

  public static final DateFormat DF_DateTime = DateFormat.getDateTimeInstance( DateFormat.MEDIUM, DateFormat.MEDIUM );

  public static final DateFormat DF_Time = DateFormat.getTimeInstance( DateFormat.MEDIUM );

  static
  {
    DF_Date.setTimeZone( KalypsoGisPlugin.getDefault().getDisplayTimeZone() );
    DF_DateTime.setTimeZone( KalypsoGisPlugin.getDefault().getDisplayTimeZone() );
    DF_Time.setTimeZone( KalypsoGisPlugin.getDefault().getDisplayTimeZone() );
  }

  private final DateFormat m_df;

  private final boolean m_show_button;

  public XsdDateGuiTypeHandler( final XsdBaseTypeHandler handler, DateFormat df, boolean show_button )
  {
    super( handler );
    m_df = df;
    m_show_button = show_button;
  }

  /**
   * @see org.kalypso.ogc.gml.gui.IGuiTypeHandler#createFeatureDialog(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType)
   */
  @Override
  public IFeatureDialog createFeatureDialog( final Feature feature, final IPropertyType ftp )
  {
    return new CalendarFeatureDialog( feature, (IValuePropertyType) ftp );
  }

  /**
   * @see org.kalypso.ogc.gml.gui.IGuiTypeHandler#createFeatureviewControl(javax.xml.namespace.QName,
   *      org.kalypso.template.featureview.ObjectFactory)
   */
  @Override
  public JAXBElement< ? extends ControlType> createFeatureviewControl( final IPropertyType property, final ObjectFactory factory )
  {
    // if we get a ClassCastException here, something is very wrong
    final IValuePropertyType vpt = (IValuePropertyType) property;

    // Enumeration will get a Combo-Box
    final Map<Object, String> comboEntries = PropertyUtils.createComboEntries( vpt );
    if( comboEntries.size() > 0 )
      return super.createFeatureviewControl( property, factory );

    final QName qname = property.getQName();

    final CompositeType composite = factory.createCompositeType();

    final GridLayout layout = factory.createGridLayout();
    layout.setNumColumns( 2 );
    layout.setMakeColumnsEqualWidth( false );
    layout.setMarginWidth( 1 );
    composite.setLayout( factory.createGridLayout( layout ) );
    composite.setStyle( "SWT.NONE" ); //$NON-NLS-1$

    // Text
    final Text text = factory.createText();
    text.setStyle( "SWT.BORDER" ); //$NON-NLS-1$
    text.setEditable( true );
    text.setProperty( qname );

    final GridDataType textData = factory.createGridDataType();
    textData.setHorizontalAlignment( "GridData.FILL" ); //$NON-NLS-1$
    textData.setGrabExcessHorizontalSpace( true );
    textData.setWidthHint( FeatureviewHelper.STANDARD_TEXT_FIELD_WIDTH_HINT );
    text.setLayoutData( factory.createGridData( textData ) );

    // Knopf
    final Button button = factory.createButton();
    final GridDataType buttonData = factory.createGridDataType();
    button.setStyle( "SWT.PUSH" ); //$NON-NLS-1$
    button.setProperty( qname );

    button.setVisible( m_show_button );

    buttonData.setHorizontalAlignment( "GridData.BEGINNING" ); //$NON-NLS-1$
    button.setLayoutData( factory.createGridData( buttonData ) );

    final List<JAXBElement< ? extends ControlType>> control = composite.getControl();
    control.add( factory.createText( text ) );
    control.add( factory.createButton( button ) );

    return factory.createComposite( composite );
  }

  /**
   * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
   */
  @Override
  public String getText( final Object element )
  {
    final XMLGregorianCalendar xmlCal = (XMLGregorianCalendar) element;
    if( xmlCal == null )
      return ""; //$NON-NLS-1$

    return m_df.format( xmlCal.toGregorianCalendar().getTime() );
  }

  /**
   * @see org.kalypso.ogc.gml.gui.IGuiTypeHandler#fromText(java.lang.String)
   */
  @Override
  public Object parseText( final String text, final String formatHint ) throws ParseException
  {
    final Date date;
    if( formatHint == null )
      date = m_df.parse( text );
    else
      date = new SimpleDateFormat( formatHint ).parse( text );

    final Calendar cal = Calendar.getInstance();
    cal.setTime( date );

    final XMLGregorianCalendarImpl gregorianCalendar = new XMLGregorianCalendarImpl( (GregorianCalendar) cal );
    return gregorianCalendar;
  }
}
