/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.JAXBElement;
import javax.xml.namespace.QName;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.PaletteData;
import org.eclipse.swt.graphics.RGB;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.IFeatureModifier;
import org.kalypso.ogc.gml.featureview.dialog.ColorFeatureDialog;
import org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog;
import org.kalypso.ogc.gml.featureview.modfier.ColorModifier;
import org.kalypso.ogc.gml.featureview.modfier.StringModifier;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.template.featureview.Button;
import org.kalypso.template.featureview.ColorLabelType;
import org.kalypso.template.featureview.CompositeType;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.GridDataType;
import org.kalypso.template.featureview.GridLayout;
import org.kalypso.template.featureview.ObjectFactory;
import org.kalypso.template.featureview.Text;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandler;

/**
 * This is a gui type handler for the color-type in commons.xsd
 * {@link org.kalypsodeegree_impl.gml.schema.schemata.UrlCatalogOGC}.
 * 
 * @author Dirk Kuch, Holger Albert
 */
public class ColorGuiTypeHandler extends LabelProvider implements IGuiTypeHandler
{
  private final XsdBaseTypeHandler< ? > m_handler;

  private final Map<RGB, Image> m_imageCache = new HashMap<RGB, Image>();

  public ColorGuiTypeHandler( final XsdBaseTypeHandler< ? > handler )
  {
    m_handler = handler;
  }

  /**
   * @see org.kalypso.ogc.gml.gui.XsdBaseGuiTypeHandler#createFeatureDialog(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType)
   */
  public IFeatureDialog createFeatureDialog( final Feature feature, final IPropertyType ftp )
  {
    return new ColorFeatureDialog( feature, (IValuePropertyType) ftp );
  }

  /**
   * @see org.kalypso.ogc.gml.gui.XsdBaseGuiTypeHandler#createFeatureviewControl(org.kalypso.gmlschema.property.IPropertyType,
   *      org.kalypso.template.featureview.ObjectFactory)
   */
  public JAXBElement< ? extends ControlType> createFeatureviewControl( final IPropertyType property, final ObjectFactory factory )
  {
    final QName qname = property.getQName();

    final CompositeType composite = factory.createCompositeType();

    final GridLayout layout = factory.createGridLayout();
    layout.setNumColumns( 3 );
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
    text.setLayoutData( factory.createGridData( textData ) );

    // Label for the color
    final ColorLabelType type = factory.createColorLabelType();
    type.setStyle( "SWT.NONE" ); //$NON-NLS-1$
    type.setProperty( qname );

    final GridDataType typeData = factory.createGridDataType();
    typeData.setHorizontalAlignment( "GridData.FILL" ); //$NON-NLS-1$
    typeData.setGrabExcessHorizontalSpace( false );
    typeData.setHeightHint( 20 );
    typeData.setWidthHint( 20 );
    type.setLayoutData( factory.createGridData( typeData ) );

    // Knopf
    final Button button = factory.createButton();
    final GridDataType buttonData = factory.createGridDataType();
    button.setStyle( "SWT.PUSH" ); //$NON-NLS-1$
    button.setProperty( qname );

    buttonData.setHorizontalAlignment( "GridData.BEGINNING" ); //$NON-NLS-1$
    button.setLayoutData( factory.createGridData( buttonData ) );

    final List<JAXBElement< ? extends ControlType>> control = composite.getControl();
    control.add( factory.createText( text ) );
    control.add( factory.createColorlabel( type ) );
    control.add( factory.createButton( button ) );

    return factory.createComposite( composite );
  }

  /**
   * @see org.kalypso.ogc.gml.gui.XsdBaseGuiTypeHandler#fromText(java.lang.String)
   */
  public Object parseText( final String text, final String formatHint )
  {
    final Pattern p = Pattern.compile( "^\\((\\d{1,3}+),(\\d{1,3}+),(\\d{1,3}+)\\)$" ); //$NON-NLS-1$
    final Matcher m = p.matcher( text );

    if( m.find() )
    {
      if( m.groupCount() != 3 )
        return null;

      final String red = m.group( 1 );
      final String green = m.group( 2 );
      final String blue = m.group( 3 );

      final RGB rgb = new RGB( Integer.valueOf( red ), Integer.valueOf( green ), Integer.valueOf( blue ) );
      return rgb;
    }

    return new RGB( 0, 0, 0 );
  }

  /**
   * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
   */
  @Override
  public String getText( final Object element )
  {
    if( element instanceof RGB )
    {
      final RGB rgb = (RGB) element;
      return "(" + rgb.red + "," + rgb.green + "," + rgb.blue + ")"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    }

    return "(0,0,0)"; //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.viewers.LabelProvider#getImage(java.lang.Object)
   */
  @Override
  public Image getImage( final Object element )
  {
    final RGB rgb = element instanceof RGB ? (RGB) element : null;

    // REMARK: we implement a simple caching strategy here, because (at the moment) we don't know
    // how and when to dispose the images. So at least the number of created images is reduced here.
    if( m_imageCache.containsKey( rgb ) )
      return m_imageCache.get( rgb );

    final Image image;
    if( rgb == null )
    {
      final PaletteData paletteData = new PaletteData( new RGB[] { new RGB( 255, 255, 255 ) } );
      final ImageData imageData = new ImageData( 1, 1, 1, paletteData );
      image = new Image( null, imageData );
    }
    else
    {
      final PaletteData paletteData = new PaletteData( new RGB[] { new RGB( 64, 64, 64 ), ((RGB) element) } );
      final ImageData imageData = new ImageData( 24, 13, 1, paletteData );
      for( int x = 2; x < 23; x++ )
        for( int y = 1; y < 12; y++ )
          imageData.setPixel( x, y, 1 );
      image = new Image( null, imageData );
    }

    m_imageCache.put( rgb, image );
    return image;
  }

  /**
   * @see org.kalypso.ogc.gml.gui.IGuiTypeHandler#createFeatureModifier(org.kalypso.gmlschema.property.IPropertyType,
   *      org.kalypso.ogc.gml.selection.IFeatureSelectionManager,
   *      org.kalypso.ogc.gml.featureview.IFeatureChangeListener, java.lang.String)
   */
  public IFeatureModifier createFeatureModifier( final IPropertyType ftp, final IFeatureSelectionManager selectionManager, final IFeatureChangeListener fcl, final String format )
  {
    // if we get a ClassCastExxception here, something is very wrong
    final IValuePropertyType vpt = (IValuePropertyType) ftp;

    final Class< ? > valueClass = getValueClass();

    if( RGB.class == valueClass )
    {
      return new ColorModifier( vpt );
    }

    return new StringModifier( vpt, format );
  }

  /**
   * @see org.kalypso.gmlschema.types.ITypeHandler#getTypeName()
   */
  public QName getTypeName( )
  {
    return m_handler.getTypeName();
  }

  /**
   * @see org.kalypso.gmlschema.types.ITypeHandler#getValueClass()
   */
  public Class< ? > getValueClass( )
  {
    return m_handler.getValueClass();
  }

  /**
   * @see org.kalypso.gmlschema.types.ITypeHandler#isGeometry()
   */
  public boolean isGeometry( )
  {
    return m_handler.isGeometry();
  }
}