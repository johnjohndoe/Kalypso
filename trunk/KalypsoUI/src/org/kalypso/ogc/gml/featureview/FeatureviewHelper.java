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
package org.kalypso.ogc.gml.featureview;

import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;

import org.kalypso.gmlschema.adapter.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.jwsdp.JaxbUtilities;
import org.kalypso.ogc.gml.AnnotationUtilities;
import org.kalypso.ogc.gml.gui.GuiTypeRegistrySingleton;
import org.kalypso.ogc.gml.gui.IGuiTypeHandler;
import org.kalypso.template.featureview.Button;
import org.kalypso.template.featureview.Checkbox;
import org.kalypso.template.featureview.CompositeType;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.FeatureviewType;
import org.kalypso.template.featureview.GridData;
import org.kalypso.template.featureview.GridLayout;
import org.kalypso.template.featureview.Group;
import org.kalypso.template.featureview.Label;
import org.kalypso.template.featureview.LayoutDataType;
import org.kalypso.template.featureview.ObjectFactory;
import org.kalypso.template.featureview.Subcomposite;
import org.kalypso.template.featureview.Table;
import org.kalypso.template.featureview.Text;

/**
 * @author belger
 */
public class FeatureviewHelper
{
  public final static int STANDARD_TEXT_FIELD_WIDTH_HINT = 200;

  public static final ObjectFactory FACTORY = new ObjectFactory();

  public static final JAXBContext JC = JaxbUtilities.createQuiet( ObjectFactory.class );

  private FeatureviewHelper( )
  {
    // wird nicht instantiiert
  }

  private static void addDefaultFeatureControlTypeForProperty( final List<JAXBElement< ? extends ControlType>> controlList, final IPropertyType ftp ) throws JAXBException
  {
    ControlType type = null;
    boolean addLabel = true;
    final GridData griddata = FACTORY.createGridData();
    final JAXBElement<GridData> jaxbgriddata = FACTORY.createGridData( griddata );

    final String name = ftp.getName();
    if( ftp instanceof IValuePropertyType )
    {
      final IValuePropertyType vpt = (IValuePropertyType) ftp;
      final Class clazz = vpt.getValueClass();
      if( "boundedBy".equals( name ) )
        return;
      else if( "java.lang.String|java.lang.Integer|java.lang.Long|java.lang.Float|java.lang.Double|java.util.Date".indexOf( clazz.getName() ) != -1 )
      {
        final Text editor = FACTORY.createText();
        editor.setStyle( "SWT.NONE" );
        editor.setEditable( true );
        editor.setProperty( name );

        griddata.setHorizontalAlignment( "GridData.BEGINNING" );
        griddata.setHorizontalSpan( 1 );
        griddata.setWidthHint( STANDARD_TEXT_FIELD_WIDTH_HINT );

        editor.setLayoutData( jaxbgriddata );

        type = editor;
      }
      else if( java.lang.Boolean.class==clazz)
      {
        final Checkbox checkbox = FACTORY.createCheckbox();
        checkbox.setStyle( "SWT.NONE" );
        checkbox.setEditable( true );
        checkbox.setProperty( name );

        griddata.setHorizontalAlignment( "GridData.BEGINNING" );
        griddata.setHorizontalSpan( 1 );
        griddata.setWidthHint( STANDARD_TEXT_FIELD_WIDTH_HINT );
        checkbox.setLayoutData( jaxbgriddata );

        type = checkbox;
      }
      else if( GuiTypeRegistrySingleton.getTypeRegistry().hasClassName( clazz) )
      {
        final IGuiTypeHandler handler = (IGuiTypeHandler) GuiTypeRegistrySingleton.getTypeRegistry().getTypeHandlerForClassName( clazz );
        type = handler.createFeatureviewControl( name, FACTORY );

        griddata.setHorizontalAlignment( "GridData.FILL" );
        griddata.setGrabExcessHorizontalSpace( true );

        griddata.setHorizontalSpan( type instanceof CompositeType ? 2 : 1 );

        type.setLayoutData( jaxbgriddata );
      }
    }
    else if( ftp instanceof IRelationType )
    {
      if( ftp.isList() )
      {
        final Table table = FACTORY.createTable();
        table.setStyle( "SWT.NONE" );
        table.setProperty( name );

        griddata.setHorizontalAlignment( "GridData.FILL" );
        griddata.setVerticalAlignment( "GridData.FILL" );
        griddata.setGrabExcessHorizontalSpace( true );
        griddata.setGrabExcessVerticalSpace( true );
        griddata.setHorizontalSpan( 3 );
        griddata.setHeightHint( 200 );

        table.setLayoutData( jaxbgriddata );

        type = table;
        addLabel = false;
      }
      else
      {
        final Subcomposite compo = FACTORY.createSubcomposite();
        compo.setStyle( "SWT.NONE" );
        compo.setProperty( name );

        griddata.setHorizontalAlignment( "GridData.FILL" );
        griddata.setVerticalAlignment( "GridData.FILL" );
        griddata.setGrabExcessHorizontalSpace( true );
        griddata.setGrabExcessVerticalSpace( true );
        griddata.setHorizontalSpan( 2 );

        compo.setLayoutData( jaxbgriddata );

        final Group group = FACTORY.createGroup();

        final GridData groupdata = FACTORY.createGridData();
        groupdata.setGrabExcessHorizontalSpace( true );
        groupdata.setGrabExcessVerticalSpace( true );
        groupdata.setHorizontalAlignment( "GridData.FILL" );
        groupdata.setVerticalAlignment( "GridData.FILL" );
        groupdata.setHorizontalSpan( 3 );

        final IAnnotation annotation = AnnotationUtilities.getAnnotation(ftp);
        final String text = annotation == null ? name : annotation.getLabel();
        final String tooltip = annotation == null ? null : annotation.getTooltip();

        group.setLayoutData( FACTORY.createGridData( groupdata ) );
        group.setText( text );
        group.setTooltip( tooltip );
        group.setStyle( "SWT.NONE" );

        final GridLayout gridLayout = FACTORY.createGridLayout();
        gridLayout.setNumColumns( 2 );
        group.setLayout( FACTORY.createGridLayout( gridLayout ) );

        group.getControl().add( FACTORY.createControl( compo ) );

        type = group;
        addLabel = false;
      }
    }

    if( type == null )
    {
      final Button button = FACTORY.createButton();
      button.setStyle( "SWT.PUSH" );
      button.setProperty( name );

      griddata.setHorizontalAlignment( "GridData.BEGINNING" );
      button.setLayoutData( jaxbgriddata );
      griddata.setHorizontalSpan( 2 );

      type = button;
    }

    final IAnnotation annotation = AnnotationUtilities.getAnnotation(ftp);
    final String text = annotation == null ? name : annotation.getLabel();
    final String tooltip = annotation == null ? null : annotation.getTooltip();

    if( type != null )
      type.setTooltip( tooltip );

    int cellCount = 0;
    if( addLabel )
    {
      cellCount++;

      final Label label = FACTORY.createLabel();
      label.setStyle( "SWT.NONE" );

      label.setText( text );
      label.setTooltip( tooltip );
      label.setVisible( true );

      final GridData labelGridData = FACTORY.createGridData();
      labelGridData.setGrabExcessHorizontalSpace( false );
      labelGridData.setHorizontalAlignment( "GridData.BEGINNING" );
      label.setLayoutData( FACTORY.createGridData( labelGridData ) );
      controlList.add( FACTORY.createLabel( label ) );
    }

    if( type != null )
    {
      final LayoutDataType layoutData = type.getLayoutData().getValue();
      if( layoutData instanceof GridData )
        cellCount += ((GridData) layoutData).getHorizontalSpan();

      controlList.add( FACTORY.createControl( type ) );
    }

    for( int i = cellCount; i < 3; i++ )
    {
      final Label label = FACTORY.createLabel();
      label.setStyle( "SWT.NONE" );

      // label.setText( "" );
      label.setVisible( false );

      final GridData labelGridData = FACTORY.createGridData();
      labelGridData.setGrabExcessHorizontalSpace( false );
      labelGridData.setHorizontalAlignment( "GridData.BEGINNING" );
      label.setLayoutData( FACTORY.createLayoutData( labelGridData ) );

      controlList.add( FACTORY.createControl( label ) );
    }

  }

  /**
   * Standardview erzeugen
   * 
   * @param type
   * @return featureview
   */
  public static FeatureviewType createFeatureviewFromFeatureType( final IFeatureType type )
  {
    try
    {
      final FeatureviewType featureview = FACTORY.createFeatureviewType();
      featureview.setTypename( type.getName() );
      featureview.setStyle( "SWT.NONE" );

      final GridLayout gridLayout = FACTORY.createGridLayout();
      gridLayout.setNumColumns( 3 );
      featureview.setLayout( FACTORY.createGridLayout( gridLayout ) );
      final GridData griddata = FACTORY.createGridData();
      griddata.setGrabExcessHorizontalSpace( Boolean.TRUE );
      griddata.setGrabExcessVerticalSpace( Boolean.TRUE );
      griddata.setHorizontalAlignment( "GridData.FILL" );
      griddata.setVerticalAlignment( "GridData.FILL" );
      featureview.setLayoutData( FACTORY.createGridData( griddata ) );

      final List<JAXBElement< ? extends ControlType>> controlList = featureview.getControl();

      final IPropertyType[] properties = type.getProperties();
      for( int i = 0; i < properties.length; i++ )
      {
        final IPropertyType ftp = properties[i];
        addDefaultFeatureControlTypeForProperty( controlList, ftp );
      }

      // final Validator validator = JC.createValidator();
      // validator.validate( featureview );

      return featureview;
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();

      return null;
    }
  }

  /**
   * Standardview f�r eine Property erzeugen
   * 
   * @param type
   * @return featureview
   */
  public static FeatureviewType createFeatureviewFromFeatureTypeProperty( final IFeatureType type, final IPropertyType ftp )
  {
    try
    {
      final FeatureviewType featureview = FACTORY.createFeatureviewType();
      featureview.setTypename( type.getName() );
      featureview.setStyle( "SWT.NONE" );

      final GridLayout gridLayout = FACTORY.createGridLayout();
      gridLayout.setNumColumns( 2 );
      featureview.setLayout( FACTORY.createGridLayout( gridLayout ) );
      final GridData griddata = FACTORY.createGridData();
      griddata.setGrabExcessHorizontalSpace( true );
      griddata.setGrabExcessVerticalSpace( true );
      griddata.setHorizontalAlignment( "GridData.FILL" );
      griddata.setVerticalAlignment( "GridData.FILL" );
      featureview.setLayoutData( FACTORY.createGridData( griddata ) );

      final List<JAXBElement< ? extends ControlType>> control = featureview.getControl();

      addDefaultFeatureControlTypeForProperty( control, ftp );

      return featureview;
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();

      return null;
    }
  }

}