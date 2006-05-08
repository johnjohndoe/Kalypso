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
import javax.xml.namespace.QName;

import org.kalypso.contribs.java.xml.XMLHelper;
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
import org.kalypso.template.featureview.CompositeType;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.FeatureviewType;
import org.kalypso.template.featureview.GridDataType;
import org.kalypso.template.featureview.GridLayout;
import org.kalypso.template.featureview.Group;
import org.kalypso.template.featureview.LabelType;
import org.kalypso.template.featureview.LayoutDataType;
import org.kalypso.template.featureview.ObjectFactory;
import org.kalypso.template.featureview.SubcompositeType;
import org.kalypso.template.featureview.Table;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author belger
 */
public class FeatureviewHelper
{
  public final static int STANDARD_TEXT_FIELD_WIDTH_HINT = 200;

  public final static QName QNAME_GML_BOUNDEDBY = new QName( XMLHelper.GMLSCHEMA_NS, "boundedBy" );

  public static final ObjectFactory FACTORY = new ObjectFactory();

  public static final JAXBContext JC = JaxbUtilities.createQuiet( ObjectFactory.class );

  private FeatureviewHelper( )
  {
    // wird nicht instantiiert
  }

  private static void addDefaultFeatureControlTypeForProperty( final List<JAXBElement< ? extends ControlType>> controlList, final IPropertyType ftp, final Object propertyValue )
  {
    JAXBElement< ? extends ControlType> jaxbType = null;
    boolean addLabel = true;
    final GridDataType griddata = FACTORY.createGridDataType();

    final JAXBElement<GridDataType> jaxbgriddata = FACTORY.createGridData( griddata );

    final QName property = ftp.getQName();
    if( ftp instanceof IValuePropertyType )
    {
      final IValuePropertyType vpt = (IValuePropertyType) ftp;

      // ignore 'boundedBy'
      if( QNAME_GML_BOUNDEDBY.equals( property ) )
        return;

      final QName valueQName = vpt.getValueQName();
      if( GuiTypeRegistrySingleton.getTypeRegistry().hasTypeName( valueQName ) )
      {
        final IGuiTypeHandler handler = (IGuiTypeHandler) GuiTypeRegistrySingleton.getTypeRegistry().getTypeHandlerForTypeName( valueQName );

        jaxbType = handler.createFeatureviewControl( ftp, FACTORY );

        final ControlType type = jaxbType.getValue();

        if( type instanceof CompositeType )
        {
          griddata.setHorizontalAlignment( "GridData.FILL" );
          griddata.setGrabExcessHorizontalSpace( true );
          griddata.setHorizontalSpan( 2 );
        }
        else
        {
          griddata.setHorizontalAlignment( "GridData.BEGINNING" );
          griddata.setHorizontalSpan( 1 );

          // TODO is this ok for all controls?
          griddata.setWidthHint( STANDARD_TEXT_FIELD_WIDTH_HINT );
        }

        type.setLayoutData( jaxbgriddata );
      }
    }
    else if( ftp instanceof IRelationType )
    {
      if( ftp.isList() )
      {
        final Table table = FACTORY.createTable();
        table.setStyle( "SWT.NONE" );
        table.setProperty( property );

        griddata.setHorizontalAlignment( "GridData.FILL" );
        griddata.setVerticalAlignment( "GridData.FILL" );
        griddata.setGrabExcessHorizontalSpace( true );
        griddata.setGrabExcessVerticalSpace( true );
        griddata.setHorizontalSpan( 3 );
        griddata.setHeightHint( 200 );

        table.setLayoutData( jaxbgriddata );

        jaxbType = FACTORY.createTable( table );
        addLabel = false;
      }
      else if( propertyValue == null || propertyValue instanceof String )
      {
        // HACK: we have a link here
        // We just do nothing, so the default behaviour (label + button) applies
        // PROBLEM: we have still problems, because if we have a list with features
        // sometimes null and sometimes not, the result depends on the first visited feature
      }
      else
      {
        final SubcompositeType compo = FACTORY.createSubcompositeType();
        compo.setStyle( "SWT.NONE" );
        compo.setProperty( property );

        griddata.setHorizontalAlignment( "GridData.FILL" );
        griddata.setVerticalAlignment( "GridData.FILL" );
        griddata.setGrabExcessHorizontalSpace( true );
        griddata.setGrabExcessVerticalSpace( true );
        griddata.setHorizontalSpan( 2 );

        compo.setLayoutData( jaxbgriddata );

        final Group group = FACTORY.createGroup();

        final GridDataType groupdata = FACTORY.createGridDataType();
        groupdata.setGrabExcessHorizontalSpace( true );
        groupdata.setGrabExcessVerticalSpace( true );
        groupdata.setHorizontalAlignment( "GridData.FILL" );
        groupdata.setVerticalAlignment( "GridData.FILL" );
        groupdata.setHorizontalSpan( 3 );

        final IAnnotation annotation = AnnotationUtilities.getAnnotation( ftp );
        final String text = annotation == null ? property.getLocalPart() : annotation.getLabel();
        final String tooltip = annotation == null ? null : annotation.getTooltip();

        group.setLayoutData( FACTORY.createGridData( groupdata ) );
        group.setText( text );
        group.setTooltip( tooltip );
        group.setStyle( "SWT.NONE" );

        final GridLayout gridLayout = FACTORY.createGridLayout();
        gridLayout.setNumColumns( 2 );
        group.setLayout( FACTORY.createGridLayout( gridLayout ) );

        group.getControl().add( FACTORY.createSubcomposite( compo ) );

        jaxbType = FACTORY.createGroup( group );
        addLabel = false;
      }
    }

    if( jaxbType == null )
    {
      final Button button = FACTORY.createButton();
      button.setStyle( "SWT.PUSH" );
      button.setProperty( property );

      griddata.setHorizontalAlignment( "GridData.BEGINNING" );
      button.setLayoutData( jaxbgriddata );
      griddata.setHorizontalSpan( 2 );

      jaxbType = FACTORY.createButton( button );
    }

    final IAnnotation annotation = AnnotationUtilities.getAnnotation( ftp );
    final String text = annotation == null ? property.getLocalPart() : annotation.getLabel();
    final String tooltip = annotation == null ? null : annotation.getTooltip();

    if( jaxbType != null )
      jaxbType.getValue().setTooltip( tooltip );

    int cellCount = 0;
    if( addLabel )
    {
      cellCount++;

      final LabelType label = FACTORY.createLabelType();
      label.setStyle( "SWT.NONE" );

      label.setText( text );
      label.setTooltip( tooltip );
      label.setVisible( true );

      final GridDataType labelGridData = FACTORY.createGridDataType();
      labelGridData.setGrabExcessHorizontalSpace( false );
      labelGridData.setHorizontalAlignment( "GridData.BEGINNING" );
      label.setLayoutData( FACTORY.createGridData( labelGridData ) );
      controlList.add( FACTORY.createLabel( label ) );
    }

    if( jaxbType != null )
    {
      final ControlType type = jaxbType.getValue();

      final LayoutDataType layoutData = type.getLayoutData().getValue();
      if( layoutData instanceof GridDataType )
        cellCount += ((GridDataType) layoutData).getHorizontalSpan();

      controlList.add( jaxbType );
    }

    for( int i = cellCount; i < 3; i++ )
    {
      final LabelType label = FACTORY.createLabelType();
      label.setStyle( "SWT.NONE" );
      label.setVisible( false );

      final GridDataType labelGridData = FACTORY.createGridDataType();
      labelGridData.setGrabExcessHorizontalSpace( false );
      labelGridData.setHorizontalAlignment( "GridData.BEGINNING" );
      label.setLayoutData( FACTORY.createGridData( labelGridData ) );

      controlList.add( FACTORY.createLabel( label ) );
    }

  }

  /**
   * Standardview erzeugen
   * 
   * @param type
   * @param feature
   *          Optional, if not null, we can use the feature to decide certain things, which are not decidable only from
   *          the type (e.g. linked features)
   * @return featureview
   */
  public static FeatureviewType createFeatureviewFromFeatureType( final IFeatureType type, final Feature feature )
  {
    final FeatureviewType featureview = FACTORY.createFeatureviewType();
    featureview.setTypename( type.getQName() );
    featureview.setStyle( "SWT.NONE" );

    final GridLayout gridLayout = FACTORY.createGridLayout();
    gridLayout.setNumColumns( 3 );
    featureview.setLayout( FACTORY.createGridLayout( gridLayout ) );
    final GridDataType griddata = FACTORY.createGridDataType();
    griddata.setGrabExcessHorizontalSpace( Boolean.TRUE );
    griddata.setGrabExcessVerticalSpace( Boolean.TRUE );
    griddata.setHorizontalAlignment( "GridData.FILL" );
    griddata.setVerticalAlignment( "GridData.FILL" );
    featureview.setLayoutData( FACTORY.createGridData( griddata ) );

    final List<JAXBElement< ? extends ControlType>> controlList = featureview.getControl();
    for( final IPropertyType ftp : type.getProperties() )
    {
      final Object value = feature == null ? null : feature.getProperty( ftp );
      addDefaultFeatureControlTypeForProperty( controlList, ftp, value );
    }

    return featureview;
  }
}