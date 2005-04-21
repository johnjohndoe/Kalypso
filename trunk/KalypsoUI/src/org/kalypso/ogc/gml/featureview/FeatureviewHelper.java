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
import java.util.Locale;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.Validator;

import org.kalypso.template.featureview.ButtonType;
import org.kalypso.template.featureview.CheckboxType;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.Featureview;
import org.kalypso.template.featureview.GridDataType;
import org.kalypso.template.featureview.GridLayoutType;
import org.kalypso.template.featureview.Group;
import org.kalypso.template.featureview.LabelType;
import org.kalypso.template.featureview.ObjectFactory;
import org.kalypso.template.featureview.Subcomposite;
import org.kalypso.template.featureview.TableType;
import org.kalypso.template.featureview.TextType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.preferences.IKalypsoPreferences;
import org.kalypsodeegree.model.feature.Annotation;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;

/**
 * @author belger
 */
public class FeatureviewHelper
{
  private FeatureviewHelper()
  {
  // wird nicht instantiiert
  }

  public static final ObjectFactory FACTORY = new ObjectFactory();

  public static Unmarshaller UNMARSHALLER;

  static
  {
    try
    {
      UNMARSHALLER = FACTORY.createUnmarshaller();
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();
    }
  }

  private static void addDefaultFeatureControlTypeForProperty( final List controlList,
      final FeatureType featureType, final FeatureTypeProperty ftp ) throws JAXBException
  {
    final ControlType type;
    boolean addLabel = true;
    final GridDataType griddata = FACTORY.createGridData();

    final String typename = ftp.getType();
    final String name = ftp.getName();
    if( "boundedBy".equals( name ) )
      return;
    else if( "java.lang.String|java.lang.Integer|java.lang.Long|java.lang.Float|java.lang.Double|java.util.Date"
        .indexOf( typename ) != -1 )
    {
      final TextType editor = FACTORY.createText();
      editor.setStyle( "SWT.NONE" );
      editor.setEditable( true );
      editor.setProperty( name );

      griddata.setHorizontalAlignment( "GridData.FILL" );
      editor.setLayoutData( griddata );

      type = editor;
    }
    else if( "java.lang.Boolean".equals( typename ) )
    {
      final CheckboxType checkbox = FACTORY.createCheckbox();
      checkbox.setStyle( "SWT.NONE" );
      checkbox.setEditable( true );
      checkbox.setProperty( name );

      griddata.setHorizontalAlignment( "GridData.BEGINNING" );
      checkbox.setLayoutData( griddata );

      type = checkbox;
    }
    else if( "FeatureAssociationType".equals( typename ) )
    {
      if( featureType.getMaxOccurs( name ) != 1 )
      {
        final TableType table = FACTORY.createTable();
        table.setStyle( "SWT.NONE" );
        table.setProperty( name );

        griddata.setHorizontalAlignment( "GridData.FILL" );
        griddata.setVerticalAlignment( "GridData.FILL" );
        griddata.setGrabExcessHorizontalSpace( true );
        griddata.setGrabExcessVerticalSpace( true );
        griddata.setHorizontalSpan( 2 );

        table.setLayoutData( griddata );

        type = table;
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

        compo.setLayoutData( griddata );

        final Group group = FACTORY.createGroup();

        final GridDataType groupdata = FACTORY.createGridData();
        groupdata.setGrabExcessHorizontalSpace( true );
        groupdata.setGrabExcessVerticalSpace( true );
        groupdata.setHorizontalAlignment( "GridData.FILL" );
        groupdata.setVerticalAlignment( "GridData.FILL" );
        groupdata.setHorizontalSpan( 2 );

        //        final String lang = Locale.getDefault().getLanguage();
        final String lang = KalypsoGisPlugin.getDefault().getPluginPreferences().getString(
            IKalypsoPreferences.LANGUAGE );
        final Annotation annotation = ftp.getAnnotation( lang );
        final String text = annotation == null ? name : annotation.getLabel();
        final String tooltip = annotation == null ? null : annotation.getTooltip();

        group.setLayoutData( groupdata );
        group.setText( text );
        group.setTooltip( tooltip );
        group.setStyle( "SWT.NONE" );

        final GridLayoutType gridLayout = FACTORY.createGridLayout();
        gridLayout.setNumColumns( 2 );
        group.setLayout( gridLayout );

        group.getControl().add( compo );

        type = group;
      }
      addLabel = false;

    }
    else
    {
      final ButtonType button = FACTORY.createButton();
      button.setStyle( "SWT.PUSH" );
      button.setProperty( name );

      griddata.setHorizontalAlignment( "GridData.BEGINNING" );
      //      griddata.setWidthHint( 100 );
      button.setLayoutData( griddata );

      type = button;
    }

//    final String lang = Locale.getDefault().getLanguage();
    final String lang = KalypsoGisPlugin.getDefault().getPluginPreferences().getString(
        IKalypsoPreferences.LANGUAGE );
    final Annotation annotation = ftp.getAnnotation( lang );
    final String text = annotation == null ? name : annotation.getLabel();
    final String tooltip = annotation == null ? null : annotation.getTooltip();

    if( type != null )
      type.setTooltip( tooltip );

    if( addLabel )
    {
      final LabelType label = FACTORY.createLabel();
      label.setStyle( "SWT.NONE" );

      label.setText( text );
      label.setTooltip( tooltip );
      label.setVisible( true );

      final GridDataType labelGridData = FACTORY.createGridData();
      labelGridData.setGrabExcessHorizontalSpace( false );
      labelGridData.setHorizontalAlignment( "GridData.BEGINNING" );
      label.setLayoutData( labelGridData );

      controlList.add( label );
    }

    if( type != null )
      controlList.add( type );
  }

  /**
   * Standardview erzeugen
   * 
   * @param type
   * @return featureview
   */
  public static Featureview createFeatureviewFromFeatureType( final FeatureType type )
  {
    try
    {
      final Featureview featureview = FACTORY.createFeatureview();
      featureview.setTypename( type.getName() );
      featureview.setStyle( "SWT.NONE" );

      final GridLayoutType gridLayout = FACTORY.createGridLayout();
      gridLayout.setNumColumns( 2 );
      featureview.setLayout( gridLayout );
      final GridDataType griddata = FACTORY.createGridData();
      griddata.setGrabExcessHorizontalSpace( true );
      griddata.setGrabExcessVerticalSpace( true );
      griddata.setHorizontalAlignment( "GridData.FILL" );
      griddata.setVerticalAlignment( "GridData.FILL" );
      featureview.setLayoutData( griddata );

      final List controlList = featureview.getControl();

      final FeatureTypeProperty[] properties = type.getProperties();
      for( int i = 0; i < properties.length; i++ )
      {
        final FeatureTypeProperty ftp = properties[i];

        addDefaultFeatureControlTypeForProperty( controlList, type, ftp );
      }

      final Validator validator = FACTORY.createValidator();
      validator.validate( featureview );

      return featureview;
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();

      return null;
    }
  }

  /**
   * Standardview für eine Property erzeugen
   * 
   * @param type
   * @return featureview
   */
  public static Featureview createFeatureviewFromFeatureTypeProperty( final FeatureType type,
      final FeatureTypeProperty ftp )
  {
    try
    {
      final Featureview featureview = FACTORY.createFeatureview();
      featureview.setTypename( type.getName() );
      featureview.setStyle( "SWT.NONE" );

      final GridLayoutType gridLayout = FACTORY.createGridLayout();
      gridLayout.setNumColumns( 2 );
      featureview.setLayout( gridLayout );
      final GridDataType griddata = FACTORY.createGridData();
      griddata.setGrabExcessHorizontalSpace( true );
      griddata.setGrabExcessVerticalSpace( true );
      griddata.setHorizontalAlignment( "GridData.FILL" );
      griddata.setVerticalAlignment( "GridData.FILL" );
      featureview.setLayoutData( griddata );

      final List controlList = featureview.getControl();

      addDefaultFeatureControlTypeForProperty( controlList, type, ftp );

      final Validator validator = FACTORY.createValidator();
      validator.validate( featureview );

      return featureview;
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();

      return null;
    }
  }

}