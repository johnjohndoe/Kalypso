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

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.Validator;

import org.kalypso.template.featureview.ButtonType;
import org.kalypso.template.featureview.CheckboxType;
import org.kalypso.template.featureview.CompositeType;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.Featureview;
import org.kalypso.template.featureview.GridDataType;
import org.kalypso.template.featureview.GridLayoutType;
import org.kalypso.template.featureview.Group;
import org.kalypso.template.featureview.LabelType;
import org.kalypso.template.featureview.ObjectFactory;
import org.kalypso.template.featureview.Subcomposite;
import org.kalypso.template.featureview.TextType;
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

  private static ControlType createDefaultFeatureControlTypeForProperty(
      final FeatureTypeProperty ftp ) throws JAXBException
  {
    final GridDataType griddata = FACTORY.createGridData();

    final String typename = ftp.getType();
    if( "java.lang.String|java.lang.Integer|java.lang.Long|java.lang.Float|java.lang.Double|java.util.Date"
        .indexOf( typename ) != -1 )
    {
      final TextType editor = FACTORY.createText();
      editor.setStyle( "SWT.BORDER" );
      editor.setEditable( true );
      editor.setProperty( ftp.getName() );

      griddata.setHorizontalAlignment( "GridData.BEGINNING" );
      griddata.setWidthHint( 100 );
      editor.setLayoutData( griddata );

      return editor;
    }
    else if( "java.lang.Boolean".equals( typename ) )
    {
      final CheckboxType checkbox = FACTORY.createCheckbox();
      checkbox.setStyle( "SWT.NONE" );
      checkbox.setEditable( true );
      checkbox.setProperty( ftp.getName() );

      griddata.setHorizontalAlignment( "GridData.BEGINNING" );
      griddata.setWidthHint( 100 );
      checkbox.setLayoutData( griddata );

      return checkbox;
    }
    else if( "FeatureAssociationType".equals( typename ) )
    {
      final Subcomposite compo = FACTORY.createSubcomposite();
      compo.setStyle( "SWT.BORDER" );
      compo.setProperty( ftp.getName() );

      griddata.setHorizontalAlignment( "GridData.FILL" );
      griddata.setGrabExcessHorizontalSpace( true );
      griddata.setGrabExcessVerticalSpace( true );
      compo.setLayoutData( griddata );
      
      final Group group = FACTORY.createGroup();

      final GridDataType groupdata = FACTORY.createGridData();
      groupdata.setGrabExcessHorizontalSpace( true );
      groupdata.setGrabExcessVerticalSpace( true );
      groupdata.setHorizontalAlignment( "GridData.FILL" );

      group.setLayoutData( groupdata );
      group.setText( ftp.getName() );
      group.setStyle( "SWT.NONE" );

      final GridLayoutType gridLayout = FACTORY.createGridLayout();
      gridLayout.setNumColumns( 2 );
      group.setLayout( gridLayout );
      
      group.getControl().add( compo );

      return group;
    }

    final ButtonType button = FACTORY.createButton();
    button.setStyle( "SWT.PUSH" );
    button.setProperty( ftp.getName() );

    griddata.setHorizontalAlignment( "GridData.CENTER" );
    griddata.setWidthHint( 100 );
    button.setLayoutData( griddata );

    return button;
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
      griddata.setHorizontalAlignment( "GridData.FILL" );
      featureview.setLayoutData( griddata );

      final List controlList = featureview.getControl();

      final FeatureTypeProperty[] properties = type.getProperties();
      for( int i = 0; i < properties.length; i++ )
      {
        final FeatureTypeProperty ftp = properties[i];

        final ControlType cc = createDefaultFeatureControlTypeForProperty( ftp );

        if( cc instanceof CompositeType )
        {
          final GridDataType layoutData = (GridDataType)cc.getLayoutData();
          layoutData.setHorizontalSpan( 2 );
        }
        else
        {

          final LabelType label = FACTORY.createLabel();
          label.setStyle( "SWT.NONE" );
          label.setText( ftp.getName() );
          label.setVisible( true );

          final GridDataType labelGridData = FACTORY.createGridData();
          labelGridData.setGrabExcessHorizontalSpace( false );
          labelGridData.setHorizontalAlignment( "GridData.BEGINNING" );
          label.setLayoutData( labelGridData );

          controlList.add( label );
        }

        if( cc != null )
          controlList.add( cc );
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

}