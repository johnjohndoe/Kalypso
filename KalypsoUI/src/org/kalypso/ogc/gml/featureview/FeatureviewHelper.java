package org.kalypso.ogc.gml.featureview;

import java.util.List;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.Validator;

import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.kalypso.template.featureview.ButtonType;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.Featureview;
import org.kalypso.template.featureview.GridDataType;
import org.kalypso.template.featureview.GridLayoutType;
import org.kalypso.template.featureview.LabelType;
import org.kalypso.template.featureview.ObjectFactory;
import org.kalypso.template.featureview.TextType;

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
      editor.setStyle( SWT.BORDER );
      editor.setEditable( true );
      editor.setProperty( ftp.getName() );

      griddata.setHorizontalAlignment( GridData.BEGINNING );
      griddata.setWidthHint( 100 );
      editor.setLayoutData( griddata );

      return editor;
    }

    final ButtonType button = FACTORY.createButton();
    button.setStyle( SWT.PUSH );
    button.setProperty( ftp.getName() );

    griddata.setHorizontalAlignment( GridData.CENTER );
    griddata.setWidthHint( 100 );
    button.setLayoutData( griddata );

    return button;
  }

  /** Standardview erzeugen */
  public static Featureview createFeatureviewFromFeatureType( final FeatureType type )
  {
    try
    {
      final Featureview featureview = FACTORY.createFeatureview();
      featureview.setTypename( type.getName() );
      featureview.setStyle( SWT.NONE );

      final GridLayoutType gridLayout = FACTORY.createGridLayout();
      gridLayout.setNumColumns( 2 );
      featureview.setLayout( gridLayout );
      final GridDataType griddata = FACTORY.createGridData();
      griddata.setGrabExcessHorizontalSpace( true );
      griddata.setHorizontalAlignment( GridData.FILL );
      featureview.setLayoutData( griddata );

      final List controlList = featureview.getControl();

      final FeatureTypeProperty[] properties = type.getProperties();
      for( int i = 0; i < properties.length; i++ )
      {
        final FeatureTypeProperty ftp = properties[i];

        final LabelType label = FACTORY.createLabel();
        label.setStyle( SWT.NONE );
        label.setText( ftp.getName() );
        label.setVisible( true );

        final GridDataType labelGridData = FACTORY.createGridData();
        labelGridData.setGrabExcessHorizontalSpace( false );
        labelGridData.setHorizontalAlignment( GridData.BEGINNING );
        label.setLayoutData( labelGridData );

        controlList.add( label );

        final ControlType cc = createDefaultFeatureControlTypeForProperty( ftp );
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