package org.kalypso.ogc.gml.featureview.control;

import java.util.Date;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureProperty;
import org.deegree_impl.model.feature.FeatureFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;

/**
 * @author belger
 */
public class TextFeatureControl extends AbstractFeatureControl
{
  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#createControl(org.eclipse.swt.widgets.Composite,
   *      int)
   */
  public Control createControl( final Composite parent, final int style )
  {
    final Text text = new Text( parent, style );

    setControl( text );

    text.addFocusListener( new FocusAdapter()
    {
      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      public void focusLost( FocusEvent e )
      {
        commitControl();
      }
    } );

    return text;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#setEnabled(boolean)
   */
  public void setEnabled( final boolean enabled )
  {
    ( (Text)getControl() ).setEnabled( enabled );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#updateControl()
   */
  public void updateControl()
  {
    final Text textControl = (Text)getControl();
    if( textControl == null || textControl.isDisposed() )
      return;

    final Feature feature = getFeature();
    final String name = getProperty();

    if( feature == null || name == null )
      textControl.setText( "<no data>" );
    else
    {
      // compare with old to prevent loop
      final String newText = getTextFromFeature();
      final String oldText = textControl.getText();
      if( newText.compareTo( oldText ) != 0 )
        textControl.setText( newText );
    }

    feature.getProperty( getProperty() );
  }

  private String getTextFromFeature()
  {
    final Feature feature = getFeature();
    final String name = getProperty();
    final Object data = feature.getProperty( name );

    if( data == null )
      return "";

    return data.toString();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#commitControl()
   */
  public void commitControl()
  {
    final Feature feature = getFeature();
    final String name = getProperty();

    final Text textControl = (Text)getControl();
    textControl.setForeground( null );

    Object newData = null;

    final String text = textControl.getText();

    try
    {
      final String typeName = feature.getFeatureType().getProperty( name ).getType();
      if( typeName.equals( "java.lang.Double" ) )
        newData = new Double( text );
      if( typeName.equals( "java.lang.Integer" ) )
        newData = new Integer( text );
      if( typeName.equals( "java.lang.Float" ) )
        newData = new Float( text );
      if( typeName.equals( "java.lang.Long" ) )
        newData = new Long( text );
      if( typeName.equals( "java.lang.Boolean" ) )
        newData = new Boolean( text );
      if( typeName.equals( "java.util.Date" ) )
        newData = new Date( text );
      else if( typeName.equals( "java.lang.String" ) )
        newData = text;
    }
    catch( final NumberFormatException nfe )
    {
      textControl.setForeground( textControl.getDisplay().getSystemColor( SWT.COLOR_RED ) );

      updateControl();
      return;
    }

    final Object oldData = feature.getProperty( name );

    // nur ändern, wenn sich wirklich was geändert hat
    if( ( newData == null && oldData != null ) || ( newData != null && !newData.equals( oldData ) ) )
    {
      final FeatureProperty fp = FeatureFactory.createFeatureProperty( name, newData );
      feature.setProperty( fp );

      fireFeatureChanged();
    }
  }
}