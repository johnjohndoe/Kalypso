package org.kalypso.ogc.gml.featureview.control;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Collection;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Text;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.ogc.gml.featureview.IFeatureControl;

/**
 * @author belger
 */
public class TextFeatureControl implements IFeatureControl, ModellEventListener
{
  private static final DateFormat DATE_FORMATTER = new SimpleDateFormat();

  private final Color m_errorColor = Display.getCurrent().getSystemColor( SWT.COLOR_RED );

  private final Feature m_feature;

  private final String m_propertyName;

  private Text m_text = null;

  private boolean m_isValid = false;

  public TextFeatureControl( final Feature feature, final String propertyName )
  {
    m_feature = feature;

    m_propertyName = propertyName;
  }

  /**
   * @see org.eclipse.swt.widgets.Widget#dispose()
   */
  public void dispose()
  {
    if( m_text != null )
      m_text.dispose();

    m_text.dispose();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#getFeature()
   */
  public final Feature getFeature()
  {
    return m_feature;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#createControl(org.eclipse.swt.widgets.Composite,
   *      int)
   */
  public Control createControl( final Composite parent, final int style )
  {
    m_text = new Text( parent, style );

    m_text.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        updateValid();
      }
    } );

    updateControl();

    return m_text;
  }

  protected void setValid( final boolean valid )
  {
    if( m_isValid != valid )
    {
      m_isValid = valid;

      m_text.setForeground( m_isValid ? null : m_errorColor );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#isValid()
   */
  public boolean isValid()
  {
    return m_isValid;
  }

  public void setEnabled( final boolean enabled )
  {
    m_text.setEnabled( enabled );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#updateControl()
   */
  public void updateControl()
  {
    if( m_text == null || m_text.isDisposed() )
      return;

    final Feature feature = getFeature();

    if( feature == null || m_propertyName == null )
      m_text.setText( "<no data>" );
    else
    {
      // compare with old to prevent loop
      final String newText = getTextFromFeature();
      final String oldText = m_text.getText();
      if( newText.compareTo( oldText ) != 0 )
        m_text.setText( newText );
    }

    setValid( true );
  }

  private String getTextFromFeature()
  {
    final Feature feature = getFeature();
    final String type = feature.getFeatureType().getProperty( m_propertyName ).getType();
    final Object data = feature.getProperty( m_propertyName );

    if( data == null )
      return "";

    if( "java.util.Date".equals( type ) )
      return DATE_FORMATTER.format( data );

    return data.toString();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#collectChanges(java.util.Collection)
   */
  public void collectChanges( final Collection c )
  {
    updateValid();
    if( !isValid() )
      return;

    final Feature feature = getFeature();

    final String text = m_text.getText();

    Object newData = null;
    try
    {
      newData = parseData( text );
    }
    catch( final Exception nfe )
    {
      nfe.printStackTrace();
    }

    final Object oldData = feature.getProperty( m_propertyName );

    // nur ändern, wenn sich wirklich was geändert hat
    if( ( newData == null && oldData != null ) || ( newData != null && !newData.equals( oldData ) ) )
      c.add( new FeatureChange( feature, m_propertyName, newData ) );
  }

  private Object parseData( final String text ) throws Exception
  {
    final String typeName = getFeature().getFeatureType().getProperty( m_propertyName ).getType();
    if( typeName.equals( "java.lang.Double" ) )
      return new Double( text );
    if( typeName.equals( "java.lang.Integer" ) )
      return new Integer( text );
    if( typeName.equals( "java.lang.Float" ) )
      return new Float( text );
    if( typeName.equals( "java.lang.Long" ) )
      return new Long( text );
    if( typeName.equals( "java.lang.Boolean" ) )
      return new Boolean( text );
    if( typeName.equals( "java.util.Date" ) )
      return DATE_FORMATTER.parse( text );

    if( text.length() == 0 )
      return null;
    return text;
  }

  protected void updateValid()
  {
    try
    {
      parseData( m_text.getText() );
      setValid( true );
    }
    catch( final Exception e )
    {
      setValid( false );
    }
  }

  /**
   * @see org.deegree.model.feature.event.ModellEventListener#onModellChange(org.deegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    updateControl();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#addModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void addModifyListener( final ModifyListener l )
  {
    m_text.addModifyListener( l );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#removeModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void removeModifyListener( final ModifyListener l )
  {
    m_text.removeModifyListener( l );
  }
}