package org.kalypso.ogc.gml.featureview.control;

import java.util.Collection;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureTypeProperty;
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
import org.kalypso.ogc.gml.featureview.IFeatureModifier;
import org.kalypso.ogc.gml.featureview.modfier.StringModifier;

/**
 * @author belger
 */
public class TextFeatureControl extends AbstractFeatureControl implements ModellEventListener
{
  private final Color m_errorColor = Display.getCurrent().getSystemColor( SWT.COLOR_RED );

  private Text m_text = null;

  private boolean m_isValid = false;

  private final IFeatureModifier m_modifier;

  public TextFeatureControl( final FeatureTypeProperty ftp )
  {
    this( null, ftp );
  }

  public TextFeatureControl( final Feature feature, final FeatureTypeProperty ftp )
  {
    super( feature, ftp );
    
    m_modifier = new StringModifier( ftp );
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

    if( feature == null || getFeatureTypeProperty() == null )
      m_text.setText( "<no data>" );
    else
    {
      // compare with old to prevent loop
      final String newText = toString();
      final String oldText = m_text.getText();
      if( newText.compareTo( oldText ) != 0 )
        m_text.setText( newText );
    }

    setValid( true );
  }

  public String toString()
  {
    return m_modifier.getValue( getFeature() ).toString(); 
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

    final Object newData = m_modifier.parseInput( getFeature(), text );

    final String name = getFeatureTypeProperty().getName();
    final Object oldData = feature.getProperty( name );

    // nur �ndern, wenn sich wirklich was ge�ndert hat
    if( ( newData == null && oldData != null ) || ( newData != null && !newData.equals( oldData ) ) )
      c.add( new FeatureChange( feature, name, newData ) );
  }

  protected void updateValid()
  {
    setValid( m_modifier.isValid( m_text.getText() ) == null );
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