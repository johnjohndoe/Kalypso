package org.kalypso.ogc.gml.featureview.control;

import java.util.Collection;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.deegree.model.geometry.GM_Object;
import org.deegree_impl.extension.TypeRegistrySingleton;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/**
 * @author belger
 */
public class ButtonFeatureControl extends AbstractFeatureControl implements ModellEventListener
{
  private Button m_button;

  public ButtonFeatureControl( final FeatureTypeProperty ftp )
  {
    this( null, ftp );
  }

  public ButtonFeatureControl( final Feature feature, final FeatureTypeProperty ftp )
  {
    super( feature, ftp );
  }

  /**
   * @see org.eclipse.swt.widgets.Widget#dispose()
   */
  public void dispose()
  {
    if( !( m_button.isDisposed() ) )
        m_button.dispose();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#createControl(org.eclipse.swt.widgets.Composite,
   *      int)
   */
  public Control createControl( final Composite parent, final int style )
  {
    m_button = new Button( parent, style );
    
    m_button.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      public void widgetSelected( SelectionEvent e )
      {
        buttonPressed();
      }
    } );

    return m_button;
  }

  public String toString()
  {
    final FeatureTypeProperty ftp = getFeatureTypeProperty();
    final String typename = ftp.getType();
    
    if( typename.startsWith( GM_Object.class.getPackage().getName() ) )
      return "Geometrie editieren";
    
    if( typename.startsWith( Feature.class.getPackage().getName() ) )
      return "Feature editieren";
    
    if( TypeRegistrySingleton.getTypeRegistry().hasClassName( typename ) )
      return TypeRegistrySingleton.getTypeRegistry().getTypeHandlerForClassName( typename ).getShortname() + " editieren";
    
    return "<unbekannter Typ>";
  }

  protected void buttonPressed()
  {
    MessageDialog.openInformation( m_button.getShell(), "Feature editieren", "Die Operation ist noch nicht implementiert." );
    
    // todo: fireChange(  );
    // todo: fireModify
  }
  
  /**
   * Die ButtonControl ist immer valid
   * 
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#isValid()
   */
  public boolean isValid()
  {
    return true;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#collectChanges(java.util.Collection)
   */
  public void collectChanges( final Collection c )
  {
  // nix tun, später vom child sammeln
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#addModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void addModifyListener( final ModifyListener l )
  {
  //
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#removeModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void removeModifyListener( final ModifyListener l )
  {
  //
  }

  /**
   * @see org.deegree.model.feature.event.ModellEventListener#onModellChange(org.deegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( ModellEvent modellEvent )
  {
  //  
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#updateControl()
   */
  public void updateControl()
  {
    m_button.setText( toString() );
  }
}