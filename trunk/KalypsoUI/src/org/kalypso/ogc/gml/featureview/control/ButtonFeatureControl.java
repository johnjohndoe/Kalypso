package org.kalypso.ogc.gml.featureview.control;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.deegree_impl.gml.schema.DateWithoutTime;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.ogc.gml.featureview.dialog.CalendarFeatureDialog;
import org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog;
import org.kalypso.ogc.gml.featureview.dialog.NotImplementedFeatureDialog;

/**
 * @author belger
 */
public class ButtonFeatureControl extends AbstractFeatureControl implements ModellEventListener
{
  private Button m_button;
  private IFeatureDialog m_dialog;
  private Collection m_modifyListener = new ArrayList();

  public ButtonFeatureControl( final FeatureTypeProperty ftp )
  {
    this( null, ftp );
  }


  public ButtonFeatureControl( final Feature feature, final FeatureTypeProperty ftp )
  {
    super( feature, ftp );
    
    m_dialog = chooseDialog( feature, ftp );
  }

  private IFeatureDialog chooseDialog( final Feature feature, final FeatureTypeProperty ftp )
  {
    final String typename = ftp.getType();
    
//    if( typename.equals( "java.lang.String" ) )
//      return text;
//    if( typeName.equals( "java.lang.Double" ) )
//      return new Double( text );
//    if( typeName.equals( "java.lang.Integer" ) )
//      return new Integer( text );
//    if( typeName.equals( "java.lang.Float" ) )
//      return new Float( text );
//    if( typeName.equals( "java.lang.Long" ) )
//      return new Long( text );
//    if( typeName.equals( "java.lang.Boolean" ) )
//      return new Boolean( text );
//    if( typeName.equals( "java.util.Date" ) )
//      return DATE_FORMATTER.parse( text );
    if( DateWithoutTime.class.getName().equals( typename ) )
      return new CalendarFeatureDialog( feature, ftp );
    
    return new NotImplementedFeatureDialog();
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
    
    updateControl();
    
    return m_button;
  }

//  public String toString()
//  {
//    final FeatureTypeProperty ftp = getFeatureTypeProperty();
//    final String typename = ftp.getType();
//    
//    if( typename.startsWith( GM_Object.class.getPackage().getName() ) )
//      return "Geometrie editieren";
//    
//    if( typename.startsWith( Feature.class.getPackage().getName() ) )
//      return "Feature editieren";
//
//    if( TypeRegistrySingleton.getTypeRegistry().hasClassName( typename ) )
//      return TypeRegistrySingleton.getTypeRegistry().getTypeHandlerForClassName( typename ).getShortname() + " editieren";
//    
//    return "<unbekannter Typ>";
//  }

  protected void buttonPressed()
  {
    if( m_dialog.open( m_button.getShell() ) == Window.OK )
    {
      final Collection c = new LinkedList();
      m_dialog.collectChanges( c );
      for( final Iterator iter = c.iterator(); iter.hasNext(); )
        fireChange( (FeatureChange)iter.next() );
      
      fireModfied();
      
      updateControl();
    }
  }
  
  private void fireModfied()
  {
    for( final Iterator iter = m_modifyListener.iterator(); iter.hasNext(); )
    {
      final ModifyListener l = (ModifyListener)iter.next();
      final Event event = new Event(  );
      // TODO: create a real event?
      
      l.modifyText( new ModifyEvent( event ) );
    }
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
    m_dialog.collectChanges( c );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#addModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void addModifyListener( final ModifyListener l )
  {
    m_modifyListener.add( l );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#removeModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void removeModifyListener( final ModifyListener l )
  {
    m_modifyListener.remove( l );
  }

  /**
   * @see org.deegree.model.feature.event.ModellEventListener#onModellChange(org.deegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    updateControl();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#updateControl()
   */
  public void updateControl()
  {
    m_button.setText( m_dialog.getLabel() );
  }
}