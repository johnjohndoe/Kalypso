package org.kalypso.ogc.gml.featureview.control;

import org.deegree.model.feature.Feature;
import org.eclipse.swt.widgets.Control;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.event.ModellEvent;
import org.kalypso.ogc.gml.event.ModellEventListener;

/**
 * Eine Control zum editieren eines Features
 * 
 * TODO: on widget selected: daten ändern
 * 
 * @author belger
 */
public abstract class AbstractFeatureControl implements IFeatureControl, ModellEventListener
{
  private Feature m_feature;
  private KalypsoFeatureLayer m_layer;
  
  private Control m_control = null;
  private String m_propertyName;

  /**
   * @see org.eclipse.swt.widgets.Widget#dispose()
   */
  public void dispose()
  {
    if( m_control != null )
      m_control.dispose();

    // deregister listeners
    setFeature( null, null );
  }
  
  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#setProperty(java.lang.String)
   */
  public final void setProperty( String name )
  {
    m_propertyName = name;
    
    updateControl();
  }
  
  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#getProperty()
   */
  public String getProperty()
  {
    return m_propertyName;
  }
  
  public final void setFeature( final KalypsoFeatureLayer layer, final Feature feature )
  {
    if( m_layer != null )
      m_layer.removeModellListener( this );
      
    m_layer = layer;
    m_feature = feature;

    if( m_layer != null )
      m_layer.addModellListener( this );
    
    updateControl();
  }
  
  public final Feature getFeature()
  {
    return m_feature;
  }

  protected final void fireFeatureChanged()
  {
    m_layer.fireModellEvent( new ModellEvent( m_layer, ModellEvent.FEATURE_CHANGE ) );
  }

  /**
   * @see org.kalypso.ogc.gml.event.ModellEventListener#onModellChange(org.kalypso.ogc.gml.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    updateControl();
  }

  /** 
   * Soll von createControl aufgerufen werden, aber nur einmal 
   */
  protected void setControl( final Control control )
  {
    if( m_control != null )
      throw new IllegalStateException( "control already set" );
    
    if( control == null )
      throw new NullPointerException( "'control' cannnot be null" );
    
    m_control = control;
    
    updateControl();
  }
  
  public Control getControl()
  {
    return m_control;
  }
}
