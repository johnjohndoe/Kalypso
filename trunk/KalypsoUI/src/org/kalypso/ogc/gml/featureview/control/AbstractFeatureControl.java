package org.kalypso.ogc.gml.featureview.control;

import org.deegree.model.feature.Feature;
import org.eclipse.swt.widgets.Composite;
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
public abstract class AbstractFeatureControl extends Control implements ModellEventListener
{
  private Feature m_feature;
  private KalypsoFeatureLayer m_layer;

  public AbstractFeatureControl( final Composite parent, final int style )
  {
    super( parent, style );
  }
  
  /**
   * @see org.eclipse.swt.widgets.Widget#dispose()
   */
  public void dispose()
  {
    super.dispose();
    
    // deregister listeners
    setFeature( null, null );
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

  /**
   * @see org.kalypso.ogc.gml.event.ModellEventListener#onModellChange(org.kalypso.ogc.gml.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    updateControl();
  }

  /** Update Control from Feature  */
  protected abstract void updateControl();
  
  /** Write control value to feature */
  protected abstract void commitControl();
}
