package org.kalypso.ogc.gml.featureview.control;

import org.deegree.model.feature.Feature;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;

/**
 * @author belger
 */
public interface IFeatureControl
{
  public void dispose();
  
  public Control createControl( final Composite parent, final int style );
  
  public void setProperty( final String name );
  
  public String getProperty( );
  
  public void setFeature( final KalypsoFeatureLayer layer, final Feature feature );
  
  public Feature getFeature();
  
  public void setEnabled( final boolean enabled );

  /** Update Control from Feature  */
  public void updateControl();
  
  /** Write control value to feature */
  public abstract void commitControl();
}
