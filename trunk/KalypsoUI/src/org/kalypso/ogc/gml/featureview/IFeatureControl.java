package org.kalypso.ogc.gml.featureview;

import java.util.Collection;

import org.deegree.model.feature.Feature;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/**
 * @author belger
 */
public interface IFeatureControl
{
  public Control createControl( final Composite parent, final int style );

  public void dispose();
  
  public Feature getFeature();
  
  public void setFeature( final Feature feature );
  
  /** Render this control as string */
  public String toString();
  
  /** Update Control from Feature  */
  public void updateControl();

  /** Adds {@link FeatureChange} objects to a collection, representing changes to features */
  public void collectChanges( final Collection c );

  public boolean isValid();
  
  public void addModifyListener( final ModifyListener l );
  public void removeModifyListener( final ModifyListener l );
}
