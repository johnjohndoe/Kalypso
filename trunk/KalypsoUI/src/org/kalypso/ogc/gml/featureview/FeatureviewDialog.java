package org.kalypso.ogc.gml.featureview;

import org.deegree.model.feature.Feature;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;

/**
 * @author belger
 */
public class FeatureviewDialog extends Dialog
{
  private final Feature m_feature;
  private final FeatureviewHelper m_factory;
  private final KalypsoFeatureLayer m_layer;

  public FeatureviewDialog( final Shell parentShell, final KalypsoFeatureLayer layer, final Feature feature, final FeatureviewHelper factory )
  { 
    super( parentShell );
    
    m_layer = layer;
    m_feature = feature;
    m_factory = factory;
  }
  
  /**
   * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  protected Control createDialogArea( final Composite parent )
  {
    getShell().setText( "Feature editieren" );
    
    return m_factory.createFeatureControl( parent, m_layer, m_feature );
  }

}
