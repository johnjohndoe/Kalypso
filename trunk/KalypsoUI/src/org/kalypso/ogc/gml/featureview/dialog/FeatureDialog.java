package org.kalypso.ogc.gml.featureview.dialog;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.ogc.gml.featureview.FeatureComposite;
import org.kalypso.ogc.gml.featureview.FeatureviewDialog;
import org.kalypso.util.command.ICommandTarget;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.event.ModellEventProvider;

/**
 * @author belger
 */
public class FeatureDialog implements IFeatureDialog
{
  private final ModellEventProvider m_mep;
  private final ICommandTarget m_target;
  private final Feature m_feature;
  private final Collection m_changes = new ArrayList();

  public FeatureDialog( final ModellEventProvider mep, final ICommandTarget target, final Feature feature )
  {
    m_mep = mep;
    m_target = target;
    m_feature = feature;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#open(org.eclipse.swt.widgets.Shell)
   */
  public int open( final Shell shell )
  {
    final FeatureComposite composite = new FeatureComposite( m_mep, m_target, m_feature );

    final FeatureviewDialog dialog = new FeatureviewDialog( shell, m_mep, composite,
        m_target );
    final int result = dialog.open();
    
    if( result == Window.OK )
    {
      m_changes.clear();
      dialog.collectChanges( m_changes );
    }
    
    composite.dispose();
    return result;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#collectChanges(java.util.Collection)
   */
  public void collectChanges( final Collection c )
  {
    c.addAll( m_changes );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#getLabel()
   */
  public String getLabel()
  {
    // TODO: use annotations
    return m_feature.getFeatureType().getName();
  }

}
