package org.kalypso.ogc.gml.featureview.dialog;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.ogc.gml.featureview.FeatureComposite;
import org.kalypso.ogc.gml.featureview.FeatureviewDialog;
import org.kalypso.ogc.gml.featureview.FeatureviewHelper;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.template.featureview.FeatureviewType;
import org.kalypso.util.command.DefaultCommandManager;
import org.kalypso.util.command.ICommandTarget;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;

/**
 * @author belger
 */
public class FeatureDialog implements IFeatureDialog
{
  private final Feature m_feature;
  private final Collection m_changes = new ArrayList();
  private final FeatureTypeProperty m_ftp;
  private ICommandTarget m_target = new JobExclusiveCommandTarget( new DefaultCommandManager(), null );

  public FeatureDialog( final Feature feature, final FeatureTypeProperty ftp )
  {
    m_feature = feature;
    m_ftp = ftp;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#open(org.eclipse.swt.widgets.Shell)
   */
  public int open( final Shell shell )
  {
    final FeatureviewType fvType = FeatureviewHelper.createFeatureviewFromFeatureTypeProperty( m_feature.getFeatureType(), m_ftp );
    final FeatureComposite composite = new FeatureComposite( m_feature, new FeatureviewType[] { fvType } );
    
    final GMLWorkspace workspace = new GMLWorkspace_Impl( new FeatureType[] { m_feature.getFeatureType() }, m_feature, null, null, null, m_feature.getFeatureType().getAnnotationMap() );
    final CommandableWorkspace commwork = new CommandableWorkspace( workspace );
    
    final FeatureviewDialog dialog = new FeatureviewDialog( commwork, m_target, shell, composite );
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
