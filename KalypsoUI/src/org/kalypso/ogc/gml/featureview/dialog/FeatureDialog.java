package org.kalypso.ogc.gml.featureview.dialog;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.commons.command.DefaultCommandManager;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.ogc.gml.featureview.FeatureComposite;
import org.kalypso.ogc.gml.featureview.FeatureviewDialog;
import org.kalypso.ogc.gml.featureview.FeatureviewHelper;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.template.featureview.FeatureviewType;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author belger
 */
public class FeatureDialog implements IFeatureDialog
{
  private final Feature m_feature;

  private final Collection<FeatureChange> m_changes = new ArrayList<FeatureChange>();

  private final IPropertyType m_ftp;

  private ICommandTarget m_target = new JobExclusiveCommandTarget( new DefaultCommandManager(), null );

  private final GMLWorkspace m_workspace;

  private final IFeatureSelectionManager m_selectionManager;

  /**
   * FeatureDialog that shows a property of a feature to edit, usually the property is type of FeatureAssociationType
   * and maxOccurs is greater than 1, so ist a table inside
   */
  public FeatureDialog( final GMLWorkspace workspace, final Feature feature, final IPropertyType ftp, final IFeatureSelectionManager selectionManager )
  {
    m_workspace = workspace;
    m_feature = feature;
    m_ftp = ftp;
    m_selectionManager = selectionManager;
  }

  /**
   * FeatureDialog that allows complete editing of the given feature
   */
  public FeatureDialog( final GMLWorkspace workspace, final Feature feature, final IFeatureSelectionManager selectionManager )
  {
    this( workspace, feature, null, selectionManager );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#open(org.eclipse.swt.widgets.Shell)
   */
  public int open( final Shell shell )
  {
    final FeatureviewType fvType;
    if( m_ftp != null )
      fvType = FeatureviewHelper.createFeatureviewFromFeatureTypeProperty( m_feature.getFeatureType(), m_ftp );
    else
      fvType = FeatureviewHelper.createFeatureviewFromFeatureType( m_feature.getFeatureType() );

    final CommandableWorkspace commwork;
    if( m_workspace instanceof CommandableWorkspace )
      commwork = (CommandableWorkspace)m_workspace;
    else
      commwork = new CommandableWorkspace( m_workspace );

    final FeatureComposite composite = new FeatureComposite( commwork, m_feature, m_selectionManager, new FeatureviewType[]
    { fvType } );

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
  public void collectChanges( final Collection<FeatureChange> c )
  {
    c.addAll( m_changes );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#getLabel()
   */
  public String getLabel()
  {
    // TODO: use annotations
    return m_feature.getFeatureType().getQName().getLocalPart();
  }
}