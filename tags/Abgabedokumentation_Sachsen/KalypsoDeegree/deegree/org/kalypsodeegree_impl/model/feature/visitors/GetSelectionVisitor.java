package org.kalypsodeegree_impl.model.feature.visitors;

import java.util.ArrayList;
import java.util.List;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author belger
 */
public class GetSelectionVisitor implements FeatureVisitor
{
  public static List getSelectedFeatures( final GMLWorkspace workspace, final int selectionID )
  {
    final GetSelectionVisitor visitor = new GetSelectionVisitor( selectionID );
    workspace.accept( visitor, workspace.getRootFeature(), FeatureVisitor.DEPTH_ZERO );

    return visitor.getSelectedFeatures();
  }

  public static List getSelectedFeatures( final GMLWorkspace workspace, final FeatureType ft,
      final int selectionID )
  {
    final GetSelectionVisitor visitor = new GetSelectionVisitor( selectionID );
    workspace.accept( visitor, ft, FeatureVisitor.DEPTH_ZERO );
    return visitor.getSelectedFeatures();
  }

  public static List getSelectedFeatures( final FeatureList features, final int selectionID )
  {
    final GetSelectionVisitor visitor = new GetSelectionVisitor( selectionID );
    features.accept( visitor );
    return visitor.getSelectedFeatures();
  }

  /** List<Feature> */
  private final List m_selectedFeatures = new ArrayList();

  private final int m_selectionID;

  public GetSelectionVisitor( final int selectionID )
  {
    m_selectionID = selectionID;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    if( f.isSelected( m_selectionID ) )
      m_selectedFeatures.add( f );

    return true;
  }

  public List getSelectedFeatures()
  {
    return m_selectedFeatures;
  }
}