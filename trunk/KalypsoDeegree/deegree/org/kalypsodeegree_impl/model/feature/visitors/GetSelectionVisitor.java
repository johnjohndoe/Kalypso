package org.deegree_impl.model.feature.visitors;

import java.util.ArrayList;
import java.util.List;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureVisitor;
import org.deegree.model.feature.GMLWorkspace;

/**
 * @author belger
 */
public class GetSelectionVisitor implements FeatureVisitor
{
  public static List getSelectedFeatures( final GMLWorkspace workspace, final FeatureType ft, final int selectionID )
  {
    final GetSelectionVisitor visitor = new GetSelectionVisitor( selectionID );
    try
    {
      workspace.accept( visitor, ft, FeatureVisitor.DEPTH_ZERO );
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
      return null;
    }
    return visitor.getSelectedFeatures();
  }
  
  private final List m_selectedFeatures = new ArrayList();
  private final int m_selectionID;
  
  public GetSelectionVisitor( final int selectionID )
  {
    m_selectionID = selectionID;
  }
  
  /**
   * @see org.deegree.model.feature.FeatureVisitor#visit(org.deegree.model.feature.Feature)
   */
  public boolean visit( final Feature f ) throws Throwable
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
