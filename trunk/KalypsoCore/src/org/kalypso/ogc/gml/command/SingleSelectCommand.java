/**
 * TODO: license definieren
 */

package org.kalypso.ogc.gml.command;

import java.util.Iterator;
import java.util.List;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureVisitor;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventProvider;
import org.deegree_impl.model.feature.visitors.GetSelectionVisitor;
import org.deegree_impl.model.feature.visitors.UnselectFeatureVisitor;
import org.kalypso.util.command.ICommand;

/**
 * @author doemming
 */
public class SingleSelectCommand implements ICommand
{
  private final Feature m_feature;

  private final int mySelectionId;

  private final ModellEventProvider m_modellEventProvider;

  private final GMLWorkspace m_workspace;

  private final FeatureVisitor m_unselectVisitor;

  private List m_selectedFeatures;

  public SingleSelectCommand( final GMLWorkspace workspace, final Feature feature, int selectionId,
      ModellEventProvider eventProvider )
  {
    m_workspace = workspace;
    m_feature = feature;
    mySelectionId = selectionId;

    m_modellEventProvider = eventProvider;
    m_unselectVisitor = new UnselectFeatureVisitor( selectionId );
    
    m_selectedFeatures = GetSelectionVisitor.getSelectedFeatures( workspace, selectionId );
  }

  public boolean isUndoable()
  {
    return true;
  }

  public void process() throws Exception
  {
    redo();
  }

  public void redo() throws Exception
  {
    m_workspace.accept( m_unselectVisitor, m_workspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );
    m_feature.select( mySelectionId );

    m_modellEventProvider.fireModellEvent( new ModellEvent( m_modellEventProvider, ModellEvent.SELECTION_CHANGED ) );
  }

  public void undo() throws Exception
  {
    m_feature.unselect( mySelectionId );
    for( final Iterator iter = m_selectedFeatures.iterator(); iter.hasNext(); )
      ((Feature)iter.next()).select( mySelectionId );
    
    m_modellEventProvider.fireModellEvent( new ModellEvent( m_modellEventProvider, ModellEvent.SELECTION_CHANGED ) );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Objekt selektieren";
  }
}