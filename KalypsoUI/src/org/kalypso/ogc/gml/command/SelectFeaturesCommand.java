package org.kalypso.ogc.gml.command;

import java.util.Iterator;
import java.util.List;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree_impl.model.feature.visitors.GetSelectionVisitor;
import org.deegree_impl.model.feature.visitors.UnselectFeatureVisitor;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.util.command.ICommand;

/**
 * Setzt die selektierten Features innerhalb eines Workspace
 * 
 * @author belger
 */
public class SelectFeaturesCommand implements ICommand
{
  private final CommandableWorkspace m_workspace;

  private final Feature[] m_selection;

  private final int m_selectionID;

  private List m_selectedFeatures;

  private final UnselectFeatureVisitor m_unselectVisitor;

  public SelectFeaturesCommand( final CommandableWorkspace workspace, final Feature[] selection,
      final int selectionID )
  {
    m_workspace = workspace;
    m_selection = selection;
    m_selectionID = selectionID;

    m_selectedFeatures = GetSelectionVisitor.getSelectedFeatures( workspace, selectionID );
    m_unselectVisitor = new UnselectFeatureVisitor( m_selectionID );
  }

  /**
   * @see org.kalypso.util.command.ICommand#isUndoable()
   */
  public boolean isUndoable()
  {
    return true;
  }

  /**
   * @see org.kalypso.util.command.ICommand#process()
   */
  public void process() throws Exception
  {
    m_workspace.accept( m_unselectVisitor, m_workspace.getRootFeature(), m_selectionID );

    for( int i = 0; i < m_selection.length; i++ )
      m_selection[i].select( m_selectionID );

    m_workspace.fireModellEvent( new ModellEvent( m_workspace, ModellEvent.SELECTION_CHANGED ) );
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    process();
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    m_workspace.accept( m_unselectVisitor, m_workspace.getRootFeature(), m_selectionID );

    for( final Iterator selIt = m_selectedFeatures.iterator(); selIt.hasNext(); )
      ( (Feature)selIt.next() ).select( m_selectionID );

    m_workspace.fireModellEvent( new ModellEvent( m_workspace, ModellEvent.SELECTION_CHANGED ) );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Objekte selektieren";
  }

}