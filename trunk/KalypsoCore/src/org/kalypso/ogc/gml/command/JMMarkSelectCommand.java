package org.kalypso.ogc.gml.command;

import java.util.List;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.GMLWorkspace;
import org.kalypso.util.command.ICommand;

/**
 * DOCUMENT ME!
 * 
 * @author doemming
 */
public class JMMarkSelectCommand implements ICommand
{
  private final List m_features;

  private final int mySelectionId;

  private final int mySelectionMode;

  private final GMLWorkspace m_workspace;

  public JMMarkSelectCommand( final GMLWorkspace workspace, final List features, int selectionId,
      int selectionModus )
  {
    m_workspace = workspace;
    m_features = features;
    mySelectionId = selectionId;
    mySelectionMode = selectionModus;
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
    for( int i = 0; i < m_features.size(); i++ )
    {
      Feature fe = (Feature)m_features.get( i );
      switch( mySelectionMode )
      {
      case JMSelector.MODE_SELECT:
        fe.select( mySelectionId );
        break;
      case JMSelector.MODE_UNSELECT:
        fe.unselect( mySelectionId );
        break;
      case JMSelector.MODE_TOGGLE:
        fe.toggle( mySelectionId );
        break;
      default:
        break;
      }
    }

    m_workspace.fireModellEvent( null );
  }

  public void undo() throws Exception
  {
    for( int i = 0; i < m_features.size(); i++ )
    {
      Feature fe = (Feature)m_features.get( i );
      switch( mySelectionMode )
      {
      case JMSelector.MODE_SELECT:
        fe.unselect( mySelectionId );
        break;
      case JMSelector.MODE_UNSELECT:
        fe.select( mySelectionId );
        break;
      case JMSelector.MODE_TOGGLE:
        fe.toggle( mySelectionId );
        break;
      default:
        break;
      }
    }

    m_workspace.fireModellEvent( null );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "selectiert features";
  }
}