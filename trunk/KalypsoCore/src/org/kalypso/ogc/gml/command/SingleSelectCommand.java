/**
 * TODO: license definieren
 */

package org.kalypso.ogc.gml.command;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.event.ModellEventProvider;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.util.command.ICommand;

/**
 * DOCUMENT ME!
 * 
 * @author doemming
 */
public class SingleSelectCommand implements ICommand
{

  private final Feature m_feature;

  private final int mySelectionId;

  private final ModellEventProvider m_modellEventProvider;

  private final ICommand m_unselectAllCommand;

  public SingleSelectCommand( final Feature feature, int selectionId,
      ModellEventProvider eventProvider, KalypsoFeatureLayer[] layers )
  {
    m_feature = feature;
    mySelectionId = selectionId;

    m_modellEventProvider = eventProvider;
    m_unselectAllCommand = new UnselectAllCommand( layers, selectionId );
  }

  public boolean isUndoable()
  {
    return true;
  }

  public void process() throws Exception
  {
    m_unselectAllCommand.process();
    redo();
  }

  public void redo() throws Exception
  {
    m_unselectAllCommand.redo();

    m_feature.select( mySelectionId );

    m_modellEventProvider.fireModellEvent( null );
  }

  public void undo() throws Exception
  {

    m_feature.unselect( mySelectionId );

    m_modellEventProvider.fireModellEvent( null );
    
    m_unselectAllCommand.undo();
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "selectiert features";
  }
}