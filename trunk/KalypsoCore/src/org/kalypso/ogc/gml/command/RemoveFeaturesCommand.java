package org.kalypso.ogc.gml.command;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.GMLWorkspace;
import org.kalypso.util.command.ICommand;

/**
 * @author bce
 */
public class RemoveFeaturesCommand implements ICommand
{
  private final Feature[] m_features;
  private final GMLWorkspace m_workspace;

  public RemoveFeaturesCommand( final GMLWorkspace workspace, final Feature[] features )
  {
    m_workspace = workspace;
    m_features = features;
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
//    m_layer.removeFeatures( m_features );
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
    // TODO: an der alten Position wieder einf?gen?
//    m_layer.addFeatures( m_features );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Element löschen";
  }
}