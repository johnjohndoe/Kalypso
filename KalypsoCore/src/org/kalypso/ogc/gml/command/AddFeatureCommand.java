package org.kalypso.ogc.gml.command;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.GMLWorkspace;
import org.kalypso.util.command.ICommand;

/**
 * @author Belger
 */
public class AddFeatureCommand implements ICommand
{
  private final org.deegree.model.feature.Feature m_feature;
  private final GMLWorkspace m_workspace;

  public AddFeatureCommand( final GMLWorkspace workspace, final Feature fe )
  {
    m_workspace = workspace;
    m_feature = fe;
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
//    m_layer.addFeature( m_feature );
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
//    m_layer.addFeature( m_feature );  
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
//    m_layer.removeFeature(m_feature);
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Element hinzufügen";
  }
}
