package org.kalypso.ogc.gml.command;


import org.deegree.model.feature.Feature;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.util.command.ICommand;

/**
 * @author belger
 */
public class CreateFeatureCommand implements ICommand
{
  private final KalypsoFeatureLayer m_layer;
  private final Feature[] m_features;
  
  public CreateFeatureCommand( final KalypsoFeatureLayer layer, final Feature[] features )
  {
    m_layer = layer;
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
    redo();
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    m_layer.addFeatures(m_features);
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    m_layer.removeFeatures(m_features);
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "create Feature";
  }

  
}
