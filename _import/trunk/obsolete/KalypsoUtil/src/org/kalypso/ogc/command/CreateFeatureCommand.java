package org.kalypso.ogc.command;

import org.deegree.model.feature.FeatureProperty;
import org.deegree_impl.model.feature.FeatureFactory;
import org.kalypso.ogc.gml.KalypsoFeature;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.util.command.ICommand;

/**
 * @author belger
 */
public class CreateFeatureCommand implements ICommand
{
  private final KalypsoFeatureLayer m_layer;
  private final KalypsoFeature[] m_features;
  
  public CreateFeatureCommand( final KalypsoFeatureLayer layer, final KalypsoFeature[] features )
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
