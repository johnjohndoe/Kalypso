package org.kalypso.ogc.gml.command;

import org.deegree.model.feature.Feature;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.util.command.ICommand;

/**
 * @author Belger
 */
public class AddFeatureCommand implements ICommand
{
  private final org.deegree.model.feature.Feature m_feature;
  private final KalypsoFeatureLayer m_layer;

  public AddFeatureCommand( final KalypsoFeatureLayer layer, final Feature fe )
  {
    m_layer = layer;
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
    m_layer.addFeature( m_feature );
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    m_layer.addFeature( m_feature );  
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    m_layer.removeFeature(m_feature);
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Element hinzuf?gen";
  }
}
