package org.kalypso.ogc.command;

import org.deegree.model.feature.FeatureProperty;
import org.deegree_impl.model.feature.FeatureFactory;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.sort.DisplayContext;
import org.kalypso.util.command.ICommand;

/**
 * @author belger
 */
public class ModifyFeatureCommand implements ICommand
{
  private final KalypsoFeatureLayer m_layer;
  private final DisplayContext m_displayContext;
  private final String m_name;
  private final Object m_newValue;
  private Object m_oldValue;

  public ModifyFeatureCommand( final KalypsoFeatureLayer layer, final DisplayContext dc, final String name, final Object value )
  {
    m_layer = layer;
    m_displayContext = dc;
    m_name = name;
    m_newValue = value;
    
    m_oldValue = dc.getFeature().getProperty( name );
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
    setFeatureProperty( m_newValue );
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    setFeatureProperty( m_newValue );  
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    setFeatureProperty( m_oldValue );  
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Wert ?ndern";
  }

  private void setFeatureProperty( final Object value )
  {
    final FeatureProperty fp = FeatureFactory.createFeatureProperty( m_name, value );
    m_displayContext.getFeature().setProperty( fp );
    m_layer.fireModellEvent( null );
  }
}
