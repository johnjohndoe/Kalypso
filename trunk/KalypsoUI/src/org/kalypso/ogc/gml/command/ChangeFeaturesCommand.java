package org.kalypso.ogc.gml.command;

import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventProvider;
import org.deegree_impl.model.feature.FeatureFactory;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.util.command.ICommand;

/**
 * @author belger
 */
public class ChangeFeaturesCommand implements ICommand
{
  private final FeatureChange[] m_newChanges;
  private final FeatureChange[] m_oldChanges;
  private final ModellEventProvider m_eventprovider;

  public ChangeFeaturesCommand( final ModellEventProvider eventprovider, final FeatureChange[] changes )
  {
    m_eventprovider = eventprovider;
    m_newChanges = changes;
    m_oldChanges = new FeatureChange[changes.length];
    for( int i = 0; i < changes.length; i++ )
    {
      final FeatureChange change = changes[i];
      
      final Object oldValue = change.feature.getProperty( change.property );
      m_oldChanges[i] = new FeatureChange( change.feature, change.property, oldValue );
    }
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
    applyChanges( m_newChanges );
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    applyChanges( m_newChanges );
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    applyChanges( m_oldChanges );  
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Feature verändern";
  }
  
  private void applyChanges( final FeatureChange[] changes )
  {
    for( int i = 0; i < changes.length; i++ )
    {
      final FeatureChange change = changes[i];
      final FeatureProperty fp = FeatureFactory.createFeatureProperty( change.property, change.newValue );
      change.feature.setProperty( fp );
    }
    
    if( m_eventprovider != null )
      m_eventprovider.fireModellEvent( new ModellEvent( m_eventprovider, ModellEvent.FEATURE_CHANGE ) );
  }
}
