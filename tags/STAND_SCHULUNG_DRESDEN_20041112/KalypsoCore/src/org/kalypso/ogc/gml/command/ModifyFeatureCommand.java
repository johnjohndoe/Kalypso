package org.kalypso.ogc.gml.command;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.model.feature.FeatureFactory;
import org.kalypso.util.command.ICommand;

/**
 * @author belger
 */
public class ModifyFeatureCommand implements ICommand
{
  private final Feature m_feature;
  private final Map m_newMap;
  private final Map m_oldMap = new HashMap();
  private final GMLWorkspace m_workspace;
  

  public ModifyFeatureCommand( final GMLWorkspace workspace, final Feature feature, final Map map )
  {
    m_workspace = workspace;
    m_feature = feature;
    m_newMap = map;
    
    for( Iterator iter = map.keySet().iterator(); iter.hasNext(); )
    {
      final String propName = (String)iter.next();
      m_oldMap.put( propName, feature.getProperty(propName) );
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
    setFeatureProperty( m_newMap );
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    setFeatureProperty( m_newMap );  
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    setFeatureProperty( m_oldMap );  
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Wert ?ndern";
  }

  private void setFeatureProperty( final Map map )
  {
    for( Iterator iter = map.entrySet().iterator(); iter.hasNext(); )
    {
      final Map.Entry entry = (Entry)iter.next();
      
      final FeatureProperty property = FeatureFactory.createFeatureProperty( (String)entry.getKey(), entry.getValue() );
      m_feature.setProperty( property );
    }

    m_workspace.fireModellEvent( null );
  }
}
