package org.kalypso.ogc.gml.command;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.deegree.model.feature.Feature;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.util.command.ICommand;

/**
 * @author sbad0205
 */
public class UnselectAllCommand implements ICommand
{
  private final KalypsoFeatureLayer[] m_layers;
  private final int m_selectionId;
  private final List m_touchedFeature=new ArrayList();

  public UnselectAllCommand(KalypsoFeatureLayer[] layers,int selectionId)
  {
    m_layers=layers;
    m_selectionId=selectionId;
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
    for(int i=0;i<m_layers.length;i++)
    {
      KalypsoFeatureLayer layer=m_layers[i];
      Feature[] fes = layer.getAllFeatures();
      for( int j = 0; j < fes.length; j++ )
      {
        Feature feature = fes[j];
        if(feature.unselect(m_selectionId))
          m_touchedFeature.add(feature);
      }
    }
    fireEvents();
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    Iterator i=m_touchedFeature.iterator();
    while( i.hasNext() )
     ((Feature)i.next()).unselect(m_selectionId);       

    fireEvents();
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    Iterator i=m_touchedFeature.iterator();
    while( i.hasNext() )
     ((Feature)i.next()).select(m_selectionId);       
   
    fireEvents();
  }
  
  private void fireEvents()
  {
    for(int i=0;i<m_layers.length;i++)
    {
      m_layers[i].fireModellEvent(null);
    }
  }
  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return null;
  }
}
