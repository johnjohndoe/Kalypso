/**
 * TODO: license definieren
 */

package org.kalypso.ogc.gml.command;

import java.util.List;

import org.deegree.model.feature.Feature;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.util.command.ICommand;

/**
 * DOCUMENT ME!
 * 
 * @author doemming
 */
public class JMMarkSelectCommand implements ICommand
{

  private final List myListFe[];

  private final int mySelectionId;

  private final int mySelectionMode;

  private final KalypsoFeatureLayer[] m_layers;

  public JMMarkSelectCommand( final List[] featureLists, int selectionId, int selectionModus,
      KalypsoFeatureLayer[] layers )
  {
    myListFe = featureLists;
    mySelectionId = selectionId;
    mySelectionMode = selectionModus;
    m_layers = layers;
  }

  public boolean isUndoable()
  {
    return true;
  }

  public void process() throws Exception
  {
    redo();
  }

  public void redo() throws Exception
  {
    for( int n = 0; n < myListFe.length; n++ )
    {
      List listFe = myListFe[n];
      for( int i = 0; i < listFe.size(); i++ )
      {
        Feature fe = (Feature)listFe.get( i );
        switch( mySelectionMode )
        {
        case JMSelector.MODE_SELECT:
          fe.select( mySelectionId );
          break;
        case JMSelector.MODE_UNSELECT:
          fe.unselect( mySelectionId );
          break;
        case JMSelector.MODE_TOGGLE:
          fe.toggle( mySelectionId );
          break;
        default:
          break;
        }
      }
      m_layers[n].fireModellEvent( null );
    }
  }

  public void undo() throws Exception
  {
    for( int n = 0; n < myListFe.length; n++ )
    {
      List listFe = myListFe[n];
      for( int i = 0; i < listFe.size(); i++ )
      {
        Feature fe = (Feature)listFe.get( i );
        switch( mySelectionMode )
        {
        case JMSelector.MODE_SELECT:
          fe.unselect( mySelectionId );
          break;
        case JMSelector.MODE_UNSELECT:
          fe.select( mySelectionId );
          break;
        case JMSelector.MODE_TOGGLE:
          fe.toggle( mySelectionId );
          break;
        default:
          break;
        }
      }
      m_layers[n].fireModellEvent( null );
    }
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "selectiert features";
  }
}