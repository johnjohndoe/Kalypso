/**
 * TODO: license definieren
 */

package org.kalypso.ogc.widgets;

import org.deegree.model.geometry.GM_Envelope;
import org.kalypso.ogc.MapPanel;
import org.kalypso.util.command.ICommand;

/**
 * 
 * @author Belger
 */
public class ChangeExtentCommand implements ICommand
{
  private final GM_Envelope myDoBoundingBox;

  private final GM_Envelope myUndoBoundingBox;

  private final MapPanel m_mapPanel;

  public ChangeExtentCommand( final MapPanel mapPanel, final GM_Envelope boundingBox )
  {
    m_mapPanel = mapPanel;
    myUndoBoundingBox = mapPanel.getBoundingBox();
    myDoBoundingBox = boundingBox;
  }

  public boolean isUndoable()
  {
    return true;
  }

  public void process() throws Exception
  {
    m_mapPanel.setBoundingBox( myDoBoundingBox );
  }

  public void redo() throws Exception
  {
    process();
  }

  public void undo() throws Exception
  {
    m_mapPanel.setBoundingBox( myUndoBoundingBox );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Ausschnitt ?ndern";
  }
}