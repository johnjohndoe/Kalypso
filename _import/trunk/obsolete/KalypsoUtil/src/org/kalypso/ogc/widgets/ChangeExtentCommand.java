/**
 * TODO: license definieren
 */

package org.kalypso.ogc.widgets;

import org.deegree.model.geometry.GM_Envelope;
import org.kalypso.ogc.MapModell;
import org.kalypso.util.command.ICommand;

/**
 * DOCUMENT ME!
 * 
 * @author $author$
 */
public class ChangeExtentCommand implements ICommand
{
  private final GM_Envelope myDoBoundingBox;

  private final GM_Envelope myUndoBoundingBox;

  private final MapModell myMapModell;

  public ChangeExtentCommand( MapModell mapModell, GM_Envelope boundingBox )
  {
    myMapModell = mapModell;
    myUndoBoundingBox = mapModell.getBoundingBox();
    myDoBoundingBox = boundingBox;
  }

  public boolean isUndoable()
  {
    return true;
  }

  public void process() throws Exception
  {
    myMapModell.setBoundingBox( myDoBoundingBox );
  }

  public void redo() throws Exception
  {
    process();
  }

  public void undo() throws Exception
  {
    myMapModell.setBoundingBox( myUndoBoundingBox );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Ausschnitt ?ndern";
  }
}