/**
 * TODO: license definieren
 */

package org.kalypso.ogc.widgets;

import org.deegree.graphics.MapView;
import org.deegree.model.geometry.GM_Envelope;
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

  private final MapView myMapView;

  public ChangeExtentCommand( MapView mapView, GM_Envelope boundingBox )
  {
    myMapView = mapView;
    myUndoBoundingBox = mapView.getBoundingBox();
    myDoBoundingBox = boundingBox;
  }

  public boolean isUndoable()
  {
    return true;
  }

  public void process() throws Exception
  {
    myMapView.setBoundingBox( myDoBoundingBox );
  }

  public void redo() throws Exception
  {
    process();
  }

  public void undo() throws Exception
  {
    myMapView.setBoundingBox( myUndoBoundingBox );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Ausschnitt ändern";
  }
}