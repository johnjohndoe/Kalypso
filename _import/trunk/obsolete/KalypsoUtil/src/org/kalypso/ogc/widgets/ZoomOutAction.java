/**
 * TODO: license definieren
 */

package org.kalypso.ogc.widgets;

import org.deegree.model.geometry.GM_Envelope;
import org.eclipse.jface.action.IAction;
import org.kalypso.ogc.MapModell;
import org.kalypso.util.command.ICommandManager;

/**
 * 
 * @author von Dömming
 */
public class ZoomOutAction extends GisMapEditorActionDelegate
{
  public void run( IAction action )
  {
    if( myEditor == null )
      return;
    final MapModell modell = myEditor.getMapModell();
    if( modell == null )
      return;
    GM_Envelope zoomBox = modell.getZoomOutBoundingBox();

    try
    {
      ( (ICommandManager)myEditor ).postCommand( new ChangeExtentCommand( modell, zoomBox ), null );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }
}