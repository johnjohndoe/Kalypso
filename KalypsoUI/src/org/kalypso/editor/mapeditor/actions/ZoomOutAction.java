/**
 * TODO: license definieren
 */

package org.kalypso.editor.mapeditor.actions;

import org.deegree.model.geometry.GM_Envelope;
import org.eclipse.jface.action.IAction;
import org.kalypso.ogc.MapModell;
import org.kalypso.ogc.widgets.ChangeExtentCommand;

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

    myEditor.postCommand( new ChangeExtentCommand( modell, zoomBox ), null );
  }
}