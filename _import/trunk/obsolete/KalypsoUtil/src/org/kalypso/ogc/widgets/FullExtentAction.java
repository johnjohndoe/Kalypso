/** TODO: license definieren
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
public class FullExtentAction extends GisMapEditorActionDelegate
{  
 
    /**
     * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
     */
    public void run( IAction action )
    {
      if(myEditor==null)
        return;
      final MapModell modell=myEditor. getMapModell();
      if(modell==null)
        return;
      GM_Envelope fullExtent = modell.getFullExtentBoundingBox();
     try
    {
      ((ICommandManager)myEditor).postCommand( new ChangeExtentCommand(modell,fullExtent),null);
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }     
    }
}
