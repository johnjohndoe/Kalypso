/** TODO: license definieren
*/

package org.kalypso.editor.mapeditor.commands;

import org.deegree.model.geometry.GM_Envelope;
import org.eclipse.jface.resource.ImageDescriptor;
import org.kalypso.eclipse.jface.action.FullAction;
import org.kalypso.ogc.MapPanel;
import org.kalypso.ogc.widgets.ChangeExtentCommand;
import org.kalypso.util.command.ICommandManager;

/**
 *
 * @author von Dömming
 */
public class FullExtentAction extends FullAction
{
  private final ICommandManager myCommandManager;
  private final MapPanel myMapPanel;
  
    public FullExtentAction(final String text,final ImageDescriptor imageDescriptor,final String toolTipText, final MapPanel mapPanel, final ICommandManager commandManager  )
    {
      super(text,imageDescriptor,toolTipText);
      myCommandManager=commandManager;
      myMapPanel=mapPanel;
    }
    
    public void run()
    {
      GM_Envelope fullExtent = myMapPanel.getMapModell().getFullExtentBoundingBox();
      myCommandManager.postCommand( new ChangeExtentCommand(myMapPanel.getMapModell(),fullExtent));
    }  
}
