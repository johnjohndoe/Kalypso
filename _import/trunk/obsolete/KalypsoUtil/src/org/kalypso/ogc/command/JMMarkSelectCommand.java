/** TODO: license definieren
*/

package org.kalypso.ogc.command;

import java.util.List;

import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Position;
import org.kalypso.ogc.MapModell;
import org.kalypso.ogc.gml.KalypsoTheme;
import org.kalypso.util.command.ICommand;


/**
 * DOCUMENT ME!
 *
 * @author doemming
 */
public class JMMarkSelectCommand implements ICommand
{
    private GM_Envelope mySelectEnv = null;
    private GM_Position mySelectPos = null;
    private List myListFe = null; // list of display elements
    private KalypsoTheme myTheme = null;
    private boolean selectWithinStatus = false;
    private double myRadius = 0d;
    private int mySelectionMode = -1;
    private int mySelectionId;
  
    public JMMarkSelectCommand(KalypsoTheme theme, GM_Envelope selectEnv, boolean selectWithinStatus, int selectionId )
    { 
        this.mySelectEnv = selectEnv;
        this.selectWithinStatus = selectWithinStatus;
        init(theme,selectionId);
    }

    public JMMarkSelectCommand(KalypsoTheme theme, GM_Position selectPos,int selectionId )
    {
        this.mySelectPos = selectPos;
        this.myRadius = 20;//JMSelectOptionPanel.getInstance(  ).getRadius(  );
        init(theme,selectionId  );
    }

    private void init(KalypsoTheme theme,int selectionId  )
    {
        mySelectionMode=JMSelector.MODE_SELECT;
        myTheme =theme;
        mySelectionId=selectionId;//  this.selectionMode = JMSelectOptionPanel.getInstance(  ).getSelectionMode(  );
    }
    
    public boolean isUndoable(  )
    {
        return true;
    }

    public void process(  ) throws Exception
    {
        JMSelector selector = new JMSelector();
        //selector.setSelectionMode( selectionMode );

        if( mySelectEnv != null )
            myListFe = selector.select( mySelectEnv, myTheme, selectWithinStatus,mySelectionId );
        else if( mySelectPos != null && myRadius >= 0d )
            myListFe = selector.select( mySelectPos, myRadius, myTheme, false,mySelectionId );
        else 
            myListFe = selector.select( mySelectPos, myTheme,mySelectionId );
   
        if( myListFe.isEmpty(  ) )
            throw new Exception( "NOP cammand" );
        myTheme.getLayer().fireModellEvent(null);        
    }

    public void redo(  ) throws Exception
    {
      JMSelector selector = new JMSelector();
      selector.setSelectionMode( mySelectionMode );
      selector.perform( myListFe,mySelectionId );
      myTheme.getLayer().fireModellEvent(null);        
        }

    public void undo(  ) throws Exception
    {

            JMSelector selector = new JMSelector();
            switch(mySelectionMode)
            {
              case JMSelector.MODE_SELECT:
                selector.setSelectionMode( JMSelector.MODE_UNSELECT );
              break;
              case JMSelector.MODE_UNSELECT:
                selector.setSelectionMode( JMSelector.MODE_SELECT );
              break;
              default:
              selector.setSelectionMode( mySelectionMode );
              break;
            }
              selector.perform( myListFe, mySelectionId );
            
      myTheme.getLayer().fireModellEvent(null);        
      
    }


    /**
     * @see org.kalypso.util.command.ICommand#getDescription()
     */
    public String getDescription()
    {
      return "selectiert features";
    }
}
