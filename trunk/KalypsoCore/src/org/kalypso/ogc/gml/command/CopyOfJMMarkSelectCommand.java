/** TODO: license definieren
*/

package org.kalypso.ogc.gml.command;

import java.util.List;

import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Position;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.util.command.ICommand;


/**
 * DOCUMENT ME!
 *
 * @author doemming
 */
public class CopyOfJMMarkSelectCommand implements ICommand
{
    private GM_Envelope mySelectEnv = null;
    private GM_Position mySelectPos = null;
    private List myListFe = null; // list of display elements
    private IKalypsoTheme myTheme = null;
    private boolean mySelectWithinStatus = true;
    private final double myRadius;
    private int mySelectionMode = -1;
    private int mySelectionId;
  
    public CopyOfJMMarkSelectCommand( final IKalypsoTheme theme, GM_Envelope selectEnv, boolean selectWithinStatus,double gisSelectionRadius, int selectionId ,int selectionMode)
    { 
        mySelectEnv = selectEnv;
        myRadius = gisSelectionRadius;
        mySelectWithinStatus = selectWithinStatus;
        init(theme,selectionId,selectionMode);
    }

    public CopyOfJMMarkSelectCommand( final IKalypsoTheme theme, GM_Position selectPos,double gisSelectionRadius,int selectionId, int selectionMode )
    {
        mySelectPos = selectPos;
        myRadius = gisSelectionRadius;
        init(theme,selectionId,selectionMode  );
    }

    private void init( final IKalypsoTheme theme,int selectionId,int selectionMode  )
    {
      // TODO: warum ist das kein Konstruktor?
        mySelectionMode=selectionMode;
        myTheme = theme;
        mySelectionId=selectionId;
    }
    
    public boolean isUndoable(  )
    {
        return true;
    }

    public void process(  ) throws Exception
    {
        JMSelector selector = new JMSelector(mySelectionMode);
    
        if( mySelectEnv != null )
            myListFe = selector.select( mySelectEnv, myTheme, mySelectWithinStatus,mySelectionId );
        else if( mySelectPos != null && myRadius >= 0d )
            myListFe = selector.select( mySelectPos, myRadius, myTheme, false,mySelectionId );
        else 
            myListFe = selector.select( mySelectPos, myTheme,mySelectionId );
   
        myTheme.getLayer().fireModellEvent(null);        
    }

    public void redo(  ) throws Exception
    {
      JMSelector selector = new JMSelector(mySelectionMode);
      selector.setSelectionMode( mySelectionMode );
      selector.perform( myListFe,mySelectionId );
      myTheme.getLayer().fireModellEvent(null);        
        }

    public void undo(  ) throws Exception
    {

            JMSelector selector = new JMSelector(mySelectionMode);
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
