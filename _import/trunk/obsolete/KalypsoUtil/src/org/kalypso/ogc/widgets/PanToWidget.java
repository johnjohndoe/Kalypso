/** TODO: license definieren
*/

package org.kalypso.ogc.widgets;

import java.awt.Point;

import org.deegree.model.geometry.GM_Envelope;
import org.kalypso.ogc.MapPanel;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandManager;


/**
 * @author vDoemming
 */
public class PanToWidget extends Widget implements TemporaryActionWidget
{
    private Point endPoint = null;
    private Point startPoint = null;

    public PanToWidget( final MapPanel mapPanel, final ICommandManager commandManager )
    {
      super( mapPanel, commandManager );
    }

    public String getName(  )
    {
        return "PanTo";
    }

    public void dragged( Point p )
    {
        if( startPoint != null )
        {
            endPoint = p;

            int dx = (int)( endPoint.getX(  ) - startPoint.getX(  ) );
            int dy = (int)( endPoint.getY(  ) - startPoint.getY(  ) );
            myMapPanel.setOffset( dx, dy );
        }
    }

    public void finish(  )
    {
        myMapPanel.clearOffset(  );
    }

    public void leftPressed( Point p )
    {
        startPoint = p;
        endPoint = null;
        myMapPanel.clearOffset(  );
    }

    public void leftReleased( Point p )
    {
        endPoint = p;
        perform(  );
    }

  
    /**
     * 
     * @see org.kalypso.ogc.widgets.Widget#performIntern()
     */
    public ICommand performIntern(  )
    {
        if( startPoint != null && endPoint != null )
        {
            final double mx = myMapPanel.getWidth(  ) / 2d - ( endPoint.getX(  ) - startPoint.getX(  ) );
            final double my = myMapPanel.getHeight(  ) / 2d - ( endPoint.getY(  ) - startPoint.getY(  ) );

            final GM_Envelope panBox = myMapPanel.getPanToPixelBoundingBox( mx, my );

            startPoint = null;
            endPoint = null;

            if( panBox != null )
                return new ChangeExtentCommand( myMapPanel.getMapModell(), panBox );
        }
        return null;
    }
}