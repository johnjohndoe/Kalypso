package org.kalypso.ogc.widgets;

import java.awt.Graphics;
import java.awt.Point;

import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.IWorkbenchPart;
import org.kalypso.ogc.IMapPanelProvider;
import org.kalypso.ogc.MapPanel;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandManager;

/**
 * 
 * 
 * @author bce
 */
public abstract class GisMapEditorWidgetActionDelegate implements IEditorActionDelegate, ISelectionListener, IWidget//, IAction
{
  /**
   * @see org.kalypso.ogc.widgets.IWidget#paint(java.awt.Graphics)
   */
  public void paint( Graphics g )
  {
    // widgets overwrite this methode to paint on the map
  }
  //  private final IWidget myWidget;
  private WidgetManager myWidgetManager = null;

  protected IMapPanelProvider myEditor = null;

//  public GisMapEditorWidgetActionDelegate( final IWidget widget, final WidgetManager widgetManager,
//      final String text, final ImageDescriptor image, final String tooltipText )
//  {
//    super( text, IAction.AS_RADIO_BUTTON );
//
//    setToolTipText( tooltipText );
//    setImageDescriptor( image );
//
//    myWidget = widget;
//    myWidgetManager = widgetManager;
//  }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#activate()
   */
  public void activate()
  {
   // myWidget.activate();
   // setChecked( true );
  }


  /**
   * @see org.eclipse.ui.IEditorActionDelegate#setActiveEditor(org.eclipse.jface.action.IAction,
   *      org.eclipse.ui.IEditorPart)
   */
  public void setActiveEditor( IAction action, IEditorPart targetEditor )
  {
   // wenn der editor wechselt:
    if( targetEditor != null )
    {
      myEditor = (IMapPanelProvider)targetEditor;
      MapPanel mapPanel = myEditor.getMapPanel();
      if( mapPanel != null )
      {
        myWidgetManager = mapPanel.getWidgetManager();
        if(action.isChecked() && myWidgetManager.getActualWidget()!=this)
            myWidgetManager.changeWidget(this);
      }
    }
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( IAction action )
  {
    // wenn der Button wechselt    
    System.out.println("WidgetActionDelegate.run(IAction.getID="+action.getId()+" IAction.isChecked="+action.isChecked()+" IAction.isEnabled="+action.isEnabled());
    if(action.isChecked() && myWidgetManager.getActualWidget()!=this)
      myWidgetManager.changeWidget(this);  
  }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#perform()
   */
  public void perform()
  {
    final ICommand command = performIntern();
    if( command != null && myEditor!=null)
      ( (ICommandManager)myEditor ).postCommand( command, null );
    }
  
  protected abstract ICommand performIntern(  );
  
  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
  // TODO handle selectionchanged  
  }

  /**
   * @see org.eclipse.ui.ISelectionListener#selectionChanged(org.eclipse.ui.IWorkbenchPart,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IWorkbenchPart part, ISelection selection )
  {
//  TODO handle selectionchanged
  }
  
  // Helper
  protected final GM_Envelope getDragbox( int mx, int my, int dx )
  {
    if(myEditor==null)
      return null;
    MapPanel mapPanel = myEditor.getMapPanel();
    if(mapPanel==null)
      return null;
    
    final double ratio = getRatio();

      final GeoTransform transform = mapPanel.getMapModell().getProjection(  );
      double gisMX = transform.getSourceX( mx );
      double gisMY = transform.getSourceY( my );

      double gisX1 = transform.getSourceX( mx - dx );
      double gisDX = gisMX - gisX1;

      double gisDY = gisDX * ratio;

      double gisX2 = gisMX + gisDX;
      double gisY1 = gisMY - gisDY;
      double gisY2 = gisMY + gisDY;

      return GeometryFactory.createGM_Envelope( gisX1, gisY1, gisX2, gisY2 );
  }

  protected final double getRatio()
  {
    final MapPanel mapPanel = myEditor.getMapPanel();
    final GM_Envelope boundingBox = mapPanel.getMapModell().getBoundingBox();
    
    final double ratio = boundingBox.getHeight(  ) / boundingBox.getWidth(  );
    return ratio;
  }

  
  /**
   * @see org.kalypso.ogc.widgets.IWidget#clickPopup(java.awt.Point)
   */
  public void clickPopup( Point p )
  {
  // not implemented by default  
  }
  /**
   * @see org.kalypso.ogc.widgets.IWidget#dragged(java.awt.Point)
   */
  public void dragged( Point p )
  {
    // not implemented by default  
  
  }
  /**
   * @see org.kalypso.ogc.widgets.IWidget#finish()
   */
  public void finish()
  {
    // not implemented by default  
  
  }
  /**
   * @see org.kalypso.ogc.widgets.IWidget#leftClicked(java.awt.Point)
   */
  public void leftClicked( Point p )
  {
    // not implemented by default  
  
  }
  /**
   * @see org.kalypso.ogc.widgets.IWidget#leftPressed(java.awt.Point)
   */
  public void leftPressed( Point p )
  {
    
    // not implemented by default  
}
  /**
   * @see org.kalypso.ogc.widgets.IWidget#leftReleased(java.awt.Point)
   */
  public void leftReleased( Point p )
  {
    // not implemented by default  
}
  /**
   * @see org.kalypso.ogc.widgets.IWidget#middleClicked(java.awt.Point)
   */
  public void middleClicked( Point p )
  { 
    // not implemented by default  
}
  /**
   * @see org.kalypso.ogc.widgets.IWidget#middlePressed(java.awt.Point)
   */
  public void middlePressed( Point p )
  {
    // not implemented by default  
}
  /**
   * @see org.kalypso.ogc.widgets.IWidget#middleReleased(java.awt.Point)
   */
  public void middleReleased( Point p )
  {
    // not implemented by default  
}
  /**
   * @see org.kalypso.ogc.widgets.IWidget#moved(java.awt.Point)
   */
  public void moved( Point p )
  {
    // not implemented by default  
}
  /**
   * @see org.kalypso.ogc.widgets.IWidget#rightClicked(java.awt.Point)
   */
  public void rightClicked( Point p )
  {
    // not implemented by default  
}
  /**
   * @see org.kalypso.ogc.widgets.IWidget#rightPressed(java.awt.Point)
   */
  public void rightPressed( Point p )
  {
    // not implemented by default  
}
  /**
   * @see org.kalypso.ogc.widgets.IWidget#rightReleased(java.awt.Point)
   */
  public void rightReleased( Point p )
  {
    // not implemented by default  
}
}