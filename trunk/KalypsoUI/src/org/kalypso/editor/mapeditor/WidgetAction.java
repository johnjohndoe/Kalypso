package org.kalypso.editor.mapeditor;

import java.awt.Graphics;
import java.awt.Point;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.resource.ImageDescriptor;
import org.kalypso.ogc.widgets.IWidget;
import org.kalypso.ogc.widgets.WidgetManager;

/**
 * 
 * 
 * @author bce
 */
public class WidgetAction extends Action implements IWidget, IAction
{
  private final IWidget myWidget;
  private final WidgetManager myWidgetManager;

  public WidgetAction( final IWidget widget, final WidgetManager widgetManager, final String text, final ImageDescriptor image, final String tooltipText )
  {
    super( text, IAction.AS_RADIO_BUTTON );
    
    setToolTipText(tooltipText);
    setImageDescriptor(image);
    
    myWidget = widget;
    myWidgetManager = widgetManager;
 }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#activate()
   */
  public void activate()
  {
    myWidget.activate();

    setChecked( true );
  }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#getName()
   */
  public String getName()
  {
    return myWidget.getName();
  }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#clickPopup(java.awt.Point)
   */
  public void clickPopup( Point p )
  {
    myWidget.clickPopup(p);
    }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#dragged(java.awt.Point)
   */
  public void dragged( Point p )
  {
    myWidget.dragged(p);
    }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#finish()
   */
  public void finish()
  {
    myWidget.finish();
    }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#leftClicked(java.awt.Point)
   */
  public void leftClicked( Point p )
  {
    myWidget.leftClicked(p);
    }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#leftPressed(java.awt.Point)
   */
  public void leftPressed( Point p )
  {
    myWidget.leftPressed(p);
    }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#leftReleased(java.awt.Point)
   */
  public void leftReleased( Point p )
  {
    myWidget.leftReleased(p);
    }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#middleClicked(java.awt.Point)
   */
  public void middleClicked( Point p )
  {
    myWidget.middleClicked(p);  
  }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#middlePressed(java.awt.Point)
   */
  public void middlePressed( Point p )
  {
    myWidget.middlePressed(p);  
  }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#middleReleased(java.awt.Point)
   */
  public void middleReleased( Point p )
  {
    myWidget.middleReleased(p);
    }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#moved(java.awt.Point)
   */
  public void moved( Point p )
  {
    myWidget.moved(p);  
  }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#paint(java.awt.Graphics)
   */
  public void paint( Graphics g )
  {
    myWidget.paint(g);  
  }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#perform()
   */
  public void perform()
  {
  myWidget.perform();  
  }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#rightClicked(java.awt.Point)
   */
  public void rightClicked( Point p )
  {
    myWidget.rightClicked(p);
    }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#rightPressed(java.awt.Point)
   */
  public void rightPressed( Point p )
  {
    myWidget.rightPressed(p);  
  }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#rightReleased(java.awt.Point)
   */
  public void rightReleased( Point p )
  {
    myWidget.rightReleased(p);  
  }
  
  public void run()
  {
    myWidgetManager.changeWidget( this );
  }
}
