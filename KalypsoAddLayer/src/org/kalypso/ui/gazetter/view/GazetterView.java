package org.kalypso.ui.gazetter.view;

import java.io.InputStream;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.part.ViewPart;

public class GazetterView extends ViewPart
{

  
  public GazetterView( )
  {
    //do nothing
  }

  @Override
  public void createPartControl( Composite parent )
  {
    final InputStream imageStream = getClass().getResourceAsStream("resources/gazetter.png");
    Display display = getSite().getShell().getDisplay();
    Image image = new Image(display,
    imageStream); 


    final Button imageButton = new Button( parent, SWT.PUSH);
    imageButton.setImage(image);
    
//    final GC gc = new GC(image);
//    gc.setForeground(display.getSystemColor(SWT.COLOR_WHITE));
//    gc.drawText("I've been drawn on",0,0,true);
//    gc.dispose(); 


  }

  @Override
  public void setFocus( )
  {
    // TODO Auto-generated method stub

  }

}
