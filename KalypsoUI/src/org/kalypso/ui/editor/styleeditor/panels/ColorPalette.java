/*
 * Created on 15.07.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.panels;

import javax.swing.event.EventListenerList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

/**
 * @author F.Lindemann
 *  
 */
public class ColorPalette
{

  private Composite composite = null;

  private EventListenerList listenerList = new EventListenerList();

  private int colorSize = 0;

  private int borderWidth = 0;

  private final int MAX_WIDTH = 5;

  private Color[] colors = null;

  private ColorBox[] colorBoxes = null;

  public ColorPalette( Composite parent, Color[] m_colors, int m_colorSize, int m_borderWidth )
  {
    this.colors = m_colors;
    composite = new Composite( parent, SWT.NULL );
    GridLayout compositeLayout = new GridLayout( MAX_WIDTH, true );
    GridData compositeData = new GridData();
    composite.setLayoutData( compositeData );
    composite.setLayout( compositeLayout );
    compositeLayout.marginWidth = 0;
    compositeLayout.marginHeight = 0;
    compositeLayout.horizontalSpacing = 0;
    compositeLayout.verticalSpacing = 0;
    composite.layout();
    this.colorSize = m_colorSize;
    this.borderWidth = m_borderWidth;
    init();
  }

  public void addColorPaletterListener( PanelListener pl )
  {
    listenerList.add( PanelListener.class, pl );
  }

  int i = 0;

  private void init()
  {
    colorBoxes = new ColorBox[colors.length];
    for( i = 0; i < colors.length; i++ )
    {
      final ColorBox box = new ColorBox( composite, colors[i], colorSize, borderWidth );
      colorBoxes[i] = box;
      box.addPanelListener( new PanelListener()
      {
        public void valueChanged( PanelEvent event )
        {
          for( int j = 0; j < getColorBoxes().length; j++ )
          {
            getColors()[j] = getColorBoxes()[j].getColor();
          }
          fire();
        }
      } );
    }
  }

  public void setColors( Color[] m_colors )
  {
    this.colors = m_colors;
  }

  public Color[] getColors()
  {
    return colors;
  }

  protected void fire()
  {
    Object[] listeners = listenerList.getListenerList();
    for( int counter = listeners.length - 2; counter >= 0; counter -= 2 )
    {
      if( listeners[counter] == PanelListener.class )
      {
        PanelEvent event = new PanelEvent( this );
        ( (PanelListener)listeners[counter + 1] ).valueChanged( event );
      }
    }
  }

  public ColorBox[] getColorBoxes()
  {
    return colorBoxes;
  }

  public void setColorBoxes( ColorBox[] m_colorBoxes )
  {
    this.colorBoxes = m_colorBoxes;
  }
}