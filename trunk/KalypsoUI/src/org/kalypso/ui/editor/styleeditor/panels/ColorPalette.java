/*
 * Created on 15.07.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.panels;

import javax.swing.event.EventListenerList;

import org.deegree_impl.services.wfs.filterencoding.ComplexFilter;
import org.deegree_impl.services.wfs.filterencoding.PropertyIsBetweenOperation;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.ui.editor.styleeditor.dialogs.filterpatterndialog.FilterPatternDialog;
import org.kalypso.ui.editor.styleeditor.rulePattern.RuleCollection;

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
  
  private RuleCollection ruleCollection = null;

  public ColorPalette( Composite parent, Color[] m_colors, int m_colorSize, int m_borderWidth, RuleCollection m_ruleCollection)
  {
    this.colors = m_colors;
    setRuleCollection(m_ruleCollection);
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
       
      final FilterPatternDialog  filterPatternDialog = new FilterPatternDialog( composite.getShell(), ((ComplexFilter)getRuleCollection().get(i).getFilter()).getOperation());
      box.addPanelListener( new PanelListener()
      {
        public void valueChanged( PanelEvent event )
        {          
          filterPatternDialog.setColor(((ColorBox)event.getSource()).getColor());
          filterPatternDialog.open();
          if(filterPatternDialog.getReturnCode()  ==  Dialog.OK)
          {
            ((ColorBox)event.getSource()).setColor(filterPatternDialog.getColor());            
          }
          for( int j = 0; j < getColorBoxes().length; j++ )
          {
            getColors()[j] = getColorBoxes()[j].getColor();
          }
          fire();
        }
      } );
    }
  }

  private void update()
  {
    for( int j = 0; j < colorBoxes.length; j++ )
    {
      colorBoxes[j].setColor( colors[j] );
    }
  }

  public void setColors( Color[] m_colors )
  {
    this.colors = m_colors;
    update();
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
  public RuleCollection getRuleCollection()
  {
    return ruleCollection;
  }
  public void setRuleCollection( RuleCollection m_ruleCollection )
  {
    this.ruleCollection = m_ruleCollection;
  }
}