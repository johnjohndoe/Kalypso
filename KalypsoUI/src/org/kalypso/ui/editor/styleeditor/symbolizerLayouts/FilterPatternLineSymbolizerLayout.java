/*
 * Created on 26.07.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.symbolizerLayouts;

import org.deegree.graphics.sld.LineSymbolizer;
import org.deegree.graphics.sld.Stroke;
import org.deegree.graphics.sld.Symbolizer;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.services.wfs.filterencoding.FilterEvaluationException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ui.editor.styleeditor.MessageBundle;
import org.kalypso.ui.editor.styleeditor.panels.ColorPalettePanel;
import org.kalypso.ui.editor.styleeditor.panels.PanelEvent;
import org.kalypso.ui.editor.styleeditor.panels.PanelListener;
import org.kalypso.ui.editor.styleeditor.panels.SliderPanel;
import org.kalypso.ui.editor.styleeditor.rulePattern.RuleCollection;

/**
 * @author F.Lindemann
 *  
 */

public class FilterPatternLineSymbolizerLayout extends AbstractSymbolizerLayout
{

  private int selectionIndex = 0;

  private RuleCollection ruleCollection = null;

  private int symbolizerIndex = -1;

  ColorPalettePanel colorPalettePanel = null;

  public FilterPatternLineSymbolizerLayout( Composite m_composite, Symbolizer m_symbolizer,
      KalypsoUserStyle m_userStyle, RuleCollection m_ruleCollection, int m_symbolizerIndex )
  {
    super( m_composite, m_symbolizer, m_userStyle );
    this.ruleCollection = m_ruleCollection;
    this.symbolizerIndex = m_symbolizerIndex;
  }

  public void draw() throws FilterEvaluationException
  {
    GridLayout compositeLayout = new GridLayout();
    compositeLayout.marginHeight = 2;
    // ***** group
    Group group = new Group( composite, SWT.NULL );
    GridData groupData = new GridData();
    groupData.widthHint = 210;
    groupData.heightHint = 215;
    group.setLayoutData( groupData );
    group.setLayout( compositeLayout );
    group.layout();

    final LineSymbolizer lineSymbolizer = (LineSymbolizer)symbolizer;
    final Stroke stroke = lineSymbolizer.getStroke();

    SliderPanel strokeWidthPanel = new SliderPanel( group, MessageBundle.STYLE_EDITOR_WIDTH, 0, 10,
        1, SliderPanel.INTEGER, stroke.getWidth( null ) );
    for( int i = 0; i < getRuleCollection().size(); i++ )
    {
      Symbolizer symb = getRuleCollection().get( i ).getSymbolizers()[getSymbolizerIndex()];
      if( symb instanceof LineSymbolizer )
      {
        ( (LineSymbolizer)symb ).getStroke().setWidth( stroke.getWidth( null ) );
      }
    }
    strokeWidthPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        double width = ( (SliderPanel)event.getSource() ).getSelection();
        for( int i = 0; i < getRuleCollection().size(); i++ )
        {
          Symbolizer symb = getRuleCollection().get( i ).getSymbolizers()[getSymbolizerIndex()];
          if( symb instanceof LineSymbolizer )
          {
            ( (LineSymbolizer)symb ).getStroke().setWidth( width );
          }
        }
        userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
      }
    } );

    // get all colors for each rule of the pattern for this specific symbolizer
    Color[] colors = new Color[getRuleCollection().size()];
    for( int i = 0; i < getRuleCollection().size(); i++ )
    {
      Symbolizer symb = getRuleCollection().get( i ).getSymbolizers()[getSymbolizerIndex()];
      if( symb instanceof LineSymbolizer )
      {
        java.awt.Color color = ( (LineSymbolizer)symb ).getStroke().getStroke( null );
        colors[i] = new Color( null, color.getRed(), color.getGreen(), color.getBlue() );
      }
    }

    if( colorPalettePanel == null )
    {
      colorPalettePanel = new ColorPalettePanel( group, colors, getRuleCollection() );
      colorPalettePanel.setType( ColorPalettePanel.CUSTOM_TRANSITION );
      // init colors of LineSymbolizer
      for( int i = 0; i < getRuleCollection().size(); i++ )
      {
        Symbolizer symb = getRuleCollection().get( i ).getSymbolizers()[getSymbolizerIndex()];
        if( symb instanceof LineSymbolizer )
        {
          ( (LineSymbolizer)symb ).getStroke().setStroke(
              new java.awt.Color( colors[i].getRed(), colors[i].getGreen(), colors[i].getBlue() ) );
        }
      }

      colorPalettePanel.addColorPalettePanelListener( new PanelListener()
      {
        public void valueChanged( PanelEvent event )
        {
          Color[] colorArray = colorPalettePanel.getColorPalette();

          for( int i = 0; i < getRuleCollection().size(); i++ )
          {
            Symbolizer symb = getRuleCollection().get( i ).getSymbolizers()[getSymbolizerIndex()];
            if( symb instanceof LineSymbolizer )
            {
              ( (LineSymbolizer)symb ).getStroke().setStroke(
                  new java.awt.Color( colorArray[i].getRed(), colorArray[i].getGreen(),
                      colorArray[i].getBlue() ) );
            }
          }
          userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
        }
      } );
    }
    else
      colorPalettePanel.draw( composite );
  }

  public int getSelectionIndex()
  {
    return selectionIndex;
  }

  public void setSelectionIndex( int m_selectionIndex )
  {
    this.selectionIndex = m_selectionIndex;
  }

  public int getSymbolizerIndex()
  {
    return symbolizerIndex;
  }

  public void setSymbolizerIndex( int m_symbolizerIndex )
  {
    this.symbolizerIndex = m_symbolizerIndex;
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