/*
 * Created on 26.07.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.symbolizerLayouts;

import org.deegree.graphics.sld.Graphic;
import org.deegree.graphics.sld.Mark;
import org.deegree.graphics.sld.PointSymbolizer;
import org.deegree.graphics.sld.Symbolizer;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.services.wfs.filterencoding.FilterEvaluationException;
import org.deegree_impl.graphics.sld.StyleFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ui.editor.styleeditor.panels.ColorPalettePanel;
import org.kalypso.ui.editor.styleeditor.panels.ComboPanel;
import org.kalypso.ui.editor.styleeditor.panels.PanelEvent;
import org.kalypso.ui.editor.styleeditor.panels.PanelListener;
import org.kalypso.ui.editor.styleeditor.panels.SliderPanel;
import org.kalypso.ui.editor.styleeditor.panels.WellKnownNameComboPanel;
import org.kalypso.ui.editor.styleeditor.rulePattern.RuleCollection;

/**
 * @author F.Lindemann
 *  
 */

public class FilterPatternPointSymbolizerLayout extends AbstractSymbolizerLayout
{

  private int selectionIndex = 0;

  private RuleCollection ruleCollection = null;

  private int symbolizerIndex = -1;

  ColorPalettePanel colorPalettePanel = null;

  public FilterPatternPointSymbolizerLayout( Composite m_composite, Symbolizer m_symbolizer,
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

    final PointSymbolizer pointSymbolizer = (PointSymbolizer)symbolizer;
    final Graphic graphic = pointSymbolizer.getGraphic();

    final Object objects[] = graphic.getMarksAndExtGraphics();
    Mark mark = (Mark)objects[0];

    ComboPanel wellKnownNameComboBox = new WellKnownNameComboPanel( group, "Type", mark
        .getWellKnownName() );
    for( int i = 0; i < getRuleCollection().size(); i++ )
    {
      Symbolizer symb = getRuleCollection().get( i ).getSymbolizers()[getSymbolizerIndex()];
      if( symb instanceof PointSymbolizer )
      {
        Object[] obj = ( (PointSymbolizer)symb ).getGraphic().getMarksAndExtGraphics();
        if( obj.length > 0 && obj[0] instanceof Mark )
        {
          ( (Mark)obj[0] ).setWellKnownName( mark.getWellKnownName() );
        }
      }
    }
    wellKnownNameComboBox.addPanelListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        int index = ( (ComboPanel)event.getSource() ).getSelection();
        String wkn = WellKnownNameComboPanel.getWellKnownNameByIndex( index );
        for( int i = 0; i < getRuleCollection().size(); i++ )
        {
          Symbolizer symb = getRuleCollection().get( i ).getSymbolizers()[getSymbolizerIndex()];
          if( symb instanceof PointSymbolizer )
          {
            Object[] obj = ( (PointSymbolizer)symb ).getGraphic().getMarksAndExtGraphics();
            if( obj.length > 0 && obj[0] instanceof Mark )
            {
              ( (Mark)obj[0] ).setWellKnownName( wkn );
            }
          }
        }
        userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
      }
    } );

    SliderPanel graphicSizePanel = new SliderPanel( group, "Size:", 1, 15, 1, SliderPanel.INTEGER,
        graphic.getSize( null ) );
    for( int i = 0; i < getRuleCollection().size(); i++ )
    {
      Symbolizer symb = getRuleCollection().get( i ).getSymbolizers()[getSymbolizerIndex()];
      if( symb instanceof PointSymbolizer )
      {
        ( (PointSymbolizer)symb ).getGraphic().setSize( graphic.getSize( null ) );
      }
    }
    graphicSizePanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        double size = ( (SliderPanel)event.getSource() ).getSelection();
        for( int i = 0; i < getRuleCollection().size(); i++ )
        {
          Symbolizer symb = getRuleCollection().get( i ).getSymbolizers()[getSymbolizerIndex()];
          if( symb instanceof PointSymbolizer )
          {
            ( (PointSymbolizer)symb ).getGraphic().setSize( size );
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
      if( symb instanceof PointSymbolizer )
      {
        Object[] obj = ( (PointSymbolizer)symb ).getGraphic().getMarksAndExtGraphics();
        if( obj.length > 0 && obj[0] instanceof Mark )
        {
          java.awt.Color color = ( (Mark)obj[0] ).getFill().getFill( null );
          colors[i] = new Color( null, color.getRed(), color.getGreen(), color.getBlue() );
        }
      }
    }

    if( colorPalettePanel == null )
    {
      colorPalettePanel = new ColorPalettePanel( group, colors, getRuleCollection());
      colorPalettePanel.setType( ColorPalettePanel.CUSTOM_TRANSITION );
      // init colors of PointSymbolizer
      for( int i = 0; i < getRuleCollection().size(); i++ )
      {
        Symbolizer symb = getRuleCollection().get( i ).getSymbolizers()[getSymbolizerIndex()];
        if( symb instanceof PointSymbolizer )
        {
          Object[] obj = ( (PointSymbolizer)symb ).getGraphic().getMarksAndExtGraphics();
          if( obj.length > 0 && obj[0] instanceof Mark )
          {
            ( (Mark)obj[0] ).getFill()
                .setFill(
                    new java.awt.Color( colors[i].getRed(), colors[i].getGreen(), colors[i]
                        .getBlue() ) );
          }
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
            if( symb instanceof PointSymbolizer )
            {
              ( (Mark)( (PointSymbolizer)symb ).getGraphic().getMarksAndExtGraphics()[0] )
                  .setFill( StyleFactory.createFill( new java.awt.Color( colorArray[i].getRed(),
                      colorArray[i].getGreen(), colorArray[i].getBlue() ) ) );
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