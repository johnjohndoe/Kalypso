/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
/*
 * Created on 26.07.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.symbolizerLayouts;

import org.kalypsodeegree.filterencoding.Expression;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.Font;
import org.kalypsodeegree.graphics.sld.Geometry;
import org.kalypsodeegree.graphics.sld.Halo;
import org.kalypsodeegree.graphics.sld.LabelPlacement;
import org.kalypsodeegree.graphics.sld.ParameterValueType;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.graphics.sld.TextSymbolizer;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree_impl.filterencoding.PropertyName;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ui.editor.styleeditor.MessageBundle;
import org.kalypso.ui.editor.styleeditor.StyleEditorHelper;
import org.kalypso.ui.editor.styleeditor.panels.ColorChooserPanel;
import org.kalypso.ui.editor.styleeditor.panels.FontChooserPanel;
import org.kalypso.ui.editor.styleeditor.panels.LabelPlacementComboPanel;
import org.kalypso.ui.editor.styleeditor.panels.LabelPointPlacementPanel;
import org.kalypso.ui.editor.styleeditor.panels.PanelEvent;
import org.kalypso.ui.editor.styleeditor.panels.PanelListener;
import org.kalypso.ui.editor.styleeditor.panels.SliderPanel;
import org.kalypso.ui.editor.styleeditor.panels.TextInputPanel;
import org.kalypso.ui.editor.styleeditor.panels.TextLabelComboPanel;

/**
 * @author F.Lindemann
 *  
 */

public class TextSymbolizerLayout extends AbstractSymbolizerLayout
{

  private final FeatureType m_featureTyped;

  private TextInputPanel labelTextInput = null;

  private TextLabelComboPanel textLabelComboPanel = null;

  private Halo halo = null;

  private LabelPlacement labelPlacement = null;

  public final static int GM_POINT = 0;

  public final static int GM_LINESTRING = 1;

  public final static int GM_POLYGON = 2;

  public final static int GM_MULTIPOINT = 3;

  public final static int GM_OBJECT = 4;

  //  private final FeatureTypeProperty m_ftp;

  public TextSymbolizerLayout( Composite composite, Symbolizer symbolizer,
      KalypsoUserStyle userStyle, FeatureType featureType )
  {
    super( composite, symbolizer, userStyle );
    m_featureTyped = featureType;
    //    m_ftp = ftp;
  }

  public void draw() throws FilterEvaluationException
  {
    final TextSymbolizer textSymbolizer = (TextSymbolizer)symbolizer;

    GridLayout compositeLayout = new GridLayout();
    compositeLayout.marginHeight = 2;

    // ***** Font group
    Group fontGroup = new Group( composite, SWT.NULL );
    GridData fontGroupData = new GridData();
    fontGroupData.widthHint = 210;
    fontGroupData.heightHint = 244;
    fontGroup.setLayoutData( fontGroupData );
    fontGroup.setLayout( compositeLayout );
    fontGroup.layout();

    // check whether already a Label-source in sld
    ParameterValueType label = textSymbolizer.getLabel();
    String labelTextCombo = null;
    String labelTextField = null;
    if( label != null )
    {
      Object obj[] = label.getComponents();
      for( int i = 0; i < obj.length; i++ )
      {
        if( obj[i] instanceof PropertyName )
        {
          labelTextCombo = ( (PropertyName)obj[i] ).getValue().trim();
          break;
        }
        else if( obj[i] instanceof String )
        {
          String labelString = ( (String)obj[i] ).trim();
          if( labelString.length() > 0 )
          {
            labelTextField = labelString;
            break;
          }
        }
      }
    }
    textLabelComboPanel = new TextLabelComboPanel( fontGroup, MessageBundle.STYLE_EDITOR_LABEL,
        m_featureTyped, labelTextCombo );
    labelTextInput = new TextInputPanel( fontGroup, MessageBundle.STYLE_EDITOR_OR_TEXT,
        labelTextField );

    textLabelComboPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        String ftpString = ( (TextLabelComboPanel)event.getSource() )
            .getSelectedFeatureTypeProperty();
        PropertyName propName = new PropertyName( ftpString );
        Expression exp[] =
        { propName };
        textSymbolizer.setLabel( StyleFactory.createParameterValueType( exp ) );
        getLabelTextInput().reset();
        userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
      }
    } );
    labelTextInput.addPanelListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        String labelText = ( (TextInputPanel)event.getSource() ).getLabelText();
        textSymbolizer.setLabel( StyleFactory.createParameterValueType( labelText ) );
        getTextLabelComboPanel().reset();
        userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
      }
    } );

    Font font = textSymbolizer.getFont();
    FontChooserPanel fontChooserPanel = new FontChooserPanel( fontGroup,
        MessageBundle.STYLE_EDITOR_FONT, font );
    fontChooserPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        FontChooserPanel source = (FontChooserPanel)event.getSource();
        Font m_font = source.getFont();
        textSymbolizer.setFont( m_font );
        userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
      }
    } );

    halo = textSymbolizer.getHalo();
    if( halo == null )
    {
      halo = StyleFactory.createHalo();
      halo.getFill().setOpacity( 0.3 );
    }
    // Halo ColorChooser
    ColorChooserPanel haloColorChooserPanel = null;
    haloColorChooserPanel = new ColorChooserPanel( fontGroup, MessageBundle.STYLE_EDITOR_COLOR,
        halo.getFill().getFill( null ) );
    haloColorChooserPanel.addColorChooserListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        Color color = ( (ColorChooserPanel)event.getSource() ).getColor();
        getHalo().getFill().setFill(
            new java.awt.Color( color.getRed(), color.getGreen(), color.getBlue() ) );
        if( textSymbolizer.getHalo() == null )
          textSymbolizer.setHalo( getHalo() );
        userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
      }
    } );

    // Halo Opacity Slider
    SliderPanel haloOpacityPanel = null;
    haloOpacityPanel = new SliderPanel( fontGroup, MessageBundle.STYLE_EDITOR_OPACITY, 0, 1, 1,
        SliderPanel.DECIMAL, halo.getFill().getOpacity( null ) );
    haloOpacityPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        double opacity = ( (SliderPanel)event.getSource() ).getSelection();
        getHalo().getFill().setOpacity( opacity );
        if( textSymbolizer.getHalo() == null )
          textSymbolizer.setHalo( getHalo() );
        userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
      }
    } );

    // Halo Stroke Opacity Slider
    SliderPanel haloStrokeOpacityPanel = null;
    haloStrokeOpacityPanel = new SliderPanel( fontGroup, MessageBundle.STYLE_EDITOR_STROKE_OPACITY,
        0, 1, 1, SliderPanel.DECIMAL, halo.getStroke().getOpacity( null ) );
    haloStrokeOpacityPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        double opacity = ( (SliderPanel)event.getSource() ).getSelection();
        getHalo().getStroke().setOpacity( opacity );
        if( textSymbolizer.getHalo() == null )
          textSymbolizer.setHalo( getHalo() );
        userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
      }
    } );

    labelPlacement = textSymbolizer.getLabelPlacement();
    final FeatureTypeProperty ftp;
    Geometry geometry = textSymbolizer.getGeometry();
    if( geometry != null )
    {
      String geoPropName = geometry.getPropertyName();
      ftp = StyleEditorHelper.getFeatureTypeProperty( m_featureTyped,
          geoPropName );
      
    }
    else
      ftp = m_featureTyped.getDefaultGeometryProperty();
    if( getFeatureTypeGeometryType( ftp ) == GM_LINESTRING )
    {
      if( labelPlacement == null )
        labelPlacement = StyleFactory.createLabelPlacement( StyleFactory
            .createLinePlacement( "auto" ) );
      int linePlacementIndex = labelPlacement.getLinePlacement().getPlacementType( null );
      LabelPlacementComboPanel labelPlacementComboBoxPanel = new LabelPlacementComboPanel(
          fontGroup, MessageBundle.STYLE_EDITOR_PLACEMENT, linePlacementIndex );
      labelPlacementComboBoxPanel.addPanelListener( new PanelListener()
      {
        public void valueChanged( PanelEvent event )
        {
          int type = ( (LabelPlacementComboPanel)event.getSource() ).getSelection();
          getLabelPlacement().getLinePlacement().setPlacementType( type );
          if( textSymbolizer.getLabelPlacement() == null )
            textSymbolizer.setLabelPlacement( getLabelPlacement() );
          userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
        }
      } );

      SliderPanel gapPanel = new SliderPanel( fontGroup, MessageBundle.STYLE_EDITOR_GAP, 0, 10, 1,
          SliderPanel.INTEGER, labelPlacement.getLinePlacement().getGap( null ) );
      gapPanel.addPanelListener( new PanelListener()
      {
        public void valueChanged( PanelEvent event )
        {
          double gap = ( (SliderPanel)event.getSource() ).getSelection();
          getLabelPlacement().getLinePlacement().setGap( (int)gap );
          if( textSymbolizer.getLabelPlacement() == null )
            textSymbolizer.setLabelPlacement( getLabelPlacement() );
          userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
        }
      } );

    }
    else
    {
      if( labelPlacement == null )
        labelPlacement = StyleFactory.createLabelPlacement( StyleFactory.createPointPlacement( 0.0,
            0.0, 0.0, 0.0, 0.0 ) );

      double displacement[] = labelPlacement.getPointPlacement().getDisplacement( null );
      LabelPointPlacementPanel labelPointPlacementPanel = new LabelPointPlacementPanel( fontGroup,
          MessageBundle.STYLE_EDITOR_PLACEMENT, displacement );
      labelPointPlacementPanel.addPanelListener( new PanelListener()
      {
        public void valueChanged( PanelEvent event )
        {
          double disp[] = ( (LabelPointPlacementPanel)event.getSource() ).getValue();
          getLabelPlacement().getPointPlacement().setDisplacement( disp );
          if( textSymbolizer.getLabelPlacement() == null )
            textSymbolizer.setLabelPlacement( getLabelPlacement() );
          userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
        }
      } );

      SliderPanel rotationPanel = rotationPanel = new SliderPanel( fontGroup,
          MessageBundle.STYLE_EDITOR_ROTATION, 0, 360, 15, SliderPanel.INTEGER, labelPlacement
              .getPointPlacement().getRotation( null ) * 180 );
      rotationPanel.addPanelListener( new PanelListener()
      {
        public void valueChanged( PanelEvent event )
        {
          double rotation = ( (SliderPanel)event.getSource() ).getSelection();
          rotation = rotation / 180.0;
          getLabelPlacement().getPointPlacement().setRotation( rotation );
          if( textSymbolizer.getLabelPlacement() == null )
            textSymbolizer.setLabelPlacement( getLabelPlacement() );
          userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
        }
      } );
    }
  }

  public static int getFeatureTypeGeometryType( final FeatureTypeProperty ftp )
  {
    if( ftp == null )
      return -1;
    final String ft = ftp.getType();
    if( ft.equals( "org.kalypsodeegree.model.geometry.GM_Point" ) )
      return GM_POINT;
    else if( ft.equals( "org.kalypsodeegree.model.geometry.GM_LineString" ) )
      return GM_LINESTRING;
    else if( ft.equals( "org.kalypsodeegree.model.geometry.GM_Polygon" ) )
      return GM_POLYGON;
    else if( ft.equals( "org.kalypsodeegree.model.geometry.GM_MultiPoint" ) )
      return GM_MULTIPOINT;
    else if( ft.equals( "org.kalypsodeegree.model.geometry.GM_Object" ) ) //multilinestring,
      // multipolygon
      return GM_OBJECT;
    return -1;
  }

  public TextInputPanel getLabelTextInput()
  {
    return labelTextInput;
  }

  public void setLabelTextInput( TextInputPanel m_labelTextInput )
  {
    this.labelTextInput = m_labelTextInput;
  }

  public LabelPlacement getLabelPlacement()
  {
    return labelPlacement;
  }

  public void setLabelPlacement( LabelPlacement m_labelPlacement )
  {
    this.labelPlacement = m_labelPlacement;
  }

  public Halo getHalo()
  {
    return halo;
  }

  public void setHalo( Halo m_halo )
  {
    this.halo = m_halo;
  }

  public TextLabelComboPanel getTextLabelComboPanel()
  {
    return textLabelComboPanel;
  }

}