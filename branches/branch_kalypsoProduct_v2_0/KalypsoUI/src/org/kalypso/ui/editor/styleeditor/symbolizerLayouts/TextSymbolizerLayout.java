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

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
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
import org.kalypsodeegree.filterencoding.Expression;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.Font;
import org.kalypsodeegree.graphics.sld.Geometry;
import org.kalypsodeegree.graphics.sld.Halo;
import org.kalypsodeegree.graphics.sld.LabelPlacement;
import org.kalypsodeegree.graphics.sld.ParameterValueType;
import org.kalypsodeegree.graphics.sld.PointPlacement;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.graphics.sld.TextSymbolizer;
import org.kalypsodeegree_impl.filterencoding.PropertyName;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author F.Lindemann
 */

public class TextSymbolizerLayout extends AbstractSymbolizerLayout
{

  private final IFeatureType m_featureTyped;

  private TextInputPanel labelTextInput = null;

  private TextLabelComboPanel textLabelComboPanel = null;

  private Halo halo = null;

  private LabelPlacement labelPlacement = null;

  public final static int NO_GEOMETRY = -1;

  public final static int GM_POINT = 0;

  public final static int GM_MULTIPOINT = 1;

  public final static int GM_LINESTRING = 2;

  public static final int GM_MULTILINESTRING = 3;

  public final static int GM_POLYGON = 4;

  public static final int GM_MULTIPOLYGON = 5;

  public final static int GM_OBJECT = 6;

  public TextSymbolizerLayout( final Composite comp, final Symbolizer symb, final KalypsoUserStyle style, final IFeatureType featureType )
  {
    super( comp, symb, style );
    m_featureTyped = featureType;
  }

  @Override
  public void draw( ) throws FilterEvaluationException
  {
    final TextSymbolizer textSymbolizer = (TextSymbolizer) symbolizer;

    final GridLayout compositeLayout = new GridLayout();
    compositeLayout.marginHeight = 2;

    // ***** Font group
    final Group fontGroup = new Group( composite, SWT.NULL );
    final GridData fontGroupData = new GridData();
    fontGroupData.widthHint = 210;
    fontGroupData.heightHint = 244;
    fontGroup.setLayoutData( fontGroupData );
    fontGroup.setLayout( compositeLayout );
    fontGroup.layout();

    // check whether already a Label-source in sld
    final ParameterValueType label = textSymbolizer.getLabel();
    String labelTextCombo = null;
    String labelTextField = null;
    if( label != null )
    {
      final Object obj[] = label.getComponents();
      for( final Object element : obj )
      {
        if( element instanceof PropertyName )
        {
          labelTextCombo = ((PropertyName) element).getValue().trim();
          break;
        }
        else if( element instanceof String )
        {
          final String labelString = ((String) element).trim();
          if( labelString.length() > 0 )
          {
            labelTextField = labelString;
            break;
          }
        }
      }
    }
    textLabelComboPanel = new TextLabelComboPanel( fontGroup, MessageBundle.STYLE_EDITOR_LABEL, m_featureTyped, labelTextCombo );
    labelTextInput = new TextInputPanel( fontGroup, MessageBundle.STYLE_EDITOR_OR_TEXT, labelTextField );

    textLabelComboPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( final PanelEvent event )
      {
        final String ftpString = ((TextLabelComboPanel) event.getSource()).getSelectedFeatureTypeProperty();
        final PropertyName propName = new PropertyName( ftpString );
        final Expression exp[] = { propName };
        textSymbolizer.setLabel( StyleFactory.createParameterValueType( exp ) );
        getLabelTextInput().reset();
        userStyle.fireStyleChanged();
      }
    } );
    labelTextInput.addPanelListener( new PanelListener()
    {
      public void valueChanged( final PanelEvent event )
      {
        final String labelText = ((TextInputPanel) event.getSource()).getLabelText();
        textSymbolizer.setLabel( StyleFactory.createParameterValueType( labelText ) );
        getTextLabelComboPanel().reset();
        userStyle.fireStyleChanged();
      }
    } );

    final Font font = textSymbolizer.getFont();
    final FontChooserPanel fontChooserPanel = new FontChooserPanel( fontGroup, MessageBundle.STYLE_EDITOR_FONT, font );
    fontChooserPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( final PanelEvent event )
      {
        final FontChooserPanel source = (FontChooserPanel) event.getSource();
        final Font m_font = source.getFont();
        textSymbolizer.setFont( m_font );
        userStyle.fireStyleChanged();
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
    haloColorChooserPanel = new ColorChooserPanel( fontGroup, MessageBundle.STYLE_EDITOR_COLOR, halo.getFill().getFill( null ) );
    haloColorChooserPanel.addColorChooserListener( new PanelListener()
    {
      public void valueChanged( final PanelEvent event )
      {
        final Color color = ((ColorChooserPanel) event.getSource()).getColor();
        getHalo().getFill().setFill( new java.awt.Color( color.getRed(), color.getGreen(), color.getBlue() ) );
        if( textSymbolizer.getHalo() == null )
          textSymbolizer.setHalo( getHalo() );
        userStyle.fireStyleChanged();
      }
    } );

    // Halo Opacity Slider
    SliderPanel haloOpacityPanel = null;
    haloOpacityPanel = new SliderPanel( fontGroup, MessageBundle.STYLE_EDITOR_OPACITY, 0, 1, 1, SliderPanel.DECIMAL, halo.getFill().getOpacity( null ) );
    haloOpacityPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( final PanelEvent event )
      {
        final double opacity = ((SliderPanel) event.getSource()).getSelection();
        getHalo().getFill().setOpacity( opacity );
        if( textSymbolizer.getHalo() == null )
          textSymbolizer.setHalo( getHalo() );
        userStyle.fireStyleChanged();
      }
    } );

    // Halo Stroke Opacity Slider
    SliderPanel haloStrokeOpacityPanel = null;
    haloStrokeOpacityPanel = new SliderPanel( fontGroup, MessageBundle.STYLE_EDITOR_STROKE_OPACITY, 0, 1, 1, SliderPanel.DECIMAL, halo.getStroke().getOpacity( null ) );
    haloStrokeOpacityPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( final PanelEvent event )
      {
        final double opacity = ((SliderPanel) event.getSource()).getSelection();
        getHalo().getStroke().setOpacity( opacity );
        if( textSymbolizer.getHalo() == null )
          textSymbolizer.setHalo( getHalo() );
        userStyle.fireStyleChanged();
      }
    } );

    labelPlacement = textSymbolizer.getLabelPlacement();
    final IPropertyType ftp;
    final Geometry geometry = textSymbolizer.getGeometry();
    if( geometry != null )
    {
      final PropertyName geoPropName = geometry.getPropertyName();
      ftp = StyleEditorHelper.getFeatureTypeProperty( m_featureTyped, geoPropName );

    }
    else
      ftp = m_featureTyped.getDefaultGeometryProperty();
    if( getFeatureTypeGeometryType( ftp ) == GM_LINESTRING )
    {
      if( labelPlacement == null )
        labelPlacement = StyleFactory.createLabelPlacement( StyleFactory.createLinePlacement( "auto" ) ); //$NON-NLS-1$
      final int linePlacementIndex = labelPlacement.getLinePlacement().getPlacementType( null );
      final LabelPlacementComboPanel labelPlacementComboBoxPanel = new LabelPlacementComboPanel( fontGroup, MessageBundle.STYLE_EDITOR_PLACEMENT, linePlacementIndex );
      labelPlacementComboBoxPanel.addPanelListener( new PanelListener()
      {
        public void valueChanged( final PanelEvent event )
        {
          final int type = ((LabelPlacementComboPanel) event.getSource()).getSelection();
          getLabelPlacement().getLinePlacement().setPlacementType( type );
          if( textSymbolizer.getLabelPlacement() == null )
            textSymbolizer.setLabelPlacement( getLabelPlacement() );
          userStyle.fireStyleChanged();
        }
      } );

      final SliderPanel gapPanel = new SliderPanel( fontGroup, MessageBundle.STYLE_EDITOR_GAP, 0, 10, 1, SliderPanel.INTEGER, labelPlacement.getLinePlacement().getGap( null ) );
      gapPanel.addPanelListener( new PanelListener()
      {
        public void valueChanged( final PanelEvent event )
        {
          final double gap = ((SliderPanel) event.getSource()).getSelection();
          getLabelPlacement().getLinePlacement().setGap( (int) gap );
          if( textSymbolizer.getLabelPlacement() == null )
            textSymbolizer.setLabelPlacement( getLabelPlacement() );
          userStyle.fireStyleChanged();
        }
      } );

    }
    else
    {
      if( labelPlacement == null )
        labelPlacement = StyleFactory.createLabelPlacement( StyleFactory.createPointPlacement( 0.0, 0.0, 0.0, 0.0, 0.0 ) );

      final PointPlacement pointPlacement = labelPlacement.getPointPlacement();
      final double displacement[] = pointPlacement.getDisplacement( null );
      final LabelPointPlacementPanel labelPointPlacementPanel = new LabelPointPlacementPanel( fontGroup, MessageBundle.STYLE_EDITOR_PLACEMENT, displacement );
      labelPointPlacementPanel.addPanelListener( new PanelListener()
      {
        public void valueChanged( final PanelEvent event )
        {
          final double disp[] = ((LabelPointPlacementPanel) event.getSource()).getValue();
          getLabelPlacement().getPointPlacement().setDisplacement( disp );
          if( textSymbolizer.getLabelPlacement() == null )
            textSymbolizer.setLabelPlacement( getLabelPlacement() );
          userStyle.fireStyleChanged();
        }
      } );

      final SliderPanel rotationPanel = new SliderPanel( fontGroup, MessageBundle.STYLE_EDITOR_ROTATION, 0, 360, 15, SliderPanel.INTEGER, pointPlacement.getRotation( null ) * 180 );
      rotationPanel.addPanelListener( new PanelListener()
      {
        public void valueChanged( final PanelEvent event )
        {
          double rotation = ((SliderPanel) event.getSource()).getSelection();
          rotation = rotation / 180.0;
          getLabelPlacement().getPointPlacement().setRotation( rotation );
          if( textSymbolizer.getLabelPlacement() == null )
            textSymbolizer.setLabelPlacement( getLabelPlacement() );
          userStyle.fireStyleChanged();
        }
      } );
    }
  }

  public static int getFeatureTypeGeometryType( final IPropertyType ftp )
  {
    if( ftp == null )
      return NO_GEOMETRY;
    if( ftp instanceof IValuePropertyType )
    {
      final IValuePropertyType vpt = (IValuePropertyType) ftp;
      if( GeometryUtilities.isPointGeometry( vpt ) )
        return GM_POINT;
      else if( GeometryUtilities.isMultiPointGeometry( vpt ) )
        return GM_MULTIPOINT;
      else if( GeometryUtilities.isLineStringGeometry( vpt ) )
        return GM_LINESTRING;
      else if( GeometryUtilities.isMultiLineStringGeometry( vpt ) )
        return GM_MULTILINESTRING;
      else if( GeometryUtilities.isPolygonGeometry( vpt ) )
        return GM_POLYGON;
      else if( GeometryUtilities.isMultiPolygonGeometry( vpt ) )
        return GM_MULTIPOLYGON;
      else if( GeometryUtilities.isUndefinedGeometry( vpt ) )
        return GM_OBJECT;
    }
    return NO_GEOMETRY;
  }

  public TextInputPanel getLabelTextInput( )
  {
    return labelTextInput;
  }

  public void setLabelTextInput( final TextInputPanel m_labelTextInput )
  {
    this.labelTextInput = m_labelTextInput;
  }

  public LabelPlacement getLabelPlacement( )
  {
    return labelPlacement;
  }

  public void setLabelPlacement( final LabelPlacement m_labelPlacement )
  {
    this.labelPlacement = m_labelPlacement;
  }

  public Halo getHalo( )
  {
    return halo;
  }

  public void setHalo( final Halo m_halo )
  {
    this.halo = m_halo;
  }

  public TextLabelComboPanel getTextLabelComboPanel( )
  {
    return textLabelComboPanel;
  }

}