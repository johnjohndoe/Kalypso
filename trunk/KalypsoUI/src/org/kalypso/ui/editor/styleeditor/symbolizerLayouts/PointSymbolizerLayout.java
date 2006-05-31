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

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ui.editor.styleeditor.MessageBundle;
import org.kalypso.ui.editor.styleeditor.panels.ColorChooserPanel;
import org.kalypso.ui.editor.styleeditor.panels.ComboPanel;
import org.kalypso.ui.editor.styleeditor.panels.ConfigurePointSymbolizerPanel;
import org.kalypso.ui.editor.styleeditor.panels.FormatDisplayPanel;
import org.kalypso.ui.editor.styleeditor.panels.PanelEvent;
import org.kalypso.ui.editor.styleeditor.panels.PanelListener;
import org.kalypso.ui.editor.styleeditor.panels.SliderPanel;
import org.kalypso.ui.editor.styleeditor.panels.UrlInputPanel;
import org.kalypso.ui.editor.styleeditor.panels.WellKnownNameComboPanel;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.ExternalGraphic;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.Graphic;
import org.kalypsodeegree.graphics.sld.Mark;
import org.kalypsodeegree.graphics.sld.PointSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;

/**
 * @author F.Lindemann
 */

public class PointSymbolizerLayout extends AbstractSymbolizerLayout
{

  private int markCounter = 1;

  private int extGraphicCounter = 1;

  private Composite tabFolderComposite = null;

  private int selectionIndex = 0;

  public PointSymbolizerLayout( Composite m_composite, Symbolizer m_symbolizer, KalypsoUserStyle m_userStyle )
  {
    super( m_composite, m_symbolizer, m_userStyle );
  }

  @Override
  public void draw( ) throws FilterEvaluationException
  {
    final PointSymbolizer pointSymbolizer = (PointSymbolizer) symbolizer;
    final Graphic graphic = pointSymbolizer.getGraphic();
    markCounter = 1;
    extGraphicCounter = 1;

    if( tabFolderComposite != null )
      tabFolderComposite.dispose();
    tabFolderComposite = new Composite( composite, SWT.NULL );
    tabFolderComposite.setLayout( new FormLayout() );
    tabFolderComposite.layout();

    final TabFolder markExtGraphicTabFolder = new TabFolder( tabFolderComposite, SWT.NULL );
    FormData markExtGraphicTabFolderData = new FormData();
    markExtGraphicTabFolderData.height = 133;
    markExtGraphicTabFolderData.width = 208;
    markExtGraphicTabFolderData.left = new FormAttachment( 10, 1000, 0 );
    markExtGraphicTabFolderData.top = new FormAttachment( 20, 1000, 0 );
    markExtGraphicTabFolder.setLayoutData( markExtGraphicTabFolderData );

    // ***** Graphic-Detail Section after TabFolder
    Composite graphicDetailComposite = new Composite( tabFolderComposite, SWT.NULL );
    FormLayout formLayout = new FormLayout();
    graphicDetailComposite.setLayout( formLayout );
    FormData labelData = new FormData();
    labelData.height = 92;
    labelData.width = 198;
    labelData.left = new FormAttachment( 72, 1000, 0 );
    labelData.top = new FormAttachment( 650, 1000, 0 );
    graphicDetailComposite.setLayoutData( labelData );

    Composite graphicDetails = new Composite( graphicDetailComposite, SWT.NULL );
    GridLayout graphicDetailsLayout = new GridLayout();
    graphicDetails.setLayout( graphicDetailsLayout );
    graphicDetailsLayout.marginHeight = 0;
    graphicDetailsLayout.marginWidth = 0;
    graphicDetails.layout();

    final Object objects[] = graphic.getMarksAndExtGraphics();

    for( int i = 0; i < objects.length; i++ )
    {
      drawTabItem( markExtGraphicTabFolder, objects[i] );
    }
    if( selectionIndex < objects.length )
      markExtGraphicTabFolder.setSelection( selectionIndex );

    SliderPanel graphicSizePanel = new SliderPanel( graphicDetails, MessageBundle.STYLE_EDITOR_SIZE, 1, 15, 1, SliderPanel.INTEGER, graphic.getSize( null ) );
    graphicSizePanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        double size = ((SliderPanel) event.getSource()).getSelection();
        graphic.setSize( size );
        userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
      }
    } );

    // SliderPanel graphicOpacityPanel = new SliderPanel( graphicDetails,
    // MessageBundle.STYLE_EDITOR_OPACITY, 0, 1, 1,
    // SliderPanel.DECIMAL, graphic.getOpacity( null ) );
    // graphicOpacityPanel.addPanelListener( new PanelListener()
    // {
    // public void valueChanged( PanelEvent event )
    // {
    // double opacity = ( (SliderPanel)event.getSource() ).getSelection();
    // graphic.setOpacity( opacity );
    // userStyle.fireModellEvent( new ModellEvent( userStyle,
    // ModellEvent.STYLE_CHANGE ) );
    // }
    // } );

    SliderPanel rotationPanel = new SliderPanel( graphicDetails, MessageBundle.STYLE_EDITOR_ROTATION, 0, 360, 15, SliderPanel.INTEGER, graphic.getRotation( null ) * 180 );
    rotationPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        double rotation = ((SliderPanel) event.getSource()).getSelection();
        graphic.setRotation( rotation );
        userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
      }
    } );

    ConfigurePointSymbolizerPanel configurePointSymbolizerPanel = new ConfigurePointSymbolizerPanel( graphicDetails, objects.length );
    configurePointSymbolizerPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        int action = ((ConfigurePointSymbolizerPanel) event.getSource()).getAction();
        if( action == ConfigurePointSymbolizerPanel.ADD_MARK )
        {
          try
          {
            Mark newMark = StyleFactory.createMark( "square" );
            pointSymbolizer.getGraphic().addMarksAndExtGraphic( newMark );
            setSelectionIndex( graphic.getMarksAndExtGraphics().length - 1 );
            draw();
            userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
          }
          catch( FilterEvaluationException e )
          {
            e.printStackTrace();
          }
        }
        else if( action == ConfigurePointSymbolizerPanel.REM_MARK )
        {
          int index = markExtGraphicTabFolder.getSelectionIndex();
          if( objects.length > 0 )
          {
            Object object = objects[index];
            pointSymbolizer.getGraphic().removeMarksAndExtGraphic( object );
            setSelectionIndex( index - 1 );
            if( getSelectionIndex() < 0 )
              setSelectionIndex( 0 );
            userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
            try
            {
              draw();
            }
            catch( FilterEvaluationException e )
            {
              e.printStackTrace();
            }
          }
        }
        else if( action == ConfigurePointSymbolizerPanel.FOR_MARK )
        {
          int index = markExtGraphicTabFolder.getSelectionIndex();
          if( index == (objects.length - 1) )
            return;

          Object newOrderedObjects[] = new Object[objects.length];
          for( int i = 0; i < objects.length; i++ )
          {
            if( i == index )
              newOrderedObjects[i] = objects[i + 1];
            else if( i == (index + 1) )
              newOrderedObjects[i] = objects[i - 1];
            else
              newOrderedObjects[i] = objects[i];
          }
          graphic.setMarksAndExtGraphics( newOrderedObjects );
          setSelectionIndex( index + 1 );
          userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
          try
          {
            draw();
          }
          catch( FilterEvaluationException e )
          {
            e.printStackTrace();
          }
        }
        else if( action == ConfigurePointSymbolizerPanel.BAK_MARK )
        {
          int index = markExtGraphicTabFolder.getSelectionIndex();
          if( index == 0 )
            return;

          Object newOrderedObjects[] = new Object[objects.length];
          for( int i = 0; i < objects.length; i++ )
          {
            if( i == index )
              newOrderedObjects[i] = objects[i - 1];
            else if( i == (index - 1) )
              newOrderedObjects[i] = objects[i + 1];
            else
              newOrderedObjects[i] = objects[i];
          }
          graphic.setMarksAndExtGraphics( newOrderedObjects );
          setSelectionIndex( index - 1 );
          userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
          try
          {
            draw();
          }
          catch( FilterEvaluationException e )
          {
            e.printStackTrace();
          }
        }
      }
    } );

    tabFolderComposite.pack( true );
  }

  private void drawTabItem( TabFolder markExtGraphicTabFolder, Object object ) throws FilterEvaluationException
  {
    TabItem tabItem = new TabItem( markExtGraphicTabFolder, SWT.NULL );
    Composite tabItemComposite = new Composite( markExtGraphicTabFolder, SWT.NULL );

    GridLayout compositeLayout = new GridLayout();
    tabItemComposite.setLayout( compositeLayout );
    compositeLayout.marginHeight = 0;
    compositeLayout.marginWidth = 0;
    tabItemComposite.layout();
    tabItem.setControl( tabItemComposite );

    // ***** Fill Group

    GridLayout groupLayout = new GridLayout();
    groupLayout.marginHeight = 2;

    Group group = new Group( tabItemComposite, SWT.NULL );
    group.setText( "" );
    GridData fillGroupData = new GridData();
    fillGroupData.widthHint = 202;
    group.setLayoutData( fillGroupData );
    group.setLayout( groupLayout );
    group.layout();

    // ***** Mark or External-Graphic Group
    if( object instanceof Mark )
    {
      final Mark mark = (Mark) object;
      tabItem.setText( "Mark" + (markCounter++) );
      ComboPanel wellKnownNameComboBox = new WellKnownNameComboPanel( group, MessageBundle.STYLE_EDITOR_TYPE, mark.getWellKnownName() );
      wellKnownNameComboBox.addPanelListener( new PanelListener()
      {
        public void valueChanged( PanelEvent event )
        {
          int index = ((ComboPanel) event.getSource()).getSelection();
          mark.setWellKnownName( WellKnownNameComboPanel.getWellKnownNameByIndex( index ) );
          userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
        }
      } );

      final Fill markFill;
      if( mark.getFill() == null )
        markFill = StyleFactory.createFill();
      else
        markFill = mark.getFill();
      ColorChooserPanel fillColorChooserPanel = new ColorChooserPanel( group, MessageBundle.STYLE_EDITOR_FILL_COLOR, markFill.getFill( null ) );
      fillColorChooserPanel.addColorChooserListener( new PanelListener()
      {
        public void valueChanged( PanelEvent event )
        {
          Color color = ((ColorChooserPanel) event.getSource()).getColor();
          markFill.setFill( new java.awt.Color( color.getRed(), color.getGreen(), color.getBlue() ) );
          userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
        }
      } );

      SliderPanel fillOpacityPanel = new SliderPanel( group, MessageBundle.STYLE_EDITOR_FILL_OPACITY, 0, 1, 1, SliderPanel.DECIMAL, markFill.getOpacity( null ) );
      fillOpacityPanel.addPanelListener( new PanelListener()
      {
        public void valueChanged( PanelEvent event )
        {
          double opacity = ((SliderPanel) event.getSource()).getSelection();
          markFill.setOpacity( opacity );
          userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
        }
      } );

      final Stroke markStroke;
      if( mark.getStroke() == null )
        markStroke = StyleFactory.createStroke();
      else
        markStroke = mark.getStroke();
      ColorChooserPanel strokeColorChooserPanel = new ColorChooserPanel( group, MessageBundle.STYLE_EDITOR_COLOR, markStroke.getStroke( null ) );
      strokeColorChooserPanel.addColorChooserListener( new PanelListener()
      {
        public void valueChanged( PanelEvent event )
        {
          Color color = ((ColorChooserPanel) event.getSource()).getColor();
          markStroke.setStroke( new java.awt.Color( color.getRed(), color.getGreen(), color.getBlue() ) );
          userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
        }
      } );

      // SliderPanel strokeWidthPanel = new SliderPanel( group,
      // MessageBundle.STYLE_EDITOR_WIDTH, 0, 10, 1,
      // SliderPanel.INTEGER, markStroke.getWidth( null ) );
      // strokeWidthPanel.addPanelListener( new PanelListener()
      // {
      // public void valueChanged( PanelEvent event )
      // {
      // double width = ( (SliderPanel)event.getSource() ).getSelection();
      // markStroke.setWidth( width );
      // userStyle.fireModellEvent( new ModellEvent( userStyle,
      // ModellEvent.STYLE_CHANGE ) );
      // }
      // } );

      SliderPanel strokeOpacityPanel = new SliderPanel( group, MessageBundle.STYLE_EDITOR_OPACITY, 0, 1, 1, SliderPanel.DECIMAL, markStroke.getOpacity( null ) );
      strokeOpacityPanel.addPanelListener( new PanelListener()
      {
        public void valueChanged( PanelEvent event )
        {
          double opacity = ((SliderPanel) event.getSource()).getSelection();
          markStroke.setOpacity( opacity );
          userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
        }
      } );
    }
    else if( object instanceof ExternalGraphic )
    {
      final ExternalGraphic externalGraphic = (ExternalGraphic) object;
      tabItem.setText( "ExtGraph" + (extGraphicCounter++) );
      UrlInputPanel urlInputPanel;
      try
      {
        URL onlineResourceURL = externalGraphic.getOnlineResourceURL();
        urlInputPanel = new UrlInputPanel( group, MessageBundle.STYLE_EDITOR_URL, onlineResourceURL );
        urlInputPanel.addPanelListener( new PanelListener()
        {
          public void valueChanged( PanelEvent event )
          {
            URL url = ((UrlInputPanel) event.getSource()).getURL();
            externalGraphic.setOnlineResource( url.toString() );
            userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
          }
        } );
      }
      catch( MalformedURLException e )
      {
        e.printStackTrace();
      }
      new FormatDisplayPanel( group, MessageBundle.STYLE_EDITOR_FORMAT, externalGraphic.getFormat() );
    }
  }

  public int getSelectionIndex( )
  {
    return selectionIndex;
  }

  public void setSelectionIndex( int m_selectionIndex )
  {
    this.selectionIndex = m_selectionIndex;
  }
}