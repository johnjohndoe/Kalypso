/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ui.action;

import java.util.List;

import org.kalypso.commons.command.ICommand;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.template.types.ObjectFactory;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.template.types.StyledLayerType.Style;

public class AddThemeCommand implements ICommand
{

  private GisTemplateMapModell m_mapModell;

  private IKalypsoTheme m_theme;

  private final String m_name;

  private final String m_type;

  private final String m_featurePath;

  private final String m_source;

  private final String m_stylelinktype;

  private final String m_style;

  private final String m_styleLocation;

  private final String m_styleType;

  private StyledLayerType m_layer;

  public AddThemeCommand( final GisTemplateMapModell model, final String name, final String type, final String featurePath, final String source )
  {
    this( model, name, type, featurePath, source, null, null, null, null );
  }

  /**
   * This command adds a new layer to
   * 
   * @param model
   *            active GisTemplateMapModell from the active Map
   * @param name
   *            name of the layer
   * @param type
   *            type of source (must be a valid loader) ex.: wms, wfs, shape, etc.
   * @param featurePath
   *            the feature path in the gml workspace
   * @param source
   *            a String having keywords and (paired values) depending on the Loader context
   * @param stylelinktype
   *            keyword for the used style type (normally sld)
   * @param style
   *            name of the style
   * @param styleLocation
   *            a valid resouce path (of the used plugin or a valid URL )
   * @param styleType
   *            sets the type simple or complex
   * @deprecated use constrtuctor without style information TODO separate AddTheme from AddStyle, at the moment AddTheme
   *             is both
   */
  @Deprecated
  public AddThemeCommand( final GisTemplateMapModell model, final String name, final String type, final String featurePath, final String source, final String stylelinktype, final String style, final String styleLocation, final String styleType )
  {
    m_mapModell = model;
    m_name = name;
    m_type = type;
    m_featurePath = featurePath;
    m_source = source;
    m_stylelinktype = stylelinktype;
    m_style = style;
    m_styleLocation = styleLocation;
    m_styleType = styleType;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return "Einfügen eines neuen Themas in die Karte";
  }

  private StyledLayerType init( )
  {

    final int id = m_mapModell.getThemeSize() + 1;
    final ObjectFactory factory = new ObjectFactory();

    final StyledLayerType layer = factory.createStyledLayerType();
    layer.setHref( m_source );
    layer.setFeaturePath( m_featurePath );
    layer.setName( m_name );
    layer.setLinktype( m_type );
    layer.setId( "ID_" + id );
    layer.setVisible( true );
    if( m_stylelinktype != null && m_style != null && m_styleLocation != null && m_styleType != null )
    {
      final List<Style> styleList = layer.getStyle();
      // Style Type
      final StyledLayerType.Style layertype = factory.createStyledLayerTypeStyle();
      layertype.setLinktype( m_stylelinktype );
      layertype.setStyle( m_style );
      layertype.setHref( m_styleLocation );
      layertype.setActuate( "onRequest" );
      layertype.setType( m_styleType );
      styleList.add( layertype );
    }

    return layer;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable( )
  {
    return true;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process( ) throws Exception
  {
    m_layer = init();
    m_theme = m_mapModell.insertTheme( m_layer, 0 );
    m_mapModell.activateTheme( m_theme );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    m_mapModell.addTheme( m_layer );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
    m_mapModell.removeTheme( m_theme );
  }

  public StyledLayerType updateMapModel( final GisTemplateMapModell model )
  {
    m_mapModell = model;
    return init();
  }

}