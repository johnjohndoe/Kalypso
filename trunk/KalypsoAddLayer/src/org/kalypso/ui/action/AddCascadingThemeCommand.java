/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBElement;

import org.kalypso.commons.command.ICommand;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.ogc.gml.CascadingLayerKalypsoTheme;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.template.gismapview.CascadingLayer;
import org.kalypso.template.gismapview.ObjectFactory;
import org.kalypso.template.types.StyledLayerType;

/**
 * @author kuch
 */
public class AddCascadingThemeCommand implements ICommand, IThemeCommand
{
  private final IKalypsoLayerModell m_mapModell;

  private final String m_name;

  private final List<ICommand> m_layerCommands = new ArrayList<ICommand>();

  private CascadingLayer m_layer;

  private CascadingLayerKalypsoTheme m_theme;

  private final ADD_THEME_POSITION m_position;

  /**
   * Add Cascading theme constructor
   * 
   * @param mapModell
   *            the gmt file
   * @param name
   *            name of the theme / layer in the map outline
   * @param layerCommands
   *            cmd for adding sub-layers
   */
  public AddCascadingThemeCommand( final IKalypsoLayerModell mapModell, final String name, final ICommand[] layerCommands, final ADD_THEME_POSITION position )
  {
    m_mapModell = mapModell;
    m_name = name;
    m_position = position;

    for( final ICommand command : layerCommands )
      m_layerCommands.add( command );
  }

  public AddCascadingThemeCommand( final IKalypsoLayerModell mapModell, final String name, final ADD_THEME_POSITION position )
  {
    this( mapModell, name, new ICommand[] {}, position );
  }

  public AddCascadingThemeCommand( final IKalypsoLayerModell mapModell, final String name )
  {
    this( mapModell, name, new ICommand[] {}, ADD_THEME_POSITION.eBack );
  }

  public void addCommand( final ICommand command )
  {
    m_layerCommands.add( command );
  }

  private ICommand[] getCommands( )
  {
    return m_layerCommands.toArray( new ICommand[] {} );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return null;
  }

  private void getSubLayer( final ObjectFactory factory, final List<JAXBElement< ? extends StyledLayerType>> layers, final ICommand[] layerCommands )
  {
    final IFeatureSelectionManager selectionManager = KalypsoCorePlugin.getDefault().getSelectionManager();

    final GisTemplateMapModell mapModell = new GisTemplateMapModell( m_mapModell.getContext(), m_mapModell.getCoordinatesSystem(), m_mapModell.getProject(), selectionManager );
    for( final ICommand command : layerCommands )
      if( command instanceof AddThemeCommand )
      {
        final AddThemeCommand myCmd = (AddThemeCommand) command;
        final StyledLayerType myLayer = myCmd.updateMapModel( mapModell );

        final JAXBElement<StyledLayerType> layerElement = factory.createLayer( myLayer );
        layers.add( layerElement );
      }
      else if( command instanceof AddCascadingThemeCommand )
      {
        final AddCascadingThemeCommand cmd = (AddCascadingThemeCommand) command;
        final CascadingLayer layer = cmd.init( factory );
        final List<JAXBElement< ? extends StyledLayerType>> myLayer = layer.getLayer();

        cmd.getSubLayer( factory, myLayer, cmd.getCommands() );

        final JAXBElement<StyledLayerType> layerElement = factory.createLayer( layer );
        layers.add( layerElement );
      }
      else if( command instanceof ISpecialAddThemeCommand )
      {
        final ISpecialAddThemeCommand cmd = (ISpecialAddThemeCommand) command;
        final StyledLayerType myLayer = cmd.getLayer();

        final JAXBElement<StyledLayerType> layerElement = factory.createLayer( myLayer );
        layers.add( layerElement );
      }
  }

  public CascadingLayer init( final ObjectFactory factory )
  {
    final CascadingLayer layer = factory.createCascadingLayer();
    layer.setId( "ID_" + (m_mapModell.getThemeSize() + 1) );
    layer.setLinktype( "gmt" ); //$NON-NLS-1$
    layer.setActuate( "onRequest" ); //$NON-NLS-1$
    layer.setType( "gmt" ); //$NON-NLS-1$

    layer.setName( m_name );
    layer.setVisible( true );
    layer.getDepends();

    return layer;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable( )
  {
    return false;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process( ) throws Exception
  {
    final org.kalypso.template.gismapview.ObjectFactory factory = new org.kalypso.template.gismapview.ObjectFactory();
    m_layer = init( factory );

    final List<JAXBElement< ? extends StyledLayerType>> layers = m_layer.getLayer();
    getSubLayer( factory, layers, m_layerCommands.toArray( new ICommand[] {} ) );

    if( ADD_THEME_POSITION.eFront.equals( m_position ) )
      m_theme = (CascadingLayerKalypsoTheme) m_mapModell.insertLayer( m_layer, 0 );
    else if( ADD_THEME_POSITION.eBack.equals( m_position ) )
      m_theme = (CascadingLayerKalypsoTheme) m_mapModell.addLayer( m_layer );

    m_mapModell.activateTheme( m_theme );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
  }
}
