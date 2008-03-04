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
package org.kalypso.ogc.gml;

import java.net.URL;
import java.util.List;

import javax.xml.bind.JAXBElement;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.template.gismapview.CascadingLayer;
import org.kalypso.template.types.ObjectFactory;
import org.kalypso.template.types.StyledLayerType;

/**
 * @author Stefan Kurzbach extended by Dirk Kuch
 */
// TODO: implementing IMapModell here is an ugly hack to show the layers in the outline view
// do not do such a thing. Instead let each theme return a content-provider, so the structure is delegated to the
// themes.
public class CascadingLayerKalypsoTheme extends AbstractCascadingLayerTheme
{
  public CascadingLayerKalypsoTheme( final CascadingLayer layerType, final URL context, final IFeatureSelectionManager selectionManager, final IMapModell mapModel, final String legendIcon, final boolean shouldShowChildren ) throws Exception
  {
    super( layerType.getName(), layerType.getLinktype(), mapModel, legendIcon, context, shouldShowChildren );

    GisTemplateFeatureTheme.configureProperties( this, layerType );

    setInnerMapModel( new GisTemplateMapModell( context, mapModel.getCoordinatesSystem(), mapModel.getProject(), selectionManager )
    {
      /**
       * @see org.kalypso.ogc.gml.GisTemplateMapModell#getThemeParent(org.kalypso.ogc.gml.IKalypsoTheme)
       */
      @Override
      public Object getThemeParent( final IKalypsoTheme theme )
      {
        return CascadingLayerKalypsoTheme.this;
      }
    } );

    final List<JAXBElement< ? extends StyledLayerType>> layers = layerType.getLayer();
    // TODO: maybe get active layer from top-most Gismapview
    getInnerMapModel().createFromTemplate( layers, null );

  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#dispose()
   */
  @Override
  public void dispose( )
  {
    final GisTemplateMapModell innerMapModel = getInnerMapModel();
    if( innerMapModel != null )
      innerMapModel.dispose();

    super.dispose();
  }

  public void fillLayerType( final CascadingLayer layer, final String id, final boolean isVisible, final String srsName, final IProgressMonitor monitor ) throws CoreException
  {
    layer.setId( id );
    layer.setLinktype( "gmt" ); //$NON-NLS-1$
    layer.setActuate( "onRequest" ); //$NON-NLS-1$
    layer.setType( "simple" ); //$NON-NLS-1$

    layer.setName( getName() );
    layer.setVisible( isVisible );
    layer.getDepends();

    final ObjectFactory extentFac = new ObjectFactory();

    String legendIcon = getLegendIcon();
    if( legendIcon != null )
      layer.setLegendicon( extentFac.createStyledLayerTypeLegendicon( legendIcon ) );

    layer.setShowChildren( extentFac.createStyledLayerTypeShowChildren( shouldShowChildren() ) );

    final List<JAXBElement< ? extends StyledLayerType>> layers = layer.getLayer();

    fillLayerList( layers, getInnerMapModel(), srsName, monitor );

    GisTemplateFeatureTheme.fillProperties( this, extentFac, layer );
  }

  private void fillLayerList( final List<JAXBElement< ? extends StyledLayerType>> layers, final IMapModell innerMapModel, final String srsName, final IProgressMonitor monitor ) throws CoreException
  {
    final IKalypsoTheme[] themes = innerMapModel.getAllThemes();

    int count = 0;
    for( final IKalypsoTheme theme : themes )
      GisTemplateHelper.addLayer( layers, theme, count++, getFullExtent(), srsName, monitor );
  }
}