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
package org.kalypso.flows;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Properties;

import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.namespace.QName;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.window.Window;
import org.eclipse.ui.IEditorPart;
import org.kalypso.commons.java.util.PropertiesHelper;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.loader.WfsLoader;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.Gismapview.Layers;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.ui.editor.mapeditor.GisMapEditor;
import org.kalypso.workflow.ui.browser.AbstractURLActionAnalizeTheme;
import org.kalypso.workflow.ui.browser.ICommandURL;
import org.kalypsodeegree.filterencoding.Filter;
import org.kalypsodeegree_impl.filterencoding.OperationDefines;
import org.xml.sax.InputSource;

/**
 * @author kuepfer
 */
public class URLActionFLOWSAddFilterToGMT extends AbstractURLActionAnalizeTheme
{

  /**
   * Values for the opertion key.<br>
   * supported spatial operations: <br>
   * BBOX, Equals, Disjoint, Intersects, Touches, Crosses, Within, Contains, Overlaps, Beyond, DWithin.
   */
  public final static String PARAM_GEOM_OPERATION = "spatialOperation";

  public final static String PARAM_PATH_GMT = "pathGMT";

  /**
   * @see org.kalypso.workflow.ui.browser.AbstractURLActionAnalizeTheme#analyze(java.lang.String[],
   *      org.kalypso.ogc.gml.IKalypsoTheme[], org.kalypso.workflow.ui.browser.ICommandURL)
   */
  @Override
  public boolean analyze( final String[] linkTypes, final IKalypsoTheme[] themes, final ICommandURL commandURL )
  {
    final String pathGMT = commandURL.getParameter( PARAM_PATH_GMT );
    final int operation = OperationDefines.getIdByName( commandURL.getParameter( PARAM_GEOM_OPERATION ) );

    final ArrayList<QName> propertyNames = new ArrayList<QName>();
    final ArrayList<IKalypsoFeatureTheme> themesToFilter = new ArrayList<IKalypsoFeatureTheme>();

    for( IKalypsoTheme theme : themes )
    {
      // if there are not linkTypes definde all themes are analized
      final boolean hasLinkType;
      if( linkTypes != null && linkTypes.length > 0 )
        hasLinkType = ArrayUtils.contains( linkTypes, theme.getType() );
      else
        hasLinkType = true;
      if( (theme instanceof IKalypsoFeatureTheme) && hasLinkType )
      {
        final IKalypsoFeatureTheme kft = (IKalypsoFeatureTheme) theme;
        final IFeatureType ft = kft.getFeatureType();
        // this happens if the theme is not loaded
        if( ft == null )
          continue;
        final IValuePropertyType geom = ft.getDefaultGeometryProperty();
        if( geom == null )
          continue;
        final QName qName = geom.getQName();
        propertyNames.add( qName );
        themesToFilter.add( kft );
      }

    }

    if( themesToFilter.isEmpty() )
      return generateMessageDialog( "No themes to generate filter for.", IStatus.CANCEL );
    final IKalypsoFeatureTheme[] themesToFilterArray = themesToFilter.toArray( new IKalypsoFeatureTheme[themesToFilter.size()] );
    final IEditorPart activeEditor = getActiveEditor();
    MapPanel mapPanel = null;
    if( activeEditor instanceof GisMapEditor )
      mapPanel = ((GisMapEditor) activeEditor).getMapPanel();
    final SimpleFilterChooserDialog dialog = new SimpleFilterChooserDialog( getShell(), themesToFilterArray, propertyNames.toArray( new QName[propertyNames.size()] ), mapPanel, operation );
    if( dialog.open() == Window.OK )
    {
      try
      {
        final URL gmtURL = getWorkFlowContext().resolveURL( pathGMT );
        final HashMap<IKalypsoFeatureTheme, Filter> filters = dialog.getFilter();
        addFilterToGMT( filters, gmtURL, themesToFilterArray );
      }
      catch( Exception e )
      {
        e.printStackTrace();
        generateMessageDialog( e.getMessage(), IStatus.ERROR );
        return false;
      }
    }
    return false;
  }

  private void addFilterToGMT( final HashMap<IKalypsoFeatureTheme, Filter> filters, final URL gmtURL, final IKalypsoFeatureTheme[] themes ) throws Exception
  {
    final Unmarshaller unmarshaller = GisTemplateHelper.JC_GISMAPVIEW.createUnmarshaller();
    final InputSource isToGMT = new InputSource( new InputStreamReader( gmtURL.openStream() ) );
    final Gismapview mapview = (Gismapview) unmarshaller.unmarshal( isToGMT );
    final Layers mapViewLayers = mapview.getLayers();
    final List<StyledLayerType> styledLayers = mapViewLayers.getLayer();

    for( IKalypsoFeatureTheme kft : themes )
    {
      final StyledLayerType sldLayer = getStyledLayerType( styledLayers, kft );
      String originalHref = sldLayer.getHref();
      final Properties properties = PropertiesHelper.parseFromString( originalHref, WfsLoader.KV_PAIR_SEPARATOR );
      // add filter string to href
      properties.setProperty( WfsLoader.KEY_FILTER, filters.get( kft ).toXML().toString() );
      final String newHref = PropertiesHelper.writePropertiesToString( properties, WfsLoader.KV_PAIR_SEPARATOR );
      // replace old href
      sldLayer.setHref( newHref );
    }

    final ByteArrayOutputStream bos = new ByteArrayOutputStream();
    ByteArrayInputStream bis = null;
    try
    {
      final Marshaller marshaller = GisTemplateHelper.JC_GISMAPVIEW.createMarshaller();
      marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
      marshaller.marshal( mapview, bos );
      bis = new ByteArrayInputStream( bos.toByteArray() );
      final IFile gmtToFile = ResourceUtilities.findFileFromURL( gmtURL );
      if( gmtToFile.exists() )
        gmtToFile.setContents( bis, false, true, null );
    }
    finally
    {
      IOUtils.closeQuietly( bos );
      IOUtils.closeQuietly( bis );
    }

  }

  private StyledLayerType getStyledLayerType( List<StyledLayerType> styledLayers, IKalypsoTheme theme )
  {
    for( StyledLayerType sldLayer : styledLayers )
    {
      final String name = sldLayer.getName();
      if( name.equals( theme.getName() ) )
        return sldLayer;
    }
    return null;
  }

}
