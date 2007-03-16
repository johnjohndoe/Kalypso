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
package org.kalypso.ogc.gml;

import java.awt.Graphics;
import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.bind.JAXBException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.types.LayerType;
import org.kalypso.template.types.StyledLayerType;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.xml.sax.InputSource;

/**
 * @author Stefan Kurzbach
 */
public class CascadingKalypsoTheme extends AbstractKalypsoTheme implements IKalypsoTheme, ITemplateTheme
{
  /**
   * @author Stefan Kurzbach
   */
  private final class GmtFileChangeListener implements IResourceChangeListener
  {
    GmtFileChangeListener( )
    {
    }

    /**
     * @see org.eclipse.core.resources.IResourceChangeListener#resourceChanged(org.eclipse.core.resources.IResourceChangeEvent)
     */
    public void resourceChanged( final IResourceChangeEvent event )
    {
      if( m_file == null )
      {
        return;
      }
      final IResourceDelta rootDelta = event.getDelta();
      final IResourceDelta fileDelta = rootDelta.findMember( m_file.getFullPath() );
      if( fileDelta == null )
      {
        return;
      }
      if( (fileDelta.getFlags() & IResourceDelta.CONTENT) != 0 )
      {
        try
        {
          startLoadJob();
        }
        catch( final CoreException e )
        {
          // TODO something useful
          e.printStackTrace();
        }
      }
    }
  }

  private GisTemplateMapModell m_innerMapModel;

  private String m_mapViewRefUrl;

  private IResourceChangeListener m_resourceChangeListener;

  IFile m_file;

  public CascadingKalypsoTheme( final StyledLayerType layerType, final URL context, final IFeatureSelectionManager selectionManager, final IMapModell mapModel ) throws CoreException
  {
    super( layerType.getHref(), "Cascading", mapModel );
    m_mapViewRefUrl = layerType.getHref();
    URL url = null;
    try
    {
      url = new URL( context, m_mapViewRefUrl );
    }
    catch( final MalformedURLException e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e, "Kein gültiger Kontext " + context + " oder Kartenreferenz " + m_mapViewRefUrl ) );
    }
    if( url != null )
    {
      m_innerMapModel = new GisTemplateMapModell( url, mapModel.getCoordinatesSystem(), mapModel.getProject(), selectionManager );
      m_file = ResourceUtilities.findFileFromURL( url );
      if( m_file != null )
      {
        m_resourceChangeListener = new GmtFileChangeListener();
        m_file.getWorkspace().addResourceChangeListener( m_resourceChangeListener, IResourceChangeEvent.POST_CHANGE );
        startLoadJob();
      }
      else
      {
        throw new CoreException( StatusUtilities.createErrorStatus( "Kann " + url.toExternalForm() + " nicht finden." ) );
      }
    }
  }

  void startLoadJob( ) throws CoreException
  {
    m_file.refreshLocal( IResource.DEPTH_ZERO, null );
    final InputSource inputSource = new InputSource( m_file.getContents() );
    Gismapview innerGisView;
    try
    {
      innerGisView = GisTemplateHelper.loadGisMapView( inputSource );
      m_innerMapModel.createFromTemplate( innerGisView );
      fireKalypsoThemeEvent( new KalypsoThemeEvent( this, KalypsoThemeEvent.CONTENT_CHANGED ) );
    }
    catch( final JAXBException e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e, "Konnte " + m_file.getName() + " nicht laden." ) );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#dispose()
   */
  @Override
  public void dispose( )
  {
    if( m_innerMapModel != null )
      m_innerMapModel.dispose();
    if( m_resourceChangeListener != null )
    {
      m_file.getWorkspace().removeResourceChangeListener( m_resourceChangeListener );
    }
    super.dispose();
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getBoundingBox()
   */
  public GM_Envelope getBoundingBox( )
  {
    if( m_innerMapModel != null )
    {
      return m_innerMapModel.getFullExtentBoundingBox();
    }
    else
    {
      return null;
    }
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, double,
   *      org.kalypsodeegree.model.geometry.GM_Envelope, boolean)
   */
  public void paint( Graphics g, GeoTransform p, double scale, GM_Envelope bbox, boolean selected )
  {
    if( m_innerMapModel != null )
    {
      m_innerMapModel.paint( g, p, bbox, scale, selected );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.ITemplateTheme#saveFeatures(org.eclipse.core.runtime.IProgressMonitor)
   */
  public void saveFeatures( final IProgressMonitor monitor ) throws CoreException
  {
    if( m_innerMapModel != null )
    {
      for( final IKalypsoTheme theme : m_innerMapModel.getAllThemes() )
      {
        if( theme instanceof GisTemplateFeatureTheme )
        {
          m_innerMapModel.saveTheme( (ITemplateTheme) theme, monitor );
        }
      }
    }
  }

  public void fillLayerType( final LayerType layer, final String id, final boolean isVisible )
  {
    layer.setId( id );
    layer.setHref( m_mapViewRefUrl );
    layer.setLinktype( "gmt" ); //$NON-NLS-1$
    layer.setActuate( "onRequest" ); //$NON-NLS-1$
    layer.setType( "simple" ); //$NON-NLS-1$    
    if( layer instanceof StyledLayerType )
    {
      final StyledLayerType styledLayerType = (StyledLayerType) layer;
      styledLayerType.setName( getName() );
      styledLayerType.setVisible( isVisible );
      styledLayerType.getDepends();
    }
  }

}
