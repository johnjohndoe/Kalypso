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
import org.eclipse.core.resources.IProject;
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
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.opengis.cs.CS_CoordinateSystem;
import org.xml.sax.InputSource;

/**
 * @author Stefan Kurzbach
 */
// TODO: implementing IMapModell here is an ugly hack to show the layers in the outline view
// do not do such a thing. Instead let each theme return a content-provider, so the strucutre is delegated to the themes.
public class CascadingKalypsoTheme extends AbstractKalypsoTheme implements IKalypsoTheme, ITemplateTheme, IMapModell
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
        catch( final Exception e )
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

  private String m_customName;

  public CascadingKalypsoTheme( final StyledLayerType layerType, final URL context, final IFeatureSelectionManager selectionManager, final IMapModell mapModel ) throws Exception
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

  void startLoadJob( ) throws Exception
  {
    m_file.refreshLocal( IResource.DEPTH_ZERO, null );
    final InputSource inputSource = new InputSource( m_file.getContents() );
    Gismapview innerGisView;
    try
    {
      innerGisView = GisTemplateHelper.loadGisMapView( inputSource );
      m_customName = innerGisView.getName();
      m_innerMapModel.createFromTemplate( innerGisView );
      fireKalypsoThemeEvent( new KalypsoThemeEvent( this, KalypsoThemeEvent.CONTENT_CHANGED ) );
    }
    catch( final JAXBException e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e, "Konnte " + m_file.getName() + " nicht laden." ) );
    }
  }

  public void createGismapTemplate( final GM_Envelope bbox, final String srsName, final IProgressMonitor monitor ) throws CoreException
  {
    m_innerMapModel.createGismapTemplate( bbox, srsName, m_customName, monitor, m_file );
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

  /* delegate methods to IMapModell */

  /**
   * @param theme
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#activateTheme(org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public void activateTheme( IKalypsoTheme theme )
  {
    m_innerMapModel.activateTheme( theme );
  }

  /**
   * @param theme
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#addTheme(org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public void addTheme( IKalypsoTheme theme )
  {
    m_innerMapModel.addTheme( theme );
  }

  /**
   * @param theme
   * @param status
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#enableTheme(org.kalypso.ogc.gml.IKalypsoTheme, boolean)
   */
  public void enableTheme( IKalypsoTheme theme, boolean status )
  {
    m_innerMapModel.enableTheme( theme, status );
  }

  /**
   * @return
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#getActiveTheme()
   */
  public IKalypsoTheme getActiveTheme( )
  {
    return m_innerMapModel.getActiveTheme();
  }

  /**
   * @return
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#getAllThemes()
   */
  public IKalypsoTheme[] getAllThemes( )
  {
    return m_innerMapModel.getAllThemes();
  }

  /**
   * @return
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#getCoordinatesSystem()
   */
  public CS_CoordinateSystem getCoordinatesSystem( )
  {
    return m_innerMapModel.getCoordinatesSystem();
  }

  /**
   * @return
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#getFullExtentBoundingBox()
   */
  public GM_Envelope getFullExtentBoundingBox( )
  {
    return m_innerMapModel.getFullExtentBoundingBox();
  }

  /**
   * @return
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#getProject()
   */
  public IProject getProject( )
  {
    return m_innerMapModel.getProject();
  }

  /**
   * @return
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#getScrabLayer()
   */
  public IKalypsoFeatureTheme getScrabLayer( )
  {
    return m_innerMapModel.getScrabLayer();
  }

  /**
   * @param pos
   * @return
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#getTheme(int)
   */
  public IKalypsoTheme getTheme( int pos )
  {
    return m_innerMapModel.getTheme( pos );
  }

  /**
   * @return
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#getThemeSize()
   */
  public int getThemeSize( )
  {
    return m_innerMapModel.getThemeSize();
  }

  /**
   * @param theme
   * @return
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#isThemeActivated(org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public boolean isThemeActivated( IKalypsoTheme theme )
  {
    return m_innerMapModel.isThemeActivated( theme );
  }

  /**
   * @param theme
   * @return
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#isThemeEnabled(org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public boolean isThemeEnabled( IKalypsoTheme theme )
  {
    return m_innerMapModel.isThemeEnabled( theme );
  }

  /**
   * @param theme
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#moveDown(org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public void moveDown( IKalypsoTheme theme )
  {
    m_innerMapModel.moveDown( theme );
  }

  /**
   * @param theme
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#moveUp(org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public void moveUp( IKalypsoTheme theme )
  {
    m_innerMapModel.moveUp( theme );
  }

  /**
   * @param g
   * @param p
   * @param bbox
   * @param scale
   * @param select
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, org.kalypsodeegree.model.geometry.GM_Envelope,
   *      double, boolean)
   */
  public void paint( Graphics g, GeoTransform p, GM_Envelope bbox, double scale, boolean select )
  {
    m_innerMapModel.paint( g, p, bbox, scale, select );
  }

  /**
   * @param theme
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#removeTheme(org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public void removeTheme( IKalypsoTheme theme )
  {
    m_innerMapModel.removeTheme( theme );
  }

  /**
   * @param crs
   * @throws Exception
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#setCoordinateSystem(org.opengis.cs.CS_CoordinateSystem)
   */
  public void setCoordinateSystem( CS_CoordinateSystem crs ) throws Exception
  {
    m_innerMapModel.setCoordinateSystem( crs );
  }

  /**
   * @param theme1
   * @param theme2
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#swapThemes(org.kalypso.ogc.gml.IKalypsoTheme,
   *      org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public void swapThemes( IKalypsoTheme theme1, IKalypsoTheme theme2 )
  {
    m_innerMapModel.swapThemes( theme1, theme2 );
  }

  /**
   * @param listener
   * @see org.kalypso.ogc.gml.GisTemplateMapModell#addModellListener(org.kalypsodeegree.model.feature.event.ModellEventListener)
   */
  @Override
  public void addModellListener( ModellEventListener listener )
  {
    m_innerMapModel.addModellListener( listener );
  }

  /**
   * @param modellEvent
   * @see org.kalypso.ogc.gml.GisTemplateMapModell#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  @Override
  public void onModellChange( ModellEvent modellEvent )
  {
    m_innerMapModel.onModellChange( modellEvent );
  }

  /**
   * @param listener
   * @see org.kalypso.ogc.gml.GisTemplateMapModell#removeModellListener(org.kalypsodeegree.model.feature.event.ModellEventListener)
   */
  @Override
  public void removeModellListener( ModellEventListener listener )
  {
    m_innerMapModel.removeModellListener( listener );
  }

}
