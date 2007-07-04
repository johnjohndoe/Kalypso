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
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModellListener;
import org.kalypso.ogc.gml.mapmodel.MapModellAdapter;
import org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeVisitor;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.types.LayerType;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.opengis.cs.CS_CoordinateSystem;
import org.xml.sax.InputSource;

/**
 * @author Stefan Kurzbach
 */
// TODO: implementing IMapModell here is an ugly hack to show the layers in the outline view
// do not do such a thing. Instead let each theme return a content-provider, so the strucutre is delegated to the
// themes.
public class CascadingKalypsoTheme extends AbstractKalypsoTheme implements ITemplateTheme, IMapModell
{
  /**
   * @author Stefan Kurzbach
   */
  protected final class GmtFileChangeListener implements IResourceChangeListener
  {
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

  private final IMapModellListener m_modelListener = new MapModellAdapter()
  {
    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeAdded(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme)
     */
    @Override
    public void themeAdded( final IMapModell source, final IKalypsoTheme theme )
    {
      CascadingKalypsoTheme.this.invalidate( theme.getBoundingBox() );
    }

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeOrderChanged(org.kalypso.ogc.gml.mapmodel.IMapModell)
     */
    @Override
    public void themeOrderChanged( final IMapModell source )
    {
      CascadingKalypsoTheme.this.invalidate( getBoundingBox() );

      // TODO: HACK, still looking for a better way to forward theme events to all modell listeners
      handleThemeStatusChanged();
    }

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeRemoved(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme, boolean)
     */
    @Override
    public void themeRemoved( final IMapModell source, final IKalypsoTheme theme, final boolean lastVisibility )
    {
      if( lastVisibility )
        CascadingKalypsoTheme.this.invalidate( theme.getBoundingBox() );
    }

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeVisibilityChanged(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme, boolean)
     */
    @Override
    public void themeVisibilityChanged( final IMapModell source, final IKalypsoTheme theme, final boolean visibility )
    {
      CascadingKalypsoTheme.this.invalidate( theme.getBoundingBox() );
    }

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeStatusChanged(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme)
     */
    @Override
    public void themeStatusChanged( final IMapModell source, final IKalypsoTheme theme )
    {
      handleThemeStatusChanged();
    }

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeContextChanged(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme)
     */
    @Override
    public void themeContextChanged( final IMapModell source, final IKalypsoTheme theme )
    {
      handleThemeContextChanged();
    }

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeActivated(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme, org.kalypso.ogc.gml.IKalypsoTheme)
     */
    @Override
    public void themeActivated( final IMapModell source, final IKalypsoTheme previouslyActive, final IKalypsoTheme nowActive )
    {
      handleThemeActivated();
    }

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#repaintRequested(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypsodeegree.model.geometry.GM_Envelope)
     */
    @Override
    public void repaintRequested( final IMapModell source, final GM_Envelope bbox )
    {
      CascadingKalypsoTheme.this.invalidate( bbox );
    }

  };

  private final GisTemplateMapModell m_innerMapModel;

  private final String m_mapViewRefUrl;

  private IResourceChangeListener m_resourceChangeListener;

  IFile m_file;

  public CascadingKalypsoTheme( final StyledLayerType layerType, final URL context, final IFeatureSelectionManager selectionManager, final IMapModell mapModel ) throws Exception
  {
    super( layerType.getName(), "Cascading", mapModel );

    m_mapViewRefUrl = layerType.getHref();

    final URL url = resolveUrl( context, m_mapViewRefUrl );
    m_innerMapModel = new GisTemplateMapModell( url, mapModel.getCoordinatesSystem(), mapModel.getProject(), selectionManager )
    {
      /**
       * @see org.kalypso.ogc.gml.GisTemplateMapModell#getThemeParent(org.kalypso.ogc.gml.IKalypsoTheme)
       */
      @Override
      public Object getThemeParent( final IKalypsoTheme theme )
      {
        return CascadingKalypsoTheme.this;
      }
    };

    m_innerMapModel.addMapModelListener( m_modelListener );

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

  private static URL resolveUrl( final URL context, final String viewRefUrl ) throws CoreException
  {
    try
    {
      return new URL( context, viewRefUrl );
    }
    catch( final MalformedURLException e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e, "Kein gültiger Kontext " + context + " oder Kartenreferenz " + viewRefUrl ) );
    }
  }

  protected void startLoadJob( ) throws Exception
  {
    // Do not do such a thing, if the resources are not fresh, something else is wrong!
    // Apart from, that this causes a scheduling rule conflict so no map ican be opened in expert mode
// m_file.refreshLocal( IResource.DEPTH_ZERO, null );
    final InputSource inputSource = new InputSource( m_file.getContents() );
    Gismapview innerGisView;
    try
    {
      innerGisView = GisTemplateHelper.loadGisMapView( inputSource );
      final String innerName = innerGisView.getName();
      if( innerName != null )
      {
        ((IMapModell) this).setName( innerName );
        ((IKalypsoTheme) this).setName( innerName );
      }
      m_innerMapModel.createFromTemplate( innerGisView );
      fireContextChanged();
      fireStatusChanged();
    }
    catch( final JAXBException e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e, "Konnte " + m_file.getName() + " nicht laden." ) );
    }
  }

  public void createGismapTemplate( final GM_Envelope bbox, final String srsName, final IProgressMonitor monitor ) throws CoreException
  {
    m_innerMapModel.createGismapTemplate( bbox, srsName, getName(), monitor, m_file );
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
  public void paint( final Graphics g, final GeoTransform p, final double scale, final GM_Envelope bbox, final boolean selected )
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
  public void activateTheme( final IKalypsoTheme theme )
  {
    m_innerMapModel.activateTheme( theme );
  }

  /**
   * @param theme
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#addTheme(org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public void addTheme( final IKalypsoTheme theme )
  {
    m_innerMapModel.addTheme( theme );
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
  public IKalypsoTheme getTheme( final int pos )
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
  public boolean isThemeActivated( final IKalypsoTheme theme )
  {
    return m_innerMapModel.isThemeActivated( theme );
  }

  /**
   * @param theme
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#moveDown(org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public void moveDown( final IKalypsoTheme theme )
  {
    m_innerMapModel.moveDown( theme );
  }

  /**
   * @param theme
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#moveUp(org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public void moveUp( final IKalypsoTheme theme )
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
  public void paint( final Graphics g, final GeoTransform p, final GM_Envelope bbox, final double scale, final boolean select )
  {
    m_innerMapModel.paint( g, p, bbox, scale, select );
  }

  /**
   * @param theme
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#removeTheme(org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public void removeTheme( final IKalypsoTheme theme )
  {
    m_innerMapModel.removeTheme( theme );
  }

  /**
   * @param theme1
   * @param theme2
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#swapThemes(org.kalypso.ogc.gml.IKalypsoTheme,
   *      org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public void swapThemes( final IKalypsoTheme theme1, final IKalypsoTheme theme2 )
  {
    m_innerMapModel.swapThemes( theme1, theme2 );
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#accept(org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeVisitor, int)
   */
  public void accept( final KalypsoThemeVisitor visitor, final int depth_infinite )
  {
    m_innerMapModel.accept( visitor, depth_infinite );

  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#insertTheme(org.kalypso.ogc.gml.IKalypsoTheme, int)
   */
  public void insertTheme( final IKalypsoTheme theme, final int position )
  {
    m_innerMapModel.insertTheme( theme, position );

  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#accept(org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeVisitor, int,
   *      org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public void accept( final KalypsoThemeVisitor visitor, final int depth_infinite, final IKalypsoTheme theme )
  {
    m_innerMapModel.accept( visitor, depth_infinite, theme );
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#getChildren(java.lang.Object)
   */
  @Override
  public Object[] getChildren( final Object o )
  {
    return m_innerMapModel.getChildren( o );
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#addMapModelListener(org.kalypso.ogc.gml.mapmodel.IMapModellListener)
   */
  public void addMapModelListener( final IMapModellListener l )
  {
    m_innerMapModel.addMapModelListener( l );
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#removeMapModelListener(org.kalypso.ogc.gml.mapmodel.IMapModellListener)
   */
  public void removeMapModelListener( final IMapModellListener l )
  {
    m_innerMapModel.removeMapModelListener( l );
  }

  protected void handleThemeStatusChanged( )
  {
    CascadingKalypsoTheme.this.fireStatusChanged();
  }

  protected void handleThemeContextChanged( )
  {
    CascadingKalypsoTheme.this.fireContextChanged();
  }

  protected void handleThemeActivated( )
  {
    CascadingKalypsoTheme.this.fireStatusChanged();
  }

  /**
   * Overwritten in order to have corrct parent for tree strcutures.
   * 
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#getThemeParent(org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public Object getThemeParent( final IKalypsoTheme theme )
  {
    // do not delegate to inner model, this would be wrong.
    return this;
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#getImageDescriptor(java.lang.Object)
   */
  @Override
  public ImageDescriptor getImageDescriptor( final Object object )
  {
    Assert.isLegal( object == this );

    return KalypsoGisPlugin.getImageProvider().getImageDescriptor( ImageProvider.DESCRIPTORS.IMAGE_THEME_CASCADING );
  }

}
