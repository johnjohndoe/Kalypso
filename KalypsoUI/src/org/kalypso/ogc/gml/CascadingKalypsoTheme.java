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

import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.types.LayerType;
import org.kalypso.template.types.ObjectFactory;
import org.kalypso.template.types.StyledLayerType;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.xml.sax.InputSource;

/**
 * @author Stefan Kurzbach
 */
public class CascadingKalypsoTheme extends AbstractCascadingLayerTheme
{
  private final IResourceChangeListener m_resourceChangeListener = new IResourceChangeListener()
  {
    public void resourceChanged( IResourceChangeEvent event )
    {
      handleResourceChanged( event );
    }
  };

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

  private final String m_mapViewRefUrl;

  private final IFile m_file;

  public CascadingKalypsoTheme( final StyledLayerType layerType, final URL context, final IFeatureSelectionManager selectionManager, final IMapModell mapModel, final String legendIcon, final boolean shouldShowChildren ) throws Exception
  {
    super( layerType.getName(), "Cascading", mapModel, legendIcon, context, shouldShowChildren );

    m_mapViewRefUrl = layerType.getHref();

    GisTemplateFeatureTheme.configureProperties( this, layerType );

    final URL url = CascadingKalypsoTheme.resolveUrl( context, m_mapViewRefUrl );
    setInnerMapModel( new GisTemplateMapModell( url, mapModel.getCoordinatesSystem(), mapModel.getProject(), selectionManager )
    {
      /**
       * @see org.kalypso.ogc.gml.GisTemplateMapModell#getThemeParent(org.kalypso.ogc.gml.IKalypsoTheme)
       */
      @Override
      public Object getThemeParent( final IKalypsoTheme theme )
      {
        return CascadingKalypsoTheme.this;
      }
    } );

    m_file = ResourceUtilities.findFileFromURL( url );
    if( m_file != null )
    {
      m_file.getWorkspace().addResourceChangeListener( m_resourceChangeListener, IResourceChangeEvent.POST_CHANGE );
      startLoadJob();
    }
    else
      throw new CoreException( StatusUtilities.createErrorStatus( "Kann " + url.toExternalForm() + " nicht finden." ) );
  }

  public void createGismapTemplate( final GM_Envelope bbox, final String srsName, final IProgressMonitor monitor ) throws CoreException
  {
    if( m_file != null )
    {
      try
      {
        /* Remove resource listener while saving: prohibits unnecessary reloading when file is saved */
        m_file.getWorkspace().removeResourceChangeListener( m_resourceChangeListener );

        getInnerMapModel().createGismapTemplate( bbox, srsName, getName(), monitor, m_file );
      }
      finally
      {
        m_file.getWorkspace().addResourceChangeListener( m_resourceChangeListener, IResourceChangeEvent.POST_CHANGE );
      }
    }
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

    if( m_resourceChangeListener != null )
      m_file.getWorkspace().removeResourceChangeListener( m_resourceChangeListener );

    super.dispose();
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

      final ObjectFactory extentFac = new ObjectFactory();

      String legendIcon = getLegendIcon();
      if( legendIcon != null )
        styledLayerType.setLegendicon( extentFac.createStyledLayerTypeLegendicon( legendIcon ) );

      styledLayerType.setShowChildren( extentFac.createStyledLayerTypeShowChildren( shouldShowChildren() ) );

      GisTemplateFeatureTheme.fillProperties( this, extentFac, styledLayerType );
    }
  }

  /**
   * Returns the reference to the cascaded .gmt file.
   */
  public String getMapViewRefUrl( )
  {
    return m_mapViewRefUrl;
  }

  /**
   * @see org.kalypso.ogc.gml.ITemplateTheme#saveFeatures(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public void saveFeatures( final IProgressMonitor monitor ) throws CoreException
  {
    if( getInnerMapModel() != null )
      for( final IKalypsoTheme theme : getInnerMapModel().getAllThemes() )
        if( theme instanceof GisTemplateFeatureTheme )
          ((IKalypsoSaveableTheme) theme).saveFeatures( monitor );
  }

  protected void startLoadJob( )
  {
    final UIJob job = new UIJob( "Kaskadierendes Thema laden: " + m_file.getName() )
    {
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {
        InputStream contents = null;
        try
        {
          contents = m_file.getContents();
          final InputSource inputSource = new InputSource( contents );
          final Gismapview innerGisView = GisTemplateHelper.loadGisMapView( inputSource );
          final String innerName = innerGisView.getName();
          if( innerName != null )
          {
            CascadingKalypsoTheme.this.setName( innerName );
          }
          getInnerMapModel().createFromTemplate( innerGisView );
          fireContextChanged();
        }
        catch( final Throwable e )
        {
          final IStatus status = StatusUtilities.statusFromThrowable( e, "Konnte " + m_file.getName() + " nicht laden." );
          setStatus( status );
          return status;
        }
        finally
        {
          IOUtils.closeQuietly( contents );
        }
        return Status.OK_STATUS;
      }
    };
    job.setRule( m_file );
    job.schedule();
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#fireContextChanged()
   */
  @Override
  protected void fireContextChanged( )
  {
    super.fireContextChanged();
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#fireStatusChanged()
   */
  @Override
  protected void fireStatusChanged( )
  {
    super.fireStatusChanged();
  }

  protected void handleResourceChanged( final IResourceChangeEvent event )
  {
    if( m_file == null )
      return;

    final IResourceDelta rootDelta = event.getDelta();
    final IResourceDelta fileDelta = rootDelta.findMember( m_file.getFullPath() );
    if( fileDelta == null )
      return;
    final int kind = fileDelta.getKind();
    switch( kind )
    {
      case IResourceDelta.CHANGED:
      case IResourceDelta.REMOVED:
      case IResourceDelta.ADDED:
        startLoadJob();
        break;
    }
  }
}