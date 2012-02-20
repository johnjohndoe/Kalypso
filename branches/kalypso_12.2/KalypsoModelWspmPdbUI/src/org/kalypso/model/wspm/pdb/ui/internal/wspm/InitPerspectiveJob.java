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
package org.kalypso.model.wspm.pdb.ui.internal.wspm;

import java.io.File;
import java.io.IOException;
import java.io.StringWriter;
import java.net.URL;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.CharEncoding;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.resources.StringStorage;
import org.kalypso.contribs.eclipse.ui.editorinput.StorageEditorInput;
import org.kalypso.core.jaxb.TemplateUtilities;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.ui.light.WspmGmvViewPart;
import org.kalypso.model.wspm.tuhh.ui.light.WspmMapViewPart;
import org.kalypso.template.gistreeview.Gistreeview;
import org.kalypso.template.types.LayerType;
import org.kalypso.ui.editor.gmleditor.part.GMLContentProvider;
import org.kalypso.ui.editor.gmleditor.part.GmlTreeView;

/**
 * @author Gernot Belger
 */
public class InitPerspectiveJob extends Job
{
  static final String STR_LOCAL_DATA = Messages.getString( "InitPerspectiveJob.0" ); //$NON-NLS-1$

  private final PdbWspmProject m_project;

  public InitPerspectiveJob( final PdbWspmProject project )
  {
    super( Messages.getString( "InitPerspectiveJob.1" ) ); //$NON-NLS-1$

    m_project = project;
  }

  @Override
  protected IStatus run( final IProgressMonitor monitor )
  {
    monitor.beginTask( getName(), 100 );

    initGmvView();
    monitor.worked( 50 );

    initMapView();
    monitor.worked( 50 );

    monitor.done();
    return Status.OK_STATUS;
  }

  void initGmvView( )
  {
    final WspmGmvViewPart gmvView = m_project.findView( WspmGmvViewPart.ID );
    if( gmvView == null )
      return;

    final Runnable setFilterOp = new Runnable()
    {
      @Override
      public void run( )
      {
        final GmlTreeView gmlView = gmvView.getTreeView();
        final GMLContentProvider contentProvider = gmlView.getContentProvider();
        contentProvider.setShowAssociations( false );
        contentProvider.setShowChildrenOverride( TuhhReach.QNAME_TUHH_REACH, Boolean.TRUE );
        contentProvider.setShowChildrenOverride( TuhhReachProfileSegment.QNAME_PROFILEREACHSEGMENT, Boolean.FALSE );
        final TreeViewer treeViewer = gmlView.getTreeViewer();
        final ViewerFilter pdbGmlFilter = new PdbWspmGmlFilter();
        treeViewer.setFilters( new ViewerFilter[] { pdbGmlFilter } );
      }
    };
    gmvView.getSite().getShell().getDisplay().syncExec( setFilterOp );

    final String inputContent = createGmvInput();

    final StringStorage storage = new StringStorage( inputContent, null );
    storage.setName( STR_LOCAL_DATA );

    final IStorageEditorInput input = new StorageEditorInput( storage );
    setGmvInput( gmvView, input );
  }

  private String createGmvInput( )
  {
    try
    {
      final IFile modelFile = m_project.getModelFile();
      final URL modelURL = ResourceUtilities.createQuietURL( modelFile );

      final Gistreeview gistreeview = TemplateUtilities.OF_GISTREEVIEW.createGistreeview();
      final LayerType layerType = TemplateUtilities.OF_TEMPLATE_TYPES.createLayerType();

      layerType.setFeatureXPath( "id( 'root' )/wspm:waterBodyMember" ); // root feature //$NON-NLS-1$
      layerType.setHref( modelURL.toExternalForm() );
      layerType.setLinktype( "gml" ); //$NON-NLS-1$

      gistreeview.setInput( layerType );

      final Marshaller marshaller = TemplateUtilities.createGistreeviewMarshaller( CharEncoding.UTF_8 );
      final StringWriter sw = new StringWriter();
      marshaller.marshal( gistreeview, sw );
      sw.close();
      return sw.toString();
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();
      return null;
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      return null;
    }
  }

  private void setGmvInput( final WspmGmvViewPart gmvView, final IStorageEditorInput input )
  {
    gmvView.setInput( input );

    final UIJob job = new UIJob( StringUtils.EMPTY )
    {
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {
        gmvView.setPartName( STR_LOCAL_DATA );
        return Status.OK_STATUS;
      }
    };
    job.setSystem( true );
    job.schedule();
  }

  void initMapView( )
  {
    final IFile mapFile = ensureMapFile();
    final WspmMapViewPart view = m_project.findView( WspmMapViewPart.ID );
    if( view == null )
      return;

    view.getSite().getShell().getDisplay().asyncExec( new Runnable()
    {
      @Override
      public void run( )
      {
        view.setInput( new FileEditorInput( mapFile ) );
      }
    } );

    m_project.updateMap();
  }

  IFile ensureMapFile( )
  {
    final IFile mapFile = m_project.getMapFile();
    if( mapFile.exists() )
      return mapFile;

    try
    {
      final File mapJavaFile = mapFile.getLocation().toFile();
      final URL mapData = getClass().getResource( "mapTemplate.gmt" ); //$NON-NLS-1$
      FileUtils.copyURLToFile( mapData, mapJavaFile );

      mapFile.refreshLocal( IResource.DEPTH_ONE, new NullProgressMonitor() );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }

    return mapFile;
  }
}