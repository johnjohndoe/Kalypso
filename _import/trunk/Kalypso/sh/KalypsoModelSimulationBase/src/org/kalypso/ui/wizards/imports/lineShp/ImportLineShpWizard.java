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
package org.kalypso.ui.wizards.imports.lineShp;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;

import javax.xml.bind.JAXBException;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.Gismapview.Layers;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.wizards.imports.INewWizardKalypsoImport;
import org.kalypso.ui.wizards.imports.Messages;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 * @author Madanagopal
 */
public class ImportLineShpWizard extends Wizard implements INewWizardKalypsoImport
{
  private IStructuredSelection initialSelection;

  private IPath m_sourceLocation = null;

  LineShpMainPage mPage;

  // private String m_projectFolder;
  //
  // private IProject m_project;

  IFolder m_scenarioFolder;

  /**
   * Construct a new instance and initialize the dialog settings for this instance.
   */
  public ImportLineShpWizard( )
  {
    super();
  }

  /**
   * @param workbench
   *          the current workbench
   * @param selection
   *          the current object selection
   */
  public void init( IWorkbench workbench, IStructuredSelection selection )
  {
    initialSelection = selection;
    setNeedsProgressMonitor( true );
    setWindowTitle( Messages.getString( "org.kalypso.ui.wizards.imports.lineShp.ImportLineShpWizard.0" ) );
  }

  /**
   * @see org.kalypso.ui.wizards.imports.INewWizardKalypsoImport#initModelProperties(java.util.HashMap)
   */
  public void initModelProperties( HashMap<String, Object> map )
  {
    m_scenarioFolder = (IFolder) map.get( "ScenarioFolder" );
    // m_project = (IProject) map.get( "Project" );
    // m_projectFolder = (String) map.get( "ProjectFolder" );
  }

  @Override
  public void addPages( )
  {
    mPage = new LineShpMainPage();
    addPage( mPage );
    mPage.init( initialSelection );
  }

  /**
   * This method is called by the wizard framework when the user presses the Finish button.
   */
  @Override
  public boolean performFinish( )
  {
    final IFolder dstFilePath = m_scenarioFolder.getProject().getFolder( "imports" );
    final File srcFileShape = new File( mPage.getSourceLocation().toOSString() );
    final IFile dstFileShape = dstFilePath.getFile( mPage.getSourceLocation().lastSegment() );
    File srcFileIndex = null;
    IFile dstFileIndex = null;
    File srcFileDBase = null;
    IFile dstFileDBase = null;
    final String extension = mPage.getSourceLocation().getFileExtension();
    if( extension.equalsIgnoreCase( "shp" ) )
    {
      srcFileIndex = new File( mPage.getSourceLocation().removeFileExtension().addFileExtension( "shx" ).toOSString() );
      dstFileIndex = dstFilePath.getFile( mPage.getSourceLocation().removeFileExtension().addFileExtension( "shx" ).lastSegment() );
      srcFileDBase = new File( mPage.getSourceLocation().removeFileExtension().addFileExtension( "dbf" ).toOSString() );
      dstFileDBase = dstFilePath.getFile( mPage.getSourceLocation().removeFileExtension().addFileExtension( "dbf" ).lastSegment() );
    }
    else
    {
      throw new UnsupportedOperationException( "Unsuported file type: " + extension );
    }
    try
    {
      final File finalSrcIndex = srcFileIndex;
      final IFile finalDstIndex = dstFileIndex;
      final File finalSrcDBase = srcFileDBase;
      final IFile finalDstDBase = dstFileDBase;
      getContainer().run( true, true, new IRunnableWithProgress()
      {
        public void run( final IProgressMonitor monitor )
        {
          try
          {
            if( !dstFilePath.exists() )
            {
              dstFilePath.create( true, true, monitor );
            }
            copy( srcFileShape, dstFileShape, monitor );
            copy( finalSrcIndex, finalDstIndex, monitor );
            copy( finalSrcDBase, finalDstDBase, monitor );
            final IFile file = m_scenarioFolder.getFile( "maps/fenet.gmt" );
            m_scenarioFolder.refreshLocal( IResource.DEPTH_INFINITE, null );
            final Gismapview gismapview = GisTemplateHelper.loadGisMapView( file );
            final Layers layers = gismapview.getLayers();
            final StyledLayerType layer = new StyledLayerType();

            layer.setName( getSourceLocation().removeFileExtension().lastSegment() );
            // layer.setName( "BaseMap" ); //$NON-NLS-1$

            layer.setVisible( true );
            layer.setFeaturePath( "featureMember" ); //$NON-NLS-1$
            layer.setHref( "file:/" + dstFileShape.getLocation().removeFileExtension().toOSString() + "#" + KalypsoGisPlugin.getDefault().getCoordinatesSystem().getName() ); //$NON-NLS-1$ //$NON-NLS-2$
            layer.setType( "simple" ); //$NON-NLS-1$
            layer.setLinktype( "shape" ); //$NON-NLS-1$
            layer.setActuate( "onRequest" ); //$NON-NLS-1$
            layer.setId( "ID_" + (layers.getLayer().size() + 2) ); //$NON-NLS-1$
            layers.getLayer().add( layer );
            gismapview.setLayers( layers );
            // GM_Position max = GisTemplateHelper.getBoundingBox( gismapview ).getMax();
            // GM_Position min = GisTemplateHelper.getBoundingBox( gismapview ).getMin();
            // ExtentType extent = new ExtentType();
            // extent.setLeft( min.getX() );
            // extent.setBottom( min.getY() );
            // extent.setRight( max.getX() );
            // extent.setTop( max.getY() );
            // extent.setSrs( mPage.getCoordinateSystem() );
            // gismapview.setExtent( extent );
            final ByteArrayOutputStream bos = new ByteArrayOutputStream();
            GisTemplateHelper.saveGisMapView( gismapview, bos, file.getCharset() );
            final ByteArrayInputStream bis = new ByteArrayInputStream( bos.toByteArray() );
            bos.close();
            file.setContents( bis, false, true, monitor );
            m_scenarioFolder.refreshLocal( IResource.DEPTH_INFINITE, monitor );
          }
          catch( final CoreException e )
          {
            e.printStackTrace();
          }
          catch( final IOException e )
          {
            e.printStackTrace();
          }
          catch( final JAXBException e )
          {
            e.printStackTrace();
          }
        }
      } );
    }
    catch( InvocationTargetException e )
    {
      e.printStackTrace();
      return false;
    }
    catch( InterruptedException e )
    {
      // User canceled, so stop but donít close wizard.
      e.printStackTrace();
      return false;
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    return true;
  }

  void copy( final File src, final IFile dstFileImage, final IProgressMonitor monitor2 ) throws CoreException, IOException
  {
    InputStream in = null;
    try
    {
      in = new BufferedInputStream( new FileInputStream( src ) );
      if( dstFileImage.exists() )
      {
        dstFileImage.setContents( in, false, true, monitor2 );
      }
      else
      {
        dstFileImage.create( in, false, monitor2 );
      }
    }
    finally
    {
      IOUtils.closeQuietly( in );
    }
  }

  /**
   * Answer the selected source location
   */
  public IPath getSourceLocation( )
  {
    if( m_sourceLocation == null )
      m_sourceLocation = mPage.getSourceLocation();
    return m_sourceLocation;
  }

}
