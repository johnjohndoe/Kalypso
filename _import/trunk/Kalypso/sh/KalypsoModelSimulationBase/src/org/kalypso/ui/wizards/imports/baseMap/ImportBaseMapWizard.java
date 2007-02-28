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
package org.kalypso.ui.wizards.imports.baseMap;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.Gismapview.Layers;
import org.kalypso.template.types.ExtentType;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.ui.wizards.imports.INewWizardKalypsoImport;
import org.kalypso.ui.wizards.imports.Messages;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.xml.sax.InputSource;

/**
 * @author Madanagopal
 * @author Dejan
 */
public class ImportBaseMapWizard extends Wizard implements INewWizardKalypsoImport
{
  private IStructuredSelection initialSelection;

  private BaseMapMainPage mPage;

  private String m_projectFolder;

  ProgressMonitorDialog monitor;

  /**
   * Construct a new instance and initialize the dialog settings for this instance.
   */
  public ImportBaseMapWizard( )
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
    setWindowTitle( Messages.getString( "org.kalypso.ui.wizards.imports.baseMap.BaseMapWizard.0" ) );
  }

  /**
   * @see org.kalypso.ui.wizards.imports.INewWizardKalypsoImport#initModelProperties(java.util.HashMap)
   */
  public void initModelProperties( HashMap<String, Object> map )
  {
    m_projectFolder = (String) map.get( "ProjectFolder" );
  }

  @Override
  public void addPages( )
  {
    mPage = new BaseMapMainPage();
    addPage( mPage );
    mPage.init( initialSelection );
  }

  /**
   * This method is called by the wizard framework when the user presses the Finish button.
   */
  @Override
  public boolean performFinish( )
  {
    String basePath = ResourcesPlugin.getWorkspace().getRoot().getLocation().toOSString() + File.separator + m_projectFolder + File.separator;
    String dstFilePath = basePath + "imports" + File.separator;
    final File srcFileTif = new File( mPage.getSourceLocation().toOSString() );
    final File srcFileTfw = new File( mPage.getSourceLocation().removeFileExtension().addFileExtension( "tfw" ).toOSString() );
    // final File dstFileTif = new File( dstFilePath + mPage.getSourceLocation().lastSegment() );
    // final File dstFileTfw = new File( dstFilePath + mPage.getSourceLocation().removeFileExtension().addFileExtension(
    // "tfw" ).lastSegment() );
    final File dstFileTif = new File( dstFilePath + "basemap.tif" );
    final File dstFileTfw = new File( dstFilePath + "basemap.tfw" );

    try
    {
      getContainer().run( true, true, new IRunnableWithProgress()
      {
        public void run( IProgressMonitor monitor )
        {
          copy( srcFileTif, dstFileTif, monitor );
          copy( srcFileTfw, dstFileTfw, monitor );
        }
      } );
      String mapPath = basePath + "szenario" + File.separator + "maps" + File.separator + "base.gmt";
      try
      {
        InputSource source = new InputSource( new FileInputStream( new File( mapPath ) ) );
        Gismapview gismapview = GisTemplateHelper.loadGisMapView( source );
        StyledLayerType layer = new StyledLayerType();
        layer.setName( "BaseMap" ); //$NON-NLS-1$
        layer.setVisible( true );
        layer.setFeaturePath( "" ); //$NON-NLS-1$
        layer.setHref( "file:/" + dstFilePath + "basemap.tif#EPSG:31467" ); //$NON-NLS-1$ //$NON-NLS-2$
        layer.setType( "simple" ); //$NON-NLS-1$
        layer.setLinktype( "tif" ); //$NON-NLS-1$
        layer.setActuate( "onRequest" ); //$NON-NLS-1$
        layer.setId( "ID_1" ); //$NON-NLS-1$
        Layers layers = new Layers();
        layers.getLayer().add( layer );
        gismapview.setLayers( layers );
        GM_Position max = GisTemplateHelper.getBoundingBox( gismapview ).getMax();
        GM_Position min = GisTemplateHelper.getBoundingBox( gismapview ).getMin();
        ExtentType extent = new ExtentType();
        extent.setLeft( min.getX() );
        extent.setBottom( min.getY() );
        extent.setRight( max.getX() );
        extent.setTop( max.getY() );
        extent.setSrs( "EPSG:31467" );
        gismapview.setExtent( extent );
        GisTemplateHelper.saveGisMapView( gismapview, new FileWriter(new File(mapPath)), "UTF-8" );
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }
    catch( InvocationTargetException e )
    {
      return false;
    }
    catch( InterruptedException e )
    {
      // User canceled, so stop but donít close wizard.
      return false;
    }
    return true;
  }

  boolean copy( File src, File dst, IProgressMonitor monitor2 )
  {
    InputStream in;
    OutputStream out;
    try
    {
      in = new FileInputStream( src );
      out = new FileOutputStream( dst );

      byte[] buf = new byte[1024];
      int len;
      int lens = ((int) src.length() / 1024 + 1);
      monitor2.beginTask( "Copying..", lens );
      while( (len = in.read( buf )) > 0 )
      {
        monitor2.worked( 1 );
        out.write( buf, 0, len );
      }
      monitor2.done();
      in.close();
      out.close();
      return true;
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return false;
    }
  }

  /**
   * Answer the selected source location
   */
  public IPath getSourceLocation( )
  {
    return mPage.getSourceLocation();
  }

}
