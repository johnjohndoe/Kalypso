/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.editorLauncher;

import java.io.IOException;
import java.io.StringWriter;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Validator;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorRegistry;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.kalypso.contribs.eclipse.core.resources.StringStorage;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.editorinput.StorageEditorInput;
import org.kalypso.jwsdp.JaxbUtilities;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.Gismapview.Layers;
import org.kalypso.template.types.ExtentType;
import org.kalypso.template.types.LayerTypeUtilities;
import org.kalypso.template.types.ObjectFactory;
import org.kalypso.template.types.StyledLayerType;

/**
 * Launcher, um ein GML im Baum (GmlEditor) anzusehen.
 * 
 * @author belger
 */
public class GisMapEditorTemplateLauncher implements IDefaultTemplateLauncher
{
  /**
   * @see org.kalypso.ui.editorLauncher.IDefaultTemplateLauncher#getFilename()
   */
  public String getFilename( )
  {
    return "<Standard Kartenansicht>.gmt";
  }

  /**
   * @see org.kalypso.ui.editorLauncher.IDefaultTemplateLauncher#getEditor()
   */
  public IEditorDescriptor getEditor( )
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IEditorRegistry editorRegistry = workbench.getEditorRegistry();
    return editorRegistry.findEditor( "org.kalypso.ui.editor.mapeditor.GisMapEditor" );
  }

  /**
   * @see org.kalypso.ui.editorLauncher.IDefaultTemplateLauncher#createInput(org.eclipse.core.resources.IFile)
   */
  public IEditorInput createInput( final IFile file ) throws CoreException
  {
    final org.kalypso.template.gismapview.ObjectFactory gisMapFactory = new org.kalypso.template.gismapview.ObjectFactory();
    final JAXBContext jc = JaxbUtilities.createQuiet( org.kalypso.template.gismapview.ObjectFactory.class );

    try
    {
      if( "gml".equalsIgnoreCase( file.getProjectRelativePath().getFileExtension() ) )
        throw new CoreException( StatusUtilities.createWarningStatus( "GML Dateien können nicht über die Standardkartenvorlage angezeigt werden.\nVersuchen Sie, eine leere Karte zu erzeugen und die Datei über 'Thema hinzufügen' zu laden." ) );

      final StyledLayerType layer = new ObjectFactory().createStyledLayerType();
      LayerTypeUtilities.initLayerType( layer, file );
      layer.setVisible( true );
      layer.setName( file.getName() );
      layer.setFeaturePath( "featureMember" );

      final Layers layers = gisMapFactory.createGismapviewLayers();
      layers.getLayer().add( layer );
      layers.setActive( layer );

      final ExtentType extent = new ObjectFactory().createExtentType();
      extent.setRight( 0.0 );
      extent.setLeft( 0.0 );
      extent.setBottom( 0.0 );
      extent.setTop( 0.0 );

      final Gismapview gismapview = gisMapFactory.createGismapview();
      gismapview.setLayers( layers );
      gismapview.setExtent( extent );

      final Validator validator = jc.createValidator();
      validator.validate( gismapview );

      // final Marshaller marshaller = jc.createMarshaller();
      final Marshaller marshaller = JaxbUtilities.createMarshaller( jc );
      marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );

      final StringWriter w = new StringWriter();
      marshaller.marshal( gismapview, w );
      w.close();

      final String string = w.toString();

      // als StorageInput zurückgeben
      final StorageEditorInput input = new StorageEditorInput( new StringStorage( "<unbenannt>.gmt", string, file.getFullPath() ) );

      return input;
    }
    catch( final JAXBException e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }
    catch( final IOException e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }
  }
}
