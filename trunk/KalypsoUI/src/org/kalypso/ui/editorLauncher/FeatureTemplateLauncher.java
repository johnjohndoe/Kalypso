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

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorRegistry;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.kalypso.eclipse.core.resources.StringStorage;
import org.kalypso.eclipse.ui.editorinput.StorageEditorInput;
import org.kalypso.template.featureview.Featuretemplate;
import org.kalypso.template.featureview.ObjectFactory;
import org.kalypso.template.featureview.FeaturetemplateType.LayerType;

/**
 * @author belger
 */
public class FeatureTemplateLauncher implements IDefaultTemplateLauncher
{
  /**
   * @see org.kalypso.ui.editorLauncher.IDefaultTemplateLauncher#getFilename()
   */
  public String getFilename()
  {
    return "<Standard Feature Editor>.gft";
  }

  /**
   * @see org.kalypso.ui.editorLauncher.IDefaultTemplateLauncher#getEditor()
   */
  public IEditorDescriptor getEditor()
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IEditorRegistry editorRegistry = workbench.getEditorRegistry();

    return editorRegistry.getDefaultEditor( getFilename() );
  }

  /**
   * @see org.kalypso.ui.editorLauncher.IDefaultTemplateLauncher#createInput(org.eclipse.core.resources.IFile)
   */
  public IEditorInput createInput( final IFile file )
  {
    try
    {
      final IPath projectRelativePath = file.getProjectRelativePath();
      
      // ein default template erzeugen
      final ObjectFactory factory = new ObjectFactory();

      final LayerType layer = factory.createFeaturetemplateTypeLayerType();
      layer.setId( file.toString() );
      layer.setHref( "project:/" + projectRelativePath );
      layer.setLinktype( "gml" );
      layer.setFeaturePath( "" ); // immer das root-feature

      final Featuretemplate featuretemplate = factory.createFeaturetemplate();
      featuretemplate.setLayer( layer );

      final Marshaller marshaller = factory.createMarshaller();
      marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );

      final StringWriter w = new StringWriter();
      marshaller.marshal( featuretemplate, w );
      w.close();

      final String string = w.toString();

      // als StorageInput zurückgeben
      final StorageEditorInput input = new StorageEditorInput( new StringStorage( string, file.getFullPath() ) );

      return input;
    }
    catch( JAXBException e )
    {
      e.printStackTrace();
      
      return null;
    }
    catch( IOException e )
    {
      e.printStackTrace();
      
      return null;
    }
  }

}