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

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorRegistry;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.bind.JaxbUtilities;
import org.kalypso.contribs.eclipse.core.resources.StringStorage;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.editorinput.StorageEditorInput;
import org.kalypso.template.featureview.Featuretemplate;
import org.kalypso.template.featureview.ObjectFactory;
import org.kalypso.template.featureview.Featuretemplate.Layer;
import org.kalypso.template.types.LayerTypeUtilities;

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
    return "<Standard Feature Editor>.gft"; //$NON-NLS-1$
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
   * @throws CoreException
   * @see org.kalypso.ui.editorLauncher.IDefaultTemplateLauncher#createInput(org.eclipse.core.resources.IFile)
   */
  public IEditorInput createInput( final IFile file ) throws CoreException
  {
    try
    {
      // ein default template erzeugen
      final ObjectFactory factory = new ObjectFactory();
      final JAXBContext jc = JaxbUtilities.createQuiet( ObjectFactory.class );
      final Layer layer = factory.createFeaturetemplateLayer();
      LayerTypeUtilities.initLayerType( layer, file );

      final Featuretemplate featuretemplate = factory.createFeaturetemplate();
      featuretemplate.setLayer( layer );

      final Marshaller marshaller = JaxbUtilities.createMarshaller( jc);
      marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );

      final StringWriter w = new StringWriter();
      marshaller.marshal( featuretemplate, w );
      w.close();

      final String templateXml = w.toString();

      // als StorageInput zur�ckgeben
      final StorageEditorInput input = new StorageEditorInput( new StringStorage( "<unbenannt>.gft", templateXml, file //$NON-NLS-1$
          .getFullPath() ) );

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