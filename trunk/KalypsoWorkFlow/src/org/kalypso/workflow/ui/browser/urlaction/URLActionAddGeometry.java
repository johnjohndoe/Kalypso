/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.workflow.ui.browser.urlaction;

import java.io.OutputStreamWriter;
import java.net.URL;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.commons.resources.SetContentHelper;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.dialog.GmlShapeFileImportDialog;
import org.kalypso.workflow.WorkflowContext;
import org.kalypso.workflow.ui.browser.AbstractURLAction;
import org.kalypso.workflow.ui.browser.ICommandURL;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author kuepfer
 */
public class URLActionAddGeometry extends AbstractURLAction
{

  private final static String COMMAND_NAME = "addGeometry";

  /**
   * optional
   */
  private final static String PARAM_PATH = "path";

  /**
   * @see org.kalypso.workflow.ui.browser.IURLAction#run(org.kalypso.workflow.ui.browser.ICommandURL)
   */
  public boolean run( ICommandURL commandURL )
  {
    final String relativePath = commandURL.getParameter( PARAM_PATH );
    // final String contextString = commandURL.getParameter( ICommandURLActionKeys.KEY_CONTEXT );
    // final String fPath = commandURL.getParameter( ICommandURLActionKeys.KEY_FEATURE_PATH );
    // final String qName = commandURL.getParameter( ICommandURLActionKeys.KEY_QNAME );
    final WorkflowContext wfContext = getWorkFlowContext();

    // Choose file to get geometry from
    final IWorkbench workbench = getWorkbench();
    final IWorkbenchWindow activeWorkbenchWindow = workbench.getActiveWorkbenchWindow();
    final Shell shell = activeWorkbenchWindow.getShell();

    final IProject project = wfContext.getContextProject();
    final GmlShapeFileImportDialog dialog = new GmlShapeFileImportDialog( shell, false, false, true, project, new Class[] { GeometryUtilities.getPolygonClass() } );
    int open = dialog.open();
    if( open == Window.OK )
    {
      final Object value = dialog.getSelectedObject();
      // boolean b = GeometryUtilities.isGeometry( value );
      // add Geometry to file
      try
      {
        final URL fileUrl = wfContext.resolveURL( relativePath );
        final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( fileUrl, new UrlResolver() );
        final Feature rootFeature = workspace.getRootFeature();

        final IFeatureType featureType = workspace.getFeatureType( new QName( "http://schema.kalypso.wb.tu-harburg.de/plangebiet.xsd", "Plangebiet" ) );
        Feature newfeature = workspace.createFeature( rootFeature, featureType );
        IPropertyType property = featureType.getProperty( new QName( "http://schema.kalypso.wb.tu-harburg.de/plangebiet.xsd", "gebiet" ) );
        newfeature.setProperty( property, value );
        IRelationType relationType = GMLSchemaFactory.createRelationType( new QName( "http://schema.kalypso.wb.tu-harburg.de/plangebiet.xsd", "Plangebiete" ), rootFeature.getFeatureType(), 0, IPropertyType.UNBOUND_OCCURENCY );
        workspace.addFeatureAsComposition( rootFeature, relationType, 0, newfeature );
        final IFile file = ResourceUtilities.findFileFromURL( fileUrl );
        final SetContentHelper thread = new SetContentHelper()
        {

          @Override
          protected void write( OutputStreamWriter writer ) throws Throwable
          {
            GmlSerializer.serializeWorkspace( writer, workspace );

          }
        };
        thread.setFileContents( file, false, true, new NullProgressMonitor() );
      }
      catch( Exception e )
      {
        e.printStackTrace();
        return false;
      }
      return true;
    }
    else
    {
      return false;
    }
  }

  /**
   * @see org.kalypso.workflow.ui.browser.IURLAction#getActionName()
   */
  public String getActionName( )
  {
    return COMMAND_NAME;
  }

}
