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
package org.kalypso.workflow.ui.browser.urlaction;

import java.io.OutputStreamWriter;
import java.net.URL;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.commons.resources.SetContentHelper;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.contribs.javax.xml.namespace.QNameUtilities;
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
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathUtilities;
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
  private final static String PARAM_TARGETPATH = "targetPath";

  // position, where to place new feature as xpath
  // optional, else root feature is parent
  private final static String PARAM_XPATH_PARENT_FEATURE = "xpathParentFeature";

  // required
  private final static String PARAM_NEW_RELATION_QN = "relationType";

  // featuretype of feature to create <br>
  // syntax is <namespace>#<localname> <br>
  // example: createFT=http://kalypso.org#KalypsoFeature
  // optional, else target of relation if featuretype
  private final static String PARAM_NEW_FT_QN = "newFeatureType";

  // propername of feature to create
  // required
  private final static String PARAM_NEW_PROPERTY_QN = "newPropType";

  // property type that can be selected in source gml <br>
  // example: opengis.net#PolygonPropertyType
  // TODO
  private final static String PARAM_SELECTABLE_PropertyType = "selectPropQName";

  /**
   * @see org.kalypso.workflow.ui.browser.IURLAction#run(org.kalypso.workflow.ui.browser.ICommandURL)
   */
  public boolean run( ICommandURL commandURL )
  {
    final String relativeTarget = commandURL.getParameter( PARAM_TARGETPATH );
    final String placementXPath = commandURL.getParameter( PARAM_XPATH_PARENT_FEATURE );
    final String createFTQName = commandURL.getParameter( PARAM_NEW_FT_QN );
    final String relationQNameString = commandURL.getParameter( PARAM_NEW_RELATION_QN );
    final String createFPQNameString = commandURL.getParameter( PARAM_NEW_PROPERTY_QN );

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

    final int open = dialog.open();
    if( open != Window.OK )
      return false;

    final Object newPropertValue = dialog.getSelectedObject();

    // boolean b = GeometryUtilities.isGeometry( value );
    // add Geometry to file

    try
    {
      // load target workspace
      final URL targetURL = wfContext.resolveURL( relativeTarget );
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( targetURL, new UrlResolver() );

      // find parent from feature to create
      final Feature targetParentFE;
      if( placementXPath != null && placementXPath.length() > 0 )
      {
        final GMLXPath xPath = new GMLXPath( placementXPath );
        final Object xpathResult = GMLXPathUtilities.query( xPath, workspace );
        if( !(xpathResult instanceof Feature) )
          return false;
        targetParentFE = (Feature) xpathResult;
      }
      else
        targetParentFE = workspace.getRootFeature();

      // find relation where to place new feature
      final QName relationQName = QNameUtilities.createQName( relationQNameString );
      final IRelationType relationType = (IRelationType) targetParentFE.getFeatureType().getProperty( relationQName );
      
      // find featuretype to create
      final IFeatureType newFT;
      if( createFTQName != null && createFTQName.length() > 0 )
      {
        final QName newFTQName = QNameUtilities.createQName( createFTQName );
        newFT = workspace.getGMLSchema().getFeatureType( newFTQName );
      }
      else
      {
        newFT = relationType.getTargetFeatureType();
      }

      final Feature newFeature = workspace.createFeature( targetParentFE, newFT );
      workspace.addFeatureAsComposition( targetParentFE, relationType, 0, newFeature );
      
      // find relation where to place new feature
      final QName newPropQName = QNameUtilities.createQName( createFPQNameString );
      final IPropertyType newPT = newFT.getProperty( newPropQName );
      // TODO check is pt is a list
      newFeature.setProperty( newPT, newPropertValue );

      final IFile targetResource = ResourceUtilities.findFileFromURL( targetURL );
      final SetContentHelper thread = new SetContentHelper()
      {

        @Override
        protected void write( OutputStreamWriter writer ) throws Throwable
        {
          GmlSerializer.serializeWorkspace( writer, workspace );
        }
      };
      thread.setFileContents( targetResource, false, true, new NullProgressMonitor() );
      targetResource.refreshLocal( IResource.DEPTH_ONE, null );
      return true;
    }
    catch( Exception e )
    {
      e.printStackTrace();
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
