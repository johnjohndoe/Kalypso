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
package org.kalypso.flows;

import java.io.OutputStreamWriter;
import java.net.URL;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.commons.resources.SetContentHelper;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.contribs.javax.xml.namespace.QNameUtilities;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.dialog.GmlShapeFileImportDialog;
import org.kalypso.workflow.WorkflowContext;
import org.kalypso.workflow.ui.browser.AbstractURLAction;
import org.kalypso.workflow.ui.browser.ICommandURL;
import org.kalypso.workflow.ui.browser.IURLActionConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathUtilities;

/**
 * @author kuepfer <br>
 *         TODO merge with URLActionAddGeometry, because they are nearly the same
 */
public class URLActionImportBPlanGML extends AbstractURLAction
{

  private static final String PARAM_SELECTABLE_QNAMES = "selectableQNames";

  /**
   * @see org.kalypso.workflow.ui.browser.IURLAction#run(org.kalypso.workflow.ui.browser.ICommandURL)
   */
  public boolean run( ICommandURL commandURL )
  {
    final String relativeTarget = commandURL.getParameter( IURLActionConstants.PARAM_TARGETPATH );
    final String placementXPath = commandURL.getParameter( IURLActionConstants.PARAM_XPATH_PARENT_FEATURE );
    final String relationQNameString = commandURL.getParameter( IURLActionConstants.PARAM_NEW_RELATION_QN );
    final boolean replace = Boolean.parseBoolean( commandURL.getParameter( IURLActionConstants.PARAM_REPLACE ) );

    final String selectableQNamesStrings = commandURL.getParameter( PARAM_SELECTABLE_QNAMES );
    if( selectableQNamesStrings == null )
      return false;

    final QName[] selectableQNames = QNameUtilities.createQNames( selectableQNamesStrings, IURLActionConstants.PARAM_DEFAULT_SEPARATOR );

    final WorkflowContext wfContext = getWorkFlowContext();

    // Choose file to get geometry from
    final IWorkbench workbench = getWorkbench();
    final IWorkbenchWindow activeWorkbenchWindow = workbench.getActiveWorkbenchWindow();
    final Shell shell = activeWorkbenchWindow.getShell();

    final IProject project = wfContext.getContextProject();

    final GmlShapeFileImportDialog dialog = new GmlShapeFileImportDialog( shell, false, false, true, new String[] { "gml" }, project, selectableQNames );

    final int open = dialog.open();
    if( open != Window.OK )
      return false;

    final Object newPropertValue = dialog.getSelectedObject();

    try
    {
      // load target workspace
      final URL targetURL = wfContext.resolveURL( relativeTarget );
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( targetURL, new UrlResolver(), null );
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
      Feature[] features = workspace.getFeatures( relationType.getTargetFeatureType() );

      // final Feature[] features = object.toFeatures();
      // replace all
      if( replace )
      {
        for( Feature f : features )
          workspace.removeLinkedAsCompositionFeature( targetParentFE, relationType, f );
        workspace.addFeatureAsComposition( targetParentFE, relationType, 0, (Feature) newPropertValue );
      }
      else
        workspace.addFeatureAsComposition( targetParentFE, relationType, features.length + 1, (Feature) newPropertValue );

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
      targetResource.refreshLocal( IResource.DEPTH_INFINITE, null );
      return true;
    }
    catch( Exception e )
    {
      e.printStackTrace();
      generateMessageDialog( "Error while importing landuse plan, message: " + e.getMessage(), IStatus.ERROR );
      return false;
    }
  }

}
