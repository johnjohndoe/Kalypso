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
package org.kalypso.ui.rrm.action;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.convert.namodel.hydrotope.HydrotopeCreationOperation;
import org.kalypso.convert.namodel.schema.binding.GeologyCollection;
import org.kalypso.convert.namodel.schema.binding.LanduseCollection;
import org.kalypso.convert.namodel.schema.binding.SoilTypeCollection;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.handlers.MapHandlerUtils;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Gernot Belger
 * @author Dejan Antanaskovic
 */
public class RRMCreateHydrotopsHandler extends AbstractHandler
{
  /**
   * @see org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event )
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final Shell shell = workbench.getActiveWorkbenchWindow().getShell();
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final IStructuredSelection selection = (IStructuredSelection) context.getVariable( ISources.ACTIVE_CURRENT_SELECTION_NAME );

    final IKalypsoTheme[] themes = MapHandlerUtils.getSelectedThemes( selection );
    if( themes.length != 5 )
    {
      MessageDialog.openError( shell, "Error", "Please select all the necessary data themes." );
// ErrorDialog.openError( shell, "Error", "Please select all the necessary data themes.", Status.CANCEL_STATUS );
      return null;
    }

    String errorMsg = null;
    FeatureList flLanduse = null;
    FeatureList flPedology = null;
    FeatureList flGeology = null;
    FeatureList flCatchment = null;
    FeatureList flHydrotops = null;
    for( final IKalypsoTheme kalypsoTheme : themes )
    {
      final FeatureList list = ((IKalypsoFeatureTheme) kalypsoTheme).getFeatureList();
      if( list.size() == 0 )
      {
        errorMsg = String.format( "Theme '%s' contains no features. Hydrotops cannot be created.", kalypsoTheme.getLabel() );
        break;
      }
      final IRelationType featureTypeProperty = list.getParentFeatureTypeProperty();
      if( featureTypeProperty == null )
      {
        errorMsg = String.format( "Unknown data type found in theme '%s'. Hydrotops cannot be created.", kalypsoTheme.getLabel() );
        break;
      }
      if( LanduseCollection.QNAME_PROP_LANDUSEMEMBER.equals( featureTypeProperty.getQName() ) )
        flLanduse = list;
      else if( SoilTypeCollection.QNAME_PROP_SOILTYPEMEMBER.equals( featureTypeProperty.getQName() ) )
        flPedology = list;
      else if( GeologyCollection.QNAME_PROP_GEOLOGYMEMBER.equals( featureTypeProperty.getQName() ) )
        flGeology = list;
      else if( NaModelConstants.CATCHMENT_MEMBER_PROP.equals( featureTypeProperty.getQName() ) )
        flCatchment = list;
      else if( NaModelConstants.HYDRO_MEMBER.equals( featureTypeProperty.getQName() ) )
        flHydrotops = list;
    }
    if( flLanduse == null || flPedology == null || flGeology == null || flCatchment == null || flHydrotops == null )
    {
      errorMsg = "At least one of necessary data collections is missing. Hydrotops cannot be created.";
    }
    if( errorMsg == null )
    {
      final GMLWorkspace workspace = ((Feature) flHydrotops.get( 0 )).getWorkspace();
      final IFile outputFile = ResourceUtilities.findFileFromURL( workspace.getContext() );
      final IFeatureType typeHydrotop = workspace.getGMLSchema().getFeatureType( NaModelConstants.HYDRO_ELEMENT_FT );
      final FeatureList fflLanduse = flLanduse;
      final FeatureList fflPedology = flPedology;
      final FeatureList fflGeology = flGeology;
      final FeatureList fflCatchment = flCatchment;
      final FeatureList fflHydrotops = flHydrotops;

      if( MessageDialog.openConfirm( shell, "Kalypso", "Create hydrotops?\n\nWarning: existing hydrotops will be deleted!" ) )
      {
        final UIJob job = new UIJob( "Creating hydrotops" )
        {
          @Override
          public IStatus runInUIThread( final IProgressMonitor monitor )
          {
            final HydrotopeCreationOperation op = new HydrotopeCreationOperation( fflLanduse, fflPedology, fflGeology, fflCatchment, fflHydrotops, workspace, typeHydrotop );
            try
            {
              final ProgressMonitorDialog monitorDialog = new ProgressMonitorDialog( shell );
              monitor.beginTask( "Creating hydrotops", 100 );
              monitorDialog.run( true, true, op );
              monitorDialog.setCancelable( false );
              monitor.setTaskName( "Saving hydrotops GML file" );
              final File file = outputFile.getLocation().toFile();
              GmlSerializer.serializeWorkspace( file, workspace, "UTF-8" );
              outputFile.refreshLocal( IResource.DEPTH_ZERO, new NullProgressMonitor() );
            }
            catch( final InterruptedException e1 )
            {
              MessageDialog.openInformation( shell, "Cancel", "Operation cancelled." );
            }
            catch( final Exception e )
            {
              MessageDialog.openError( shell, "Error", e.getLocalizedMessage() );
            }
            return Status.OK_STATUS;
          }
        };
        job.setUser( true );
        job.setPriority( Job.LONG );
        job.schedule( 100 );
      }
    }
    else
    {
      ErrorDialog.openError( shell, "Error", errorMsg, Status.CANCEL_STATUS );
    }
    return null;
  }

}
